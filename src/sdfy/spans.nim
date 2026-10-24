## Direct rendering into borrowed premultiplied RGBA spans.
##
## Coordinates map pixel centers into local shape/paint space. Masks are either
## alpha bytes or premultiplied RGBA pixels (only alpha is read). No input is
## retained and no intermediate image is allocated. Mask and destination lengths
## must match when a mask is present; shifted overlapping views are unsupported.

import std/math
import chroma, pixie, vmath
import ./shapes
import ./simd/shapesSimd

const spanSimdEnabled* =
  allowSimd and (defined(amd64) or defined(arm64) or defined(aarch64) or defined(arm))

# high(float32) is infinity in Nim; use the largest finite IEEE float instead.
const minimumPaintInterval = 1'f32 / 3.4028234663852886e38'f32

when spanSimdEnabled:
  import ./simd/spanVectors

type
  SpanBlend* = enum
    sbOver ## Premultiplied source-over after applying coverage and mask.
    sbReplace ## Interpolate source and destination using mask (including alpha).

  SpanMode* = enum
    smFill
    smStroke
    smStrokeAA
    smDropShadow
    smInsetShadow

  PaintKind = enum
    pkSolid
    pkLinear
    pkLinear3
    pkBilinear

  SpanPaint* = object
    kind: PaintKind
    colors: array[4, ColorRGBA]
    origin, axis, scale: Vec2
    midpoint: float32

  RoundedBoxField* = object
    center, halfSize: Vec2
    rx, ry: Vec4
    circular: bool

  SpanCoordinates* = object
    origin, step: Vec2
    transform: Mat3

  SpanClip* = object
    enabled: bool
    low, high: Vec2

  SpanEffect* = object
    mode: SpanMode
    aaWidth, strokeWidth, blurRadius, spread: float32
    insetOffset: Vec2

proc finite(x: float32): bool {.inline.} =
  classify(x) notin {fcNan, fcInf, fcNegInf}

proc requireFinite(v: Vec2) =
  if not finite(v.x) or not finite(v.y):
    raise newException(ValueError, "Span coordinates must be finite")

proc initSolidPaint*(color: ColorRGBA): SpanPaint =
  SpanPaint(kind: pkSolid, colors: [color, color, color, color])

proc initLinearPaint*(start, stop: ColorRGBA, origin, axis: Vec2): SpanPaint =
  ## `origin` is the start point; `axis` is the vector from start to stop.
  ## Straight RGBA channels interpolate in byte space and clamp at endpoints.
  requireFinite(origin)
  requireFinite(axis)
  let magnitude = dot(axis, axis)
  if magnitude <= 0 or not finite(magnitude):
    raise newException(ValueError, "Gradient axis must have finite positive length")
  result = SpanPaint(
    kind: pkLinear,
    colors: [start, stop, stop, stop],
    origin: origin,
    axis: axis / magnitude,
  )
  requireFinite(result.axis)

proc initLinearPaint*(
    start, middle, stop: ColorRGBA, origin, axis: Vec2, midpoint = 0.5'f32
): SpanPaint =
  ## Three-stop gradient. `midpoint` must be strictly between zero and one,
  ## with finite reciprocals for both stop intervals.
  if not finite(midpoint) or midpoint <= 0 or midpoint >= 1:
    raise newException(ValueError, "Gradient midpoint must be between zero and one")
  if midpoint <= minimumPaintInterval or (1'f32 - midpoint) <= minimumPaintInterval:
    raise newException(ValueError, "Gradient stop intervals are too small")
  result = initLinearPaint(start, stop, origin, axis)
  result.kind = pkLinear3
  result.colors = [start, middle, stop, stop]
  result.midpoint = midpoint

proc initBilinearPaint*(colors: array[4, ColorRGBA], origin, size: Vec2): SpanPaint =
  ## Corner order: top-left, top-right, bottom-right, bottom-left.
  ## Each horizontal interpolation is rounded before vertical interpolation.
  requireFinite(origin)
  requireFinite(size)
  if size.x <= 0 or size.y <= 0:
    raise newException(ValueError, "Bilinear paint size must be positive")
  if size.x <= minimumPaintInterval or size.y <= minimumPaintInterval:
    raise newException(ValueError, "Bilinear paint size is too small")
  let inverseSize = vec2(1'f32 / size.x, 1'f32 / size.y)
  requireFinite(inverseSize)
  SpanPaint(kind: pkBilinear, colors: colors, origin: origin, scale: inverseSize)

proc initRoundedBoxField*(
    halfSize: Vec2, rx, ry: Vec4, center = vec2(0)
): RoundedBoxField =
  ## Normalize sdfy quadrant radii once. Equal axes use the circular kernel.
  requireFinite(halfSize)
  requireFinite(center)
  result =
    RoundedBoxField(center: center, halfSize: max(halfSize, vec2(0)), circular: true)
  for i in 0 ..< 4:
    if not finite(rx[i]) or not finite(ry[i]):
      raise newException(ValueError, "Corner radii must be finite")
    var x, y: float32
    if rx[i] == ry[i]:
      x = clamp(rx[i], 0'f32, min(result.halfSize.x, result.halfSize.y))
      y = x
    else:
      x = clamp(rx[i], 0'f32, result.halfSize.x)
      y = clamp(ry[i], 0'f32, result.halfSize.y)
      if x <= 0 or y <= 0:
        x = 0
        y = 0
    result.rx[i] = x
    result.ry[i] = y
    result.circular = result.circular and x == y

proc initRoundedBoxField*(
    halfSize: Vec2, radii: Vec4, center = vec2(0)
): RoundedBoxField =
  initRoundedBoxField(halfSize, radii, radii, center)

proc initSpanCoordinates*(
    origin: Vec2, step = vec2(1, 0), transform = mat3()
): SpanCoordinates =
  ## Pixel centers are `transform * (origin + index * step)`. The transform
  ## must be finite and affine. `step` may be zero (a constant sample).
  requireFinite(origin)
  requireFinite(step)
  for col in 0 ..< 3:
    for row in 0 ..< 3:
      if not finite(transform[col, row]):
        raise newException(ValueError, "Span transform must be finite")
  if transform[0, 2] != 0 or transform[1, 2] != 0 or
      abs(transform[2, 2] - 1'f32) > 0.000001'f32:
    raise newException(ValueError, "Span transform must be affine")
  result = SpanCoordinates(origin: origin, step: step, transform: transform)
  # Matrix inversion can round an affine homogeneous component away from one.
  result.transform[2, 2] = 1

proc initSpanClip*(low, high: Vec2): SpanClip =
  ## Local bounds include `low` and exclude `high`. Default SpanClip disables
  ## clipping. Equal bounds form an empty clip.
  requireFinite(low)
  requireFinite(high)
  if low.x > high.x or low.y > high.y:
    raise newException(ValueError, "Span clip bounds are reversed")
  SpanClip(enabled: true, low: low, high: high)

proc initSpanEffect*(
    mode = smFill,
    aaWidth = 1'f32,
    strokeWidth = 1'f32,
    blurRadius = 1'f32,
    spread = 0'f32,
    insetOffset = vec2(0),
): SpanEffect =
  ## Coverage is clamp(0.5 - distance/max(aaWidth, 1e-5), 0, 1). Strokes lie inside the
  ## shape boundary. Shadows use sigma=max(blurRadius/2, 0.5), preserving paint
  ## alpha; inset offsets are expressed in local coordinates.
  if not finite(aaWidth) or aaWidth <= 0 or not finite(strokeWidth) or strokeWidth < 0 or
      not finite(blurRadius) or blurRadius < 0 or not finite(spread):
    raise newException(ValueError, "Span effect widths must be finite and valid")
  requireFinite(insetOffset)
  SpanEffect(
    mode: mode,
    aaWidth: max(aaWidth, 0.00001'f32),
    strokeWidth: strokeWidth,
    blurRadius: blurRadius,
    spread: spread,
    insetOffset: insetOffset,
  )

proc distance*(field: RoundedBoxField, p: Vec2): float32 {.inline.} =
  let local = p - field.center
  if field.circular:
    sdRoundedBox(local, field.halfSize, field.rx)
  else:
    sdEllipticalRoundedBox(local, field.halfSize, field.rx, field.ry)

proc lerp(a, b: ColorRGBA, t: float32): ColorRGBA =
  let t = clamp(t, 0'f32, 1'f32)
  rgba(
    round(a.r.float32 * (1 - t) + b.r.float32 * t).uint8,
    round(a.g.float32 * (1 - t) + b.g.float32 * t).uint8,
    round(a.b.float32 * (1 - t) + b.b.float32 * t).uint8,
    round(a.a.float32 * (1 - t) + b.a.float32 * t).uint8,
  )

proc sample(paint: SpanPaint, p: Vec2): ColorRGBA =
  case paint.kind
  of pkSolid:
    paint.colors[0]
  of pkLinear:
    lerp(paint.colors[0], paint.colors[1], dot(p - paint.origin, paint.axis))
  of pkLinear3:
    let t = clamp(dot(p - paint.origin, paint.axis), 0'f32, 1'f32)
    if t <= paint.midpoint:
      lerp(paint.colors[0], paint.colors[1], t / paint.midpoint)
    else:
      lerp(
        paint.colors[1], paint.colors[2], (t - paint.midpoint) / (1 - paint.midpoint)
      )
  of pkBilinear:
    let uv = (p - paint.origin) * paint.scale
    lerp(
      lerp(paint.colors[0], paint.colors[1], uv.x),
      lerp(paint.colors[3], paint.colors[2], uv.x),
      uv.y,
    )

proc coverage(field: RoundedBoxField, p: Vec2, effect: SpanEffect): float32 =
  let d = field.distance(p)
  result = clamp(0.5'f32 - d / effect.aaWidth, 0'f32, 1'f32)
  case effect.mode
  of smFill:
    discard
  of smStroke, smStrokeAA:
    let half = effect.strokeWidth * 0.5'f32
    let band = abs(d + half) - half
    result =
      if effect.mode == smStroke:
        float32(band < 0)
      else:
        clamp(0.5'f32 - band / effect.aaWidth, 0'f32, 1'f32)
  of smDropShadow, smInsetShadow:
    let sigma = max(effect.blurRadius * 0.5'f32, 0.5'f32)
    if effect.mode == smDropShadow:
      let outside = max(0'f32, d - effect.spread)
      result = exp(-0.5'f32 * outside * outside / (sigma * sigma))
    else:
      let inside = min(0'f32, field.distance(p - effect.insetOffset) + effect.spread)
      result *= exp(-0.5'f32 * inside * inside / (sigma * sigma))

proc composite(src, dst: ColorRGBX, mask: uint32, blend: SpanBlend): ColorRGBX =
  if blend == sbReplace:
    let inv = 255'u32 - mask
    result = rgbx(
      ((src.r.uint32 * mask + dst.r.uint32 * inv + 127) div 255).uint8,
      ((src.g.uint32 * mask + dst.g.uint32 * inv + 127) div 255).uint8,
      ((src.b.uint32 * mask + dst.b.uint32 * inv + 127) div 255).uint8,
      ((src.a.uint32 * mask + dst.a.uint32 * inv + 127) div 255).uint8,
    )
  else:
    let r = (src.r.uint32 * mask + 127) div 255
    let g = (src.g.uint32 * mask + 127) div 255
    let b = (src.b.uint32 * mask + 127) div 255
    let a = (src.a.uint32 * mask + 127) div 255
    let inv = 255'u32 - a
    result = rgbx(
      (r + (dst.r.uint32 * inv + 127) div 255).uint8,
      (g + (dst.g.uint32 * inv + 127) div 255).uint8,
      (b + (dst.b.uint32 * inv + 127) div 255).uint8,
      (a + (dst.a.uint32 * inv + 127) div 255).uint8,
    )

when spanSimdEnabled:
  type Channels = array[4, Float4]

  proc distance4(field: RoundedBoxField, px, py: Float4): Float4 {.inline.} =
    let x = px - splat(field.center.x)
    let y = py - splat(field.center.y)
    if field.circular:
      sdRoundedBoxSimd(x, y, field.halfSize.x, field.halfSize.y, field.rx)
    else:
      sdEllipticalRoundedBoxSimd(
        x, y, field.halfSize.x, field.halfSize.y, field.rx, field.ry
      )

  proc channels(color: ColorRGBA): Channels {.inline.} =
    [
      splat(color.r.float32),
      splat(color.g.float32),
      splat(color.b.float32),
      splat(color.a.float32),
    ]

  proc clamp01(value: Float4): Float4 {.inline.} =
    min4(splat(1), max4(splat(0), value))

  proc mixChannels(a, b: Channels, t: Float4): Channels {.inline.} =
    let t = clamp01(t)
    for i in 0 ..< 4:
      result[i] = rounded(a[i] * (splat(1) - t) + b[i] * t)

  proc paint4(paint: SpanPaint, px, py: Float4): Channels {.inline.} =
    case paint.kind
    of pkSolid:
      result = channels(paint.colors[0])
    of pkLinear, pkLinear3:
      let t =
        (px - splat(paint.origin.x)) * splat(paint.axis.x) +
        (py - splat(paint.origin.y)) * splat(paint.axis.y)
      if paint.kind == pkLinear:
        result = mixChannels(channels(paint.colors[0]), channels(paint.colors[1]), t)
      else:
        let clamped = clamp01(t)
        let first = mixChannels(
          channels(paint.colors[0]),
          channels(paint.colors[1]),
          clamped * splat(1'f32 / paint.midpoint),
        )
        let second = mixChannels(
          channels(paint.colors[1]),
          channels(paint.colors[2]),
          (clamped - splat(paint.midpoint)) * splat(1'f32 / (1 - paint.midpoint)),
        )
        let mask = le4(clamped, splat(paint.midpoint))
        for i in 0 ..< 4:
          result[i] = choose(mask, first[i], second[i])
    of pkBilinear:
      let u = (px - splat(paint.origin.x)) * splat(paint.scale.x)
      let v = (py - splat(paint.origin.y)) * splat(paint.scale.y)
      result = mixChannels(
        mixChannels(channels(paint.colors[0]), channels(paint.colors[1]), u),
        mixChannels(channels(paint.colors[3]), channels(paint.colors[2]), u),
        v,
      )

  proc coverage4(
      field: RoundedBoxField, px, py: Float4, effect: SpanEffect
  ): Float4 {.inline.} =
    let d = field.distance4(px, py)
    let invAa = splat(1'f32 / effect.aaWidth)
    result = clamp01(splat(0.5) - d * invAa)
    case effect.mode
    of smFill:
      discard
    of smStroke, smStrokeAA:
      let half = splat(effect.strokeWidth * 0.5'f32)
      let q = d + half
      let band = max4(q, splat(0) - q) - half
      result =
        if effect.mode == smStroke:
          choose(lt4(band, splat(0)), splat(1), splat(0))
        else:
          clamp01(splat(0.5) - band * invAa)
    of smDropShadow, smInsetShadow:
      let sigma = max(effect.blurRadius * 0.5'f32, 0.5'f32)
      let invSigma = splat(-0.5'f32 / (sigma * sigma))
      if effect.mode == smDropShadow:
        let outside = max4(splat(0), d - splat(effect.spread))
        result = expNegative(outside * outside * invSigma)
      else:
        let inner = field.distance4(
          px - splat(effect.insetOffset.x), py - splat(effect.insetOffset.y)
        )
        let inside = min4(splat(0), inner + splat(effect.spread))
        result = result * expNegative(inside * inside * invSigma)

  proc composite4(
      source: Channels, coverage: Float4, mask, destination: Word4, blend: SpanBlend
  ): Word4 {.inline.} =
    var src: array[4, Word4]
    src[3] = toWords(rounded(source[3] * coverage))
    for channel in 0 ..< 3:
      src[channel] = div255(mulWords(toWords(source[channel]), src[3]))
    var outChannels: array[4, Word4]
    let dstChannels = [
      andWords(destination, words(255)),
      andWords(shrWords(destination, 8), words(255)),
      andWords(shrWords(destination, 16), words(255)),
      shrWords(destination, 24),
    ]
    if blend == sbReplace:
      let inverse = subWords(words(255), mask)
      for channel in 0 ..< 4:
        let dst = dstChannels[channel]
        outChannels[channel] =
          div255(addWords(mulWords(src[channel], mask), mulWords(dst, inverse)))
    else:
      for channel in 0 ..< 4:
        src[channel] = div255(mulWords(src[channel], mask))
      let inverse = subWords(words(255), src[3])
      for channel in 0 ..< 4:
        let dst = dstChannels[channel]
        outChannels[channel] = addWords(src[channel], div255(mulWords(dst, inverse)))
    result = orWords(
      orWords(outChannels[0], shlWords(outChannels[1], 8)),
      orWords(shlWords(outChannels[2], 16), shlWords(outChannels[3], 24)),
    )

proc distances*(
    field: RoundedBoxField, points: array[4, Vec2], useSimd = true
): array[4, float32] =
  ## Distance-only packet API for custom paint or image sampling pipelines.
  when spanSimdEnabled:
    if useSimd:
      var xs, ys: array[4, float32]
      for lane in 0 ..< 4:
        xs[lane] = points[lane].x
        ys[lane] = points[lane].y
      storeFloats(
        result[0].addr, field.distance4(loadFloats(xs[0].addr), loadFloats(ys[0].addr))
      )
      return
  for lane in 0 ..< 4:
    result[lane] = field.distance(points[lane])

proc drawSpan[M: uint8 | ColorRGBX](
    destination: var openArray[ColorRGBX],
    field: RoundedBoxField,
    coordinates: SpanCoordinates,
    paint: SpanPaint,
    mask: openArray[M],
    effect: SpanEffect,
    clip: SpanClip,
    blend: SpanBlend,
    useSimd: bool,
) =
  if mask.len != 0 and mask.len != destination.len:
    raise newException(ValueError, "Span mask length must equal destination length")
  # Default-initialized coordinates/effects are invalid; reject them before writes.
  if coordinates.transform[2, 2] != 1 or effect.aaWidth <= 0:
    raise
      newException(ValueError, "Initialize span coordinates and effect before drawing")
  var index = 0
  when spanSimdEnabled:
    if useSimd:
      let mat = coordinates.transform
      while index + 4 <= destination.len:
        let indices = splat(index.float32) + lanes()
        let x = splat(coordinates.origin.x) + indices * splat(coordinates.step.x)
        let y = splat(coordinates.origin.y) + indices * splat(coordinates.step.y)
        let px = splat(mat[0, 0]) * x + splat(mat[1, 0]) * y + splat(mat[2, 0])
        let py = splat(mat[0, 1]) * x + splat(mat[1, 1]) * y + splat(mat[2, 1])
        var alpha = words(255)
        if mask.len != 0:
          when M is ColorRGBX:
            alpha = shrWords(loadWords(mask[index].unsafeAddr), 24)
          else:
            alpha = maskBytes(cast[ptr UncheckedArray[uint8]](mask[index].unsafeAddr))
        if clip.enabled:
          let inside = and4(
            and4(le4(splat(clip.low.x), px), lt4(px, splat(clip.high.x))),
            and4(le4(splat(clip.low.y), py), lt4(py, splat(clip.high.y))),
          )
          alpha = chooseWords(inside, alpha, words(0))
        let colors = paint4(paint, px, py)
        let coverage = field.coverage4(px, py, effect)
        let dst = loadWords(destination[index].addr)
        storeWords(
          destination[index].addr, composite4(colors, coverage, alpha, dst, blend)
        )
        index += 4
  while index < destination.len:
    let p =
      coordinates.transform * (coordinates.origin + coordinates.step * index.float32)
    if not clip.enabled or (
      p.x >= clip.low.x and p.y >= clip.low.y and p.x < clip.high.x and p.y < clip.high.y
    ):
      var color = paint.sample(p)
      color.a = round(color.a.float32 * field.coverage(p, effect)).uint8
      var alpha = 255'u32
      if mask.len != 0:
        when M is ColorRGBX:
          alpha = mask[index].a.uint32
        else:
          alpha = mask[index].uint32
      destination[index] = composite(color.rgbx(), destination[index], alpha, blend)
    inc index

proc drawRoundedBoxSpan*(
    destination: var openArray[ColorRGBX],
    field: RoundedBoxField,
    coordinates: SpanCoordinates,
    paint: SpanPaint,
    effect = initSpanEffect(),
    clip = SpanClip(),
    blend = sbOver,
    mask: openArray[ColorRGBX] = [],
    useSimd = true,
) =
  ## Render into a caller-owned row/span. A supplied RGBA mask contributes alpha
  ## only. Complete packets use SSE2/NEON; remaining pixels use the scalar path.
  ## `useSimd=false` forces scalar evaluation for validation or benchmarking.
  drawSpan(destination, field, coordinates, paint, mask, effect, clip, blend, useSimd)

proc drawRoundedBoxSpan*(
    destination: var openArray[ColorRGBX],
    field: RoundedBoxField,
    coordinates: SpanCoordinates,
    paint: SpanPaint,
    mask: openArray[uint8],
    effect = initSpanEffect(),
    clip = SpanClip(),
    blend = sbOver,
    useSimd = true,
) =
  ## Byte-mask overload. An empty mask means fully opaque coverage.
  drawSpan(destination, field, coordinates, paint, mask, effect, clip, blend, useSimd)

proc drawRoundedBox*(
    image: Image,
    field: RoundedBoxField,
    paint: SpanPaint,
    transform = mat3(),
    effect = initSpanEffect(),
    clip = SpanClip(),
    blend = sbOver,
    mask: Image = nil,
    useSimd = true,
) =
  ## Image convenience wrapper for the span renderer. `transform` maps local
  ## coordinates into the image; pixel centers are sampled at (x+0.5, y+0.5).
  ## Without a local clip every image pixel is considered (including shadows).
  ## Images and masks are borrowed synchronously, and a mask must match the image.
  if image.isNil:
    raise newException(ValueError, "Cannot render into a nil image")
  if not mask.isNil and (mask.width != image.width or mask.height != image.height):
    raise newException(ValueError, "Image mask dimensions must match destination")
  discard initSpanCoordinates(vec2(0), transform = transform)
  let determinant =
    transform[0, 0] * transform[1, 1] - transform[0, 1] * transform[1, 0]
  if abs(determinant) < 0.000001'f32:
    raise newException(ValueError, "Image transform must be invertible")
  let inverse = transform.inverse()
  for y in 0 ..< image.height:
    let rowStart = y * image.width
    let rowEnd = rowStart + image.width - 1
    let coordinates =
      initSpanCoordinates(vec2(0.5, y.float32 + 0.5'f32), transform = inverse)
    if mask.isNil:
      drawRoundedBoxSpan(
        image.data.toOpenArray(rowStart, rowEnd),
        field,
        coordinates,
        paint,
        effect,
        clip,
        blend,
        useSimd = useSimd,
      )
    else:
      drawRoundedBoxSpan(
        image.data.toOpenArray(rowStart, rowEnd),
        field,
        coordinates,
        paint,
        effect,
        clip,
        blend,
        mask.data.toOpenArray(rowStart, rowEnd),
        useSimd,
      )
