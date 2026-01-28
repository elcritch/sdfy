import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes
import ./shapes

import ./simd/shapesSimd

proc gaussian(x: float32, s: float32): float32 {.inline.} =
  result = 1 / (s * sqrt(2 * PI)) * exp(-1 * x^2 / (2 * s^2))

proc log2Normal(x: float32, s: float32): float32 {.inline.} =
  result = 1 / (x * s * sqrt(2 * PI)) * exp(-1 * log2(x)^2 / (2 * s^2))

proc drawSdfShapeImpl*[I, T](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: T,
    pos: ColorRGBA,
    neg: ColorRGBA,
    mode: SDFMode,
    factor: float32,
    spread: float32,
    pointOffset: Vec2, ## offset the point by this amount, corrects pixelation at edges
    aaFactor: float32, ## controls how harsh the AA is applied, higher values result in a sharper transition
    stdDevFactor: float32, ## controls the standard deviation of the Gaussian blur, higher values result in a softer transition
) {.raises: [].} =
  ## Generic signed distance function for shapes
  ## Supports rounded boxes, chamfered boxes, circles, Bézier curves, boxes, ellipses, arcs, parallelograms, pies, and rings based on params type
  ## T: RoundedBoxParams, ChamferBoxParams, CircleParams, BezierParams, BoxParams, EllipseParams, ArcParams, ParallelogramParams, PieParams, or RingParams
  mixin dataIndex

  let posC = pos.to(Color)
  let negC = neg.to(Color)

  var factor = factor
  if mode in [sdfModeAnnular, sdfModeAnnularAA, sdfModeAnnularRgbSubPixelAA, sdfModeAnnularBgrSubPixelAA]:
    factor = factor * 0.5

  for y in 0 ..< image.height:
    for x in 0 ..< image.width:
      let p = vec2(x.float32, y.float32) - center + pointOffset

      # Select the appropriate SDF function based on parameter type
      var sd =
        when T is RoundedBoxParams:
          let b = wh / 2.0
          sdRoundedBox(p, b, params.r)
        elif T is ChamferBoxParams:
          let b = wh / 2.0
          sdChamferBox(p, b, params.chamfer)
        elif T is CircleParams:
          sdCircle(p, params.r)
        elif T is BezierParams:
          sdBezier(p, params.A, params.B, params.C)
        elif T is BoxParams:
          sdBox(p, params.b)
        elif T is EllipseParams:
          sdEllipse(p, params.ab)
        elif T is ArcParams:
          sdArc(p, params.sc, params.ra, params.rb)
        elif T is ParallelogramParams:
          sdParallelogram(p, params.wi, params.he, params.sk)
        elif T is PieParams:
          sdPie(p, params.c, params.r)
        elif T is RingParams:
          sdRing(p, params.n, params.r, params.th)
        elif T is MsdfBitmapParams:
          sdMsdfBitmap(p, wh, params)
        else:
          {.error: "Unsupported shape parameter type".}

      # sd -= sdOffset
      var c: ColorRGBA = if sd < 0.0: pos else: neg
      case mode:
      of sdfModeClip:
        discard
      of sdfModeClipAA:
        # we offset by 0.5 to make the edges blur
        # the clamping makes the transition go by ~1 pixel
        # then we mix the pos and neg colors based on the clamped value
        let cl = clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)

      of sdfModeClipRgbSubPixelAA:
        let cl = vec4(
          clamp(aaFactor * sd + 0.25, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0),
          clamp(aaFactor * sd + 0.75, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        )
        c = mix(posC, negC, cl).to(ColorRGBA)
      of sdfModeClipBgrSubPixelAA:
        let cl = vec4(
          clamp(aaFactor * sd + 0.75, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0),
          clamp(aaFactor * sd + 0.25, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        )
        c = mix(posC, negC, cl).to(ColorRGBA)
      of sdfModeAnnular:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
      of sdfModeAnnularAA:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
        let cl = clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
      of sdfModeAnnularRgbSubPixelAA:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
        let cl = vec4(
          clamp(aaFactor * sd + 0.25, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0),
          clamp(aaFactor * sd + 0.75, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        )
        c = mix(posC, negC, cl).to(ColorRGBA)
      of sdfModeAnnularBgrSubPixelAA:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
        let cl = vec4(
          clamp(aaFactor * sd + 0.75, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0),
          clamp(aaFactor * sd + 0.25, 0.0, 1.0),
          clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        )
        c = mix(posC, negC, cl).to(ColorRGBA)
      of sdfModeFeather:
        c.a = uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherInv:
        c.a = 255 - uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherGaussian:
        let s = stdDevFactor
        let x = sd / factor
        let f = 255 * 1.1 * gaussian(x, s)
        c.a = uint8(f * 255)
      of sdfModeDropShadow:
        let s = stdDevFactor
        let sd = sd - spread + 1
        let x = sd / (factor + 0.5)
        let f = 255 * 1.1 * gaussian(x, s)
        c.a = if sd > 0.0: uint8(min(f, 255)) else: c.a
      of sdfModeDropShadowAA:
        let s = stdDevFactor
        let cl = clamp(aaFactor * sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
        let x = sd / factor
        let f = 255 * 1.1 * gaussian(x, s)
        c.a = if sd >= 0.0: uint8(min(f, 255)) else: c.a
      of sdfModeInsetShadow:
        let s = stdDevFactor
        let sd = sd + spread + 1
        let x = sd / (factor + 0.5)
        let f = 255 * 1.1 * gaussian(x, s)
        c.a = if sd < 0.0: uint8(min(f, 255)) else: c.a
      of sdfModeInsetShadowAnnular:
        let s = stdDevFactor
        let sd = sd + spread + 1
        let x = sd / (factor + 0.5)
        let f = 255 * 1.1 * gaussian(x, s)
        c.a = if sd < 0.0: uint8(min(f, 255)) else: 0


      let idx = image.dataIndex(x, y)
      image.data[idx] = c.rgbx()

proc drawSdfShape*[I, T](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: T,
    pos: ColorRGBA,
    neg: ColorRGBA,
    mode: SDFMode,
    factor: float32 = 4,
    spread: float32 = 0.0,
    pointOffset: Vec2 = vec2(0.2, 0.2), ## offset the point by this amount, corrects pixelation at edges
    aaFactor: float32 = 1.2, ## factor to multiply sd by for AA
    stdDevFactor: float32 = 1/2.2, ## controls the standard deviation of the Gaussian blur, higher values result in a softer transition
) {.hasSimd, raises: [].} =
  drawSdfShapeImpl(image, center, wh, params, pos, neg, mode, factor, spread, pointOffset, aaFactor, stdDevFactor)

proc drawSdfShape*[I](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: MsdfBitmapParams,
    pos: ColorRGBA,
    neg: ColorRGBA,
    mode: SDFMode,
    factor: float32 = 4,
    spread: float32 = 0.0,
    pointOffset: Vec2 = vec2(0.2, 0.2), ## offset the point by this amount, corrects pixelation at edges
    aaFactor: float32 = 1.2, ## factor to multiply sd by for AA
    stdDevFactor: float32 = 1/2.2, ## controls the standard deviation of the Gaussian blur, higher values result in a softer transition
) {.raises: [].} =
  drawSdfShapeImpl(image, center, wh, params, pos, neg, mode, factor, spread, pointOffset, aaFactor, stdDevFactor)

proc drawSdfShapeNonSimd*[I, T](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: T,
    pos: ColorRGBA,
    neg: ColorRGBA,
    mode: SDFMode,
    factor: float32 = 4,
    spread: float32 = 0.0,
    pointOffset: Vec2 = vec2(0.2, 0.2), ## offset the point by this amount, corrects pixelation at edges
    aaFactor: float32 = 1.2, ## factor to multiply sd by for AA
    stdDevFactor: float32 = 1/2.2, ## controls the standard deviation of the Gaussian blur, higher values result in a softer transition
) {.raises: [].} =
  drawSdfShapeImpl(image, center, wh, params, pos, neg, mode, factor, spread, pointOffset, aaFactor, stdDevFactor)
