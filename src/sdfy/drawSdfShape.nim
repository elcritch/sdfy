import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes
import ./shapes

import ./simd/shapesSimd

proc drawSdfShapeImpl*[I, T](
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
) {.raises: [].} =
  ## Generic signed distance function for shapes
  ## Supports rounded boxes, chamfered boxes, circles, Bézier curves, boxes, ellipses, arcs, parallelograms, pies, and rings based on params type
  ## T: RoundedBoxParams, ChamferBoxParams, CircleParams, BezierParams, BoxParams, EllipseParams, ArcParams, ParallelogramParams, PieParams, or RingParams
  mixin dataIndex

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
        let cl = clamp(sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
      of sdfModeAnnular:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
      of sdfModeAnnularAA:
        let sd = abs(sd + factor) - factor;
        c = if sd < 0.0: pos else: neg
        let cl = clamp(sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
      of sdfModeFeather:
        c.a = uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherInv:
        c.a = 255 - uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherGaussian:
        let sd = sd
        let s = 2.2
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = uint8(f * 255)
      of sdfModeDropShadow:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd > 0.0: uint8(min(f * 255 * 6, 255)) else: c.a
      of sdfModeDropShadowAA:
        let s = 2.2
        let cl = clamp(sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd >= 0.0: uint8(min(f * 255 * 6, 255)) else: c.a
      of sdfModeInsetShadow:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        if sd < 0.0:
          c.a = uint8(min(f * 255 * 6, 255))
        else:
          discard
      of sdfModeInsetShadowAnnular:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd < 0.0: uint8(min(f * 255 * 6, 255)) else: 0


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
) {.hasSimd, raises: [].} =
  drawSdfShapeImpl(image, center, wh, params, pos, neg, mode, factor, spread, pointOffset)

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
) {.raises: [].} =
  drawSdfShapeImpl(image, center, wh, params, pos, neg, mode, factor, spread, pointOffset)
