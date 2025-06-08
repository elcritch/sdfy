import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes
import ./simd/shapesSimd

export shapesSimd

# Note these functions were adapted from https://iquilezles.org/articles/distfunctions2d/
# under the MIT license.

func sdRoundedBox*(p: Vec2, b: Vec2, r: Vec4): float32 {.inline.} =
  ## Signed distance function for a rounded box
  ## p: point to test
  ## b: box half-extents (width/2, height/2)
  ## r: corner radii as Vec4 (x=top-right, y=bottom-right, z=bottom-left, w=top-left)
  ## Returns: signed distance (negative inside, positive outside)
  var cornerRadius = r
  
  # Select appropriate corner radius based on quadrant
  cornerRadius.xy = if p.x > 0.0: r.xy else: r.zw
  cornerRadius.x = if p.y > 0.0: cornerRadius.x else: cornerRadius.y
  
  # Calculate distance
  let q = abs(p) - b + vec2(cornerRadius.x, cornerRadius.x)
  
  result = min(max(q.x, q.y), 0.0) + length(max(q, vec2(0.0, 0.0))) - cornerRadius.x

func sdChamferBox*(p: Vec2, b: Vec2, chamfer: float32): float32 {.inline.} =
  ## Signed distance function for a chamfered box
  ## p: point to test
  ## b: box half-extents (width/2, height/2)
  ## chamfer: chamfer amount
  ## Returns: signed distance (negative inside, positive outside)
  var p = abs(p) - b
  
  # Swap coordinates if needed to put largest coordinate in x
  p = if p.y > p.x: vec2(p.y, p.x) else: vec2(p.x, p.y)
  p.y += chamfer
  
  let k = 1.0 - sqrt(2.0)
  if p.y < 0.0 and p.y + p.x * k < 0.0:
    return p.x
  
  if p.x < p.y:
    return (p.x + p.y) * sqrt(0.5)
  
  return length(p)

func sdCircle*(p: Vec2, r: float32): float32 {.inline.} =
  ## Signed distance function for a circle
  ## p: point to test
  ## r: radius
  ## Returns: signed distance (negative inside, positive outside)
  return length(p) - r

func dot2*(v: Vec2): float32 {.inline.} =
  ## Helper function to compute dot product of vector with itself (squared length)
  return dot(v, v)

func sdBezier*(pos: Vec2, A: Vec2, B: Vec2, C: Vec2): float32 {.inline.} =
  ## Signed distance function for a quadratic Bézier curve
  ## pos: point to test
  ## A, B, C: control points of the quadratic Bézier curve
  ## Returns: distance to the curve (always positive for curves)
  let
    a = B - A
    b = A - 2.0'f32*B + C
    c = a * 2.0'f32
    d = A - pos
    kk = 1.0'f32 / dot(b, b)
    kx = kk * dot(a, b)
    ky = kk * (2.0'f32*dot(a, a) + dot(d, b)) / 3.0'f32
    kz = kk * dot(d, a)
  
  var res = 0.0'f32
  let
    p = ky - kx*kx
    p3 = p*p*p
    q = kx*(2.0'f32*kx*kx - 3.0'f32*ky) + kz
    h = q*q + 4.0'f32*p3
  
  if h >= 0.0'f32:
    let
      h_sqrt = sqrt(h)
      x = vec2((h_sqrt - q) / 2.0'f32, (-h_sqrt - q) / 2.0'f32)
      # sign(x) * pow(abs(x), 1/3)
      uv = vec2(
        sign(x.x) * pow(abs(x.x), 1.0'f32/3.0'f32),
        sign(x.y) * pow(abs(x.y), 1.0'f32/3.0'f32)
      )
      t = clamp(uv.x + uv.y - kx, 0.0'f32, 1.0'f32)
    res = dot2(d + (c + b*t)*t)
  else:
    let
      z = sqrt(-p)
      v = arccos(q / (p*z*2.0'f32)) / 3.0'f32
      m = cos(v)
      n = sin(v) * 1.732050808'f32  # sqrt(3)
      t1 = clamp((m + m)*z - kx, 0.0'f32, 1.0'f32)
      t2 = clamp((-n - m)*z - kx, 0.0'f32, 1.0'f32)
      res1 = dot2(d + (c + b*t1)*t1)
      res2 = dot2(d + (c + b*t2)*t2)
    res = min(res1, res2)
  
  return sqrt(res)

proc drawSdfShape*[I, T](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: T,
    pos: ColorRGBA,
    neg: ColorRGBA,
    factor: float32 = 4,
    spread: float32 = 0.0,
    mode: SDFMode = sdfModeFeatherInv
) {.hasSimd, raises: [].} =
  ## Generic signed distance function for shapes
  ## Supports rounded boxes, chamfered boxes, and circles based on params type
  ## T: RoundedBoxParams, ChamferBoxParams, or CircleParams

  for y in 0 ..< image.height:
    for x in 0 ..< image.width:
      let p = vec2(x.float32, y.float32) - center
      
      # Select the appropriate SDF function based on parameter type
      let sd = when T is RoundedBoxParams:
        let b = wh / 2.0
        sdRoundedBox(p, b, params.r)
      elif T is ChamferBoxParams:
        let b = wh / 2.0
        sdChamferBox(p, b, params.chamfer)
      elif T is CircleParams:
        sdCircle(p, params.r)
      elif T is BezierParams:
        sdBezier(p, params.A, params.B, params.C)
      else:
        {.error: "Unsupported shape parameter type".}

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
        c.a = if sd > 0.0: uint8(min(f * 255 * 6, 255)) else: 255
      of sdfModeInsetShadow:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd < 0.0: uint8(min(f * 255 * 6, 255)) else: 255
      of sdfModeInsetShadowAnnular:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd < 0.0: uint8(min(f * 255 * 6, 255)) else: 0


      let idx = image.dataIndex(x, y)
      image.data[idx] = c.rgbx()
