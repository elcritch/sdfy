import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes

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

func sdBox*(p: Vec2, b: Vec2): float32 {.inline.} =
  ## Signed distance function for a box/rectangle
  ## p: point to test
  ## b: box half-extents (width/2, height/2)
  ## Returns: signed distance (negative inside, positive outside)
  let d = abs(p) - b
  return length(max(d, vec2(0.0, 0.0))) + min(max(d.x, d.y), 0.0)

func sdEllipse*(p: Vec2, ab: Vec2): float32 =
  ## Signed distance function for an ellipse
  ## p: point to test
  ## ab: ellipse semi-axes (width/2, height/2)
  ## Returns: signed distance (negative inside, positive outside)
  var
    p = abs(p)
    ab = ab
  
  # Swap coordinates if needed to ensure p.x <= p.y
  if p.x > p.y:
    p = vec2(p.y, p.x)
    ab = vec2(ab.y, ab.x)
  
  let
    l = ab.y*ab.y - ab.x*ab.x
    m = ab.x*p.x/l
    m2 = m*m
    n = ab.y*p.y/l
    n2 = n*n
    c = (m2 + n2 - 1.0'f32) / 3.0'f32
    c3 = c*c*c
    q = c3 + m2*n2*2.0'f32
    d = c3 + m2*n2
    g = m + m*n2
  
  var co: float32
  if d < 0.0'f32:
    let
      h = arccos(q/c3) / 3.0'f32
      s = cos(h)
      t = sin(h) * sqrt(3.0'f32)
      rx = sqrt(-c*(s + t + 2.0'f32) + m2)
      ry = sqrt(-c*(s - t + 2.0'f32) + m2)
    co = (ry + sign(l)*rx + abs(g)/(rx*ry) - m) / 2.0'f32
  else:
    let
      h = 2.0'f32*m*n*sqrt(d)
      s = sign(q + h) * pow(abs(q + h), 1.0'f32/3.0'f32)
      u = sign(q - h) * pow(abs(q - h), 1.0'f32/3.0'f32)
      rx = -s - u - c*4.0'f32 + 2.0'f32*m2
      ry = (s - u) * sqrt(3.0'f32)
      rm = sqrt(rx*rx + ry*ry)
    co = (ry/sqrt(rm - rx) + 2.0'f32*g/rm - m) / 2.0'f32
  
  let r = ab * vec2(co, sqrt(1.0'f32 - co*co))
  return length(r - p) * sign(p.y - r.y)

func sdArc*(p: Vec2, sc: Vec2, ra: float32, rb: float32): float32 {.inline.} =
  ## Signed distance function for an arc
  ## p: point to test
  ## sc: sin/cos of the arc's aperture (sc.x = sin, sc.y = cos)
  ## ra: inner radius
  ## rb: thickness (outer radius difference)
  ## Returns: signed distance (negative inside, positive outside)
  
  # p.x = abs(p.x) - equivalent to taking absolute value of x coordinate
  var p_mod = vec2(abs(p.x), p.y)
  
  # Ternary operator: (sc.y*p.x>sc.x*p.y) ? length(p-sc*ra) : abs(length(p)-ra)) - rb
  if sc.y * p_mod.x > sc.x * p_mod.y:
    # First case: length(p - sc*ra) - rb
    return length(p_mod - sc * ra) - rb
  else:
    # Second case: abs(length(p) - ra) - rb
    return abs(length(p_mod) - ra) - rb

func sdParallelogram*(p: Vec2, wi: float32, he: float32, sk: float32): float32 {.inline.} =
  ## Signed distance function for a parallelogram
  ## p: point to test
  ## wi: width
  ## he: height  
  ## sk: skew
  ## Returns: signed distance (negative inside, positive outside)
  let e = vec2(sk, he)
  var p = if p.y < 0.0: -p else: p
  
  var w = p - e
  w.x -= clamp(w.x, -wi, wi)
  var d = vec2(dot(w, w), -w.y)
  
  let s = p.x * e.y - p.y * e.x
  p = if s < 0.0: -p else: p
  
  var v = p - vec2(wi, 0.0)
  v -= e * clamp(dot(v, e) / dot(e, e), -1.0, 1.0)
  d = vec2(min(d.x, dot(v, v)), min(d.y, wi * he - abs(s)))
  
  return sqrt(d.x) * sign(-d.y)

func sdPie*(p: Vec2, c: Vec2, r: float32): float32 {.inline.} =
  ## Signed distance function for a pie slice
  ## p: point to test
  ## c: sin/cos of the pie's aperture (c.x = sin, c.y = cos)
  ## r: radius
  ## Returns: signed distance (negative inside, positive outside)
  
  # p.x = abs(p.x) - equivalent to taking absolute value of x coordinate
  var p_mod = vec2(abs(p.x), p.y)
  
  # Calculate l = length(p) - r
  let l = length(p_mod) - r
  
  # Calculate m = length(p - c*clamp(dot(p,c), 0.0, r))
  let
    dot_pc = dot(p_mod, c)
    clamped_dot = clamp(dot_pc, 0.0, r)
    m = length(p_mod - c * clamped_dot)
  
  # Calculate sign(c.y*p.x - c.x*p.y)
  let sign_val = sign(c.y * p_mod.x - c.x * p_mod.y)
  
  # Return max(l, m*sign_val)
  return max(l, m * sign_val)

func sdRing*(p: Vec2, n: Vec2, r: float32, th: float32): float32 {.inline.} =
  ## Signed distance function for a ring
  ## p: point to test
  ## n: sin/cos of the ring's aperture (n.x = sin, n.y = cos)
  ## r: radius
  ## th: thickness
  ## Returns: signed distance (negative inside, positive outside)
  
  # p.x = abs(p.x)
  var p_mod = vec2(abs(p.x), p.y)
  
  # Apply 2x2 rotation matrix: mat2x2(n.x,n.y,-n.y,n.x) * p
  # This rotates p by the angle defined by n
  let rotated_p = vec2(
    n.x * p_mod.x + n.y * p_mod.y,
    -n.y * p_mod.x + n.x * p_mod.y
  )
  
  # Calculate the two distance components
  let
    # abs(length(p) - r) - th*0.5
    d1 = abs(length(rotated_p) - r) - th * 0.5
    
    # length(vec2(p.x, max(0.0, abs(r - p.y) - th*0.5))) * sign(p.x)
    max_val = max(0.0, abs(r - rotated_p.y) - th * 0.5)
    d2 = length(vec2(rotated_p.x, max_val)) * sign(rotated_p.x)
  
  # Return max(d1, d2)
  return max(d1, d2)

# Note these functions were adapted from https://iquilezles.org/articles/distfunctions2d/
# under the MIT license.

func median3(a, b, c: float32): float32 {.inline.} =
  max(min(a, b), min(max(a, b), c))

func msdfSampleMedian*(image: Image; pos: Vec2; flipY: bool): float32 {.inline.} =
  var px = clamp(pos.x, 0.0, image.width.float32)
  var py = clamp(pos.y, 0.0, image.height.float32)
  #px -= 0.5
  #py -= 0.5
  if flipY:
    py = (image.height.float32 - 1.0) - py
  let x0 = px.floor.int
  let y0 = py.floor.int
  let x1 = x0 + 1
  let y1 = y0 + 1
  let xf = px - px.floor
  let yf = py - py.floor

  let inv = 1.0'f32 / 255.0'f32
  let c00 = image[x0, y0]
  let c10 = image[x1, y0]
  let c01 = image[x0, y1]
  let c11 = image[x1, y1]

  let a00 = c00.a.float32 * inv
  let a10 = c10.a.float32 * inv
  let a01 = c01.a.float32 * inv
  let a11 = c11.a.float32 * inv

  let r00 = if a00 <= 0: 0.0'f32 else: clamp((c00.r.float32 * inv) / a00, 0.0'f32, 1.0'f32)
  let g00 = if a00 <= 0: 0.0'f32 else: clamp((c00.g.float32 * inv) / a00, 0.0'f32, 1.0'f32)
  let b00 = if a00 <= 0: 0.0'f32 else: clamp((c00.b.float32 * inv) / a00, 0.0'f32, 1.0'f32)
  let r10 = if a10 <= 0: 0.0'f32 else: clamp((c10.r.float32 * inv) / a10, 0.0'f32, 1.0'f32)
  let g10 = if a10 <= 0: 0.0'f32 else: clamp((c10.g.float32 * inv) / a10, 0.0'f32, 1.0'f32)
  let b10 = if a10 <= 0: 0.0'f32 else: clamp((c10.b.float32 * inv) / a10, 0.0'f32, 1.0'f32)
  let r01 = if a01 <= 0: 0.0'f32 else: clamp((c01.r.float32 * inv) / a01, 0.0'f32, 1.0'f32)
  let g01 = if a01 <= 0: 0.0'f32 else: clamp((c01.g.float32 * inv) / a01, 0.0'f32, 1.0'f32)
  let b01 = if a01 <= 0: 0.0'f32 else: clamp((c01.b.float32 * inv) / a01, 0.0'f32, 1.0'f32)
  let r11 = if a11 <= 0: 0.0'f32 else: clamp((c11.r.float32 * inv) / a11, 0.0'f32, 1.0'f32)
  let g11 = if a11 <= 0: 0.0'f32 else: clamp((c11.g.float32 * inv) / a11, 0.0'f32, 1.0'f32)
  let b11 = if a11 <= 0: 0.0'f32 else: clamp((c11.b.float32 * inv) / a11, 0.0'f32, 1.0'f32)

  let r = lerp(lerp(r00, r10, xf), lerp(r01, r11, xf), yf)
  let g = lerp(lerp(g00, g10, xf), lerp(g01, g11, xf), yf)
  let b = lerp(lerp(b00, b10, xf), lerp(b01, b11, xf), yf)
  median3(r, g, b)

func msdfSampleAlpha*(image: Image; pos: Vec2; flipY: bool): float32 {.inline.} =
  var px = clamp(pos.x, 0.0, image.width.float32)
  var py = clamp(pos.y, 0.0, image.height.float32)
  if flipY:
    py = (image.height.float32 - 1.0) - py
  let x0 = px.floor.int
  let y0 = py.floor.int
  let x1 = x0 + 1
  let y1 = y0 + 1
  let xf = px - px.floor
  let yf = py - py.floor

  let inv = 1.0'f32 / 255.0'f32
  let a00 = image[x0, y0].a.float32 * inv
  let a10 = image[x1, y0].a.float32 * inv
  let a01 = image[x0, y1].a.float32 * inv
  let a11 = image[x1, y1].a.float32 * inv

  lerp(lerp(a00, a10, xf), lerp(a01, a11, xf), yf)


func sdMsdfBitmap*(p: Vec2; wh: Vec2; params: MsdfBitmapParams): float32 {.inline.} =
  let w = max(wh.x, 1.0'f32)
  let h = max(wh.y, 1.0'f32)
  let imgW = params.image.width.float32
  let imgH = params.image.height.float32
  let local = p + wh * 0.5
  let scaleX = imgW / w
  let scaleY = imgH / h
  let base = vec2(local.x * scaleX, local.y * scaleY)
  let sd = (
    if params.useAlpha:
      msdfSampleAlpha(params.image, base, params.flipY)
    else:
      msdfSampleMedian(params.image, base, params.flipY)
  ) - params.sdThreshold
  #let rangeScale = (w + h) / (imgW + imgH)
  -((params.pxRange * 1.0) * sd)
