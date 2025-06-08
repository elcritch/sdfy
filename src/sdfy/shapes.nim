import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes

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
