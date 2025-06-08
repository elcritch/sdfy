import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/neon

import ../../sdfytypes
import ../../shapes

when defined(release):
  {.push checks: off.}

import ./shapesNeon

proc sdBezierSimd*(px, py: float32x4, Ax, Ay, Bx, By, Cx, Cy: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for quadratic Bézier curve
  ## Processes 4 pixels at once
  ## For complex mathematical operations like Bézier curves, we'll fall back to scalar processing
  
  # For the complex Bézier curve algorithm with conditionals and mathematical functions
  # like arccos, pow, etc., it's more efficient to process them individually
  var result_array: array[4, float32]
  var px_array, py_array: array[4, float32]
  
  # Extract SIMD values to arrays
  vst1q_f32(px_array[0].addr, px)
  vst1q_f32(py_array[0].addr, py)
  
  # Helper function for dot product squared
  proc dot2(v: Vec2): float32 {.inline.} = dot(v, v)
  
  # Process each pixel individually using the scalar Bézier algorithm
  for i in 0..3:
    let
      pos = vec2(px_array[i], py_array[i])
      A = vec2(Ax, Ay)
      B = vec2(Bx, By)
      C = vec2(Cx, Cy)
      
    result_array[i] = sdBezier(pos, A, B, C)

  # Load result back into SIMD register
  result = vld1q_f32(result_array[0].addr)

proc sdEllipseSimd*(px, py: float32x4, abx, aby: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for ellipse
  ## Processes 4 pixels at once
  ## For complex mathematical operations like ellipse, we'll fall back to scalar processing
  
  # For the complex ellipse algorithm with conditionals and mathematical functions
  # like arccos, pow, etc., it's more efficient to process them individually
  var result_array: array[4, float32]
  var px_array, py_array: array[4, float32]
  
  # Extract SIMD values to arrays
  vst1q_f32(px_array[0].addr, px)
  vst1q_f32(py_array[0].addr, py)
  
  for i in 0..3:
    let
      pos = vec2(px_array[i], py_array[i])
      ab = vec2(abx, aby)
    result_array[i] = sdEllipse(pos, ab)
  
  # Load result back into SIMD register
  result = vld1q_f32(result_array[0].addr)

when defined(release):
  {.pop.}
