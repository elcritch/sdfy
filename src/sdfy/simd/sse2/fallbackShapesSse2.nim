import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/sse2

import ../../sdfytypes
import ../../shapes

import ./shapesSse2

when defined(release):
  {.push checks: off.}

proc sdBezierSimd*(px, py: M128, Ax, Ay, Bx, By, Cx, Cy: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for quadratic Bézier curve
  ## Processes 4 pixels at once
  ## For complex mathematical operations like Bézier curves, we fall back to scalar processing
  
  # For the complex Bézier algorithm with conditionals and mathematical functions
  # like pow, arccos, sqrt, etc., it's more efficient to process them individually
  var result_array: array[4, float32]
  var px_array, py_array: array[4, float32]
  
  # Extract SIMD values to arrays
  mm_storeu_ps(px_array[0].addr, px)
  mm_storeu_ps(py_array[0].addr, py)
  
  for i in 0..3:
    let
      pos = vec2(px_array[i], py_array[i])
      A = vec2(Ax, Ay)
      B = vec2(Bx, By)
      C = vec2(Cx, Cy)
    result_array[i] = sdBezier(pos, A, B, C)
  
  # Load result back into SIMD register
  result = mm_loadu_ps(result_array[0].addr)

proc sdEllipseSimd*(px, py: M128, abx, aby: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for ellipse
  ## Processes 4 pixels at once
  ## For complex mathematical operations like ellipse, we fall back to scalar processing
  
  # For the complex ellipse algorithm with conditionals and mathematical functions
  # like arccos, pow, etc., it's more efficient to process them individually
  var result_array: array[4, float32]
  var px_array, py_array: array[4, float32]
  
  # Extract SIMD values to arrays
  mm_storeu_ps(px_array[0].addr, px)
  mm_storeu_ps(py_array[0].addr, py)
  
  for i in 0..3:
    let
      pos = vec2(px_array[i], py_array[i])
      ab = vec2(abx, aby)
    result_array[i] = sdEllipse(pos, ab)
  
  # Load result back into SIMD register
  result = mm_loadu_ps(result_array[0].addr)


when defined(release):
  {.pop.}
