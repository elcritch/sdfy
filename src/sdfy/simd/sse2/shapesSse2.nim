import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/sse2

import ../../sdfytypes
import ../../shapes

when defined(release):
  {.push checks: off.}

proc sdRoundedBoxSimd*(px, py: M128, bx, by: float32, r: Vec4): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for rounded box
  ## Processes 4 pixels at once
  ## Based on the scalar version sdRoundedBox and NEON implementation
  
  let
    zero = mm_setzero_ps()
    rx_xy = mm_set1_ps(r.x) # top-right radius
    ry_xy = mm_set1_ps(r.y) # bottom-right radius 
    rz_zw = mm_set1_ps(r.z) # bottom-left radius
    rw_zw = mm_set1_ps(r.w) # top-left radius
    bx_vec = mm_set1_ps(bx)
    by_vec = mm_set1_ps(by)
  
  # Select corner radius based on quadrant
  # cornerRadius.xy = if p.x > 0.0: r.xy else: r.zw
  let
    px_pos = mm_cmpgt_ps(px, zero) # px > 0
    py_pos = mm_cmpgt_ps(py, zero) # py > 0
  
  # Select x-based radius (r.x/r.y if px > 0, else r.z/r.w)
  # SSE2 doesn't have mm_blendv_ps, so we use the traditional approach:
  # result = (mask & a) | (~mask & b)
  let 
    radius_x = mm_or_ps(mm_and_ps(px_pos, rx_xy), mm_andnot_ps(px_pos, rz_zw))
    radius_y = mm_or_ps(mm_and_ps(px_pos, ry_xy), mm_andnot_ps(px_pos, rw_zw))
  
  # cornerRadius.x = if p.y > 0.0: cornerRadius.x else: cornerRadius.y
  let corner_radius = mm_or_ps(mm_and_ps(py_pos, radius_x), mm_andnot_ps(py_pos, radius_y))
  
  # Calculate abs(p) - we need to compute absolute value manually in SSE2
  # abs(x) = max(x, -x)
  let
    neg_px = mm_sub_ps(zero, px)
    neg_py = mm_sub_ps(zero, py)
    abs_px = mm_max_ps(px, neg_px)
    abs_py = mm_max_ps(py, neg_py)
  
  # Calculate q = abs(p) - b + vec2(cornerRadius.x, cornerRadius.x)
  let
    qx = mm_add_ps(mm_sub_ps(abs_px, bx_vec), corner_radius)
    qy = mm_add_ps(mm_sub_ps(abs_py, by_vec), corner_radius)
  
  # max(q, 0.0)
  let
    max_qx = mm_max_ps(qx, zero)
    max_qy = mm_max_ps(qy, zero)
  
  # length(max(q, 0.0)) = sqrt(max_qx^2 + max_qy^2)
  let
    max_qx_sq = mm_mul_ps(max_qx, max_qx)
    max_qy_sq = mm_mul_ps(max_qy, max_qy)
    length_sq = mm_add_ps(max_qx_sq, max_qy_sq)
    length_vec = mm_sqrt_ps(length_sq)
  
  # min(max(q.x, q.y), 0.0) + length - cornerRadius
  let
    max_q = mm_max_ps(qx, qy)
    min_max_q = mm_min_ps(max_q, zero)
  
  # Final result: min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - cornerRadius
  result = mm_sub_ps(mm_add_ps(min_max_q, length_vec), corner_radius)


when defined(release):
  {.pop.}
