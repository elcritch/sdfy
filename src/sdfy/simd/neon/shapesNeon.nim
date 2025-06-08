import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/neon

import ../../sdfytypes
import ../../shapes

when defined(release):
  {.push checks: off.}

when not compiles(vabsq_f32(float32x4(0.0))):
  func vabsq_f32*(a: float32x4): float32x4 {.header: "arm_neon.h".}
when not compiles(vsqrtq_f32(float32x4(0.0))):
  func vsqrtq_f32*(a: float32x4): float32x4 {.header: "arm_neon.h".}
when not compiles(vcvtq_u32_f32(float32x4(0.0))):
  func vcvtq_u32_f32*(a: float32x4): uint32x4 {.header: "arm_neon.h".}

proc sdRoundedBoxSimd*(px, py: float32x4, bx, by: float32, r: Vec4): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for rounded box
  ## Processes 4 pixels at once
  
  let
    zero = vmovq_n_f32(0.0)
    rx_xy = vmovq_n_f32(r.x) # top-right radius
    ry_xy = vmovq_n_f32(r.y) # bottom-right radius
    rz_zw = vmovq_n_f32(r.z) # bottom-left radius
    rw_zw = vmovq_n_f32(r.w) # top-left radius
    bx_vec = vmovq_n_f32(bx)
    by_vec = vmovq_n_f32(by)
  
  # Select corner radius based on quadrant
  # cornerRadius.xy = if p.x > 0.0: r.xy else: r.zw
  let
    px_pos = vcgtq_f32(px, zero) # px > 0
    py_pos = vcgtq_f32(py, zero) # py > 0
  
  # Select x-based radius (r.x/r.y if px > 0, else r.z/r.w)
  let radius_x = vbslq_f32(px_pos, rx_xy, rz_zw)
  let radius_y = vbslq_f32(px_pos, ry_xy, rw_zw)
  
  # cornerRadius.x = if p.y > 0.0: cornerRadius.x else: cornerRadius.y
  let corner_radius = vbslq_f32(py_pos, radius_x, radius_y)
  
  # Calculate q = abs(p) - b + vec2(cornerRadius.x, cornerRadius.x)
  let
    abs_px = vabsq_f32(px)
    abs_py = vabsq_f32(py)
    qx = vaddq_f32(vsubq_f32(abs_px, bx_vec), corner_radius)
    qy = vaddq_f32(vsubq_f32(abs_py, by_vec), corner_radius)
  
  # max(q, 0.0)
  let
    max_qx = vmaxq_f32(qx, zero)
    max_qy = vmaxq_f32(qy, zero)
  
  # length(max(q, 0.0)) = sqrt(max_qx^2 + max_qy^2)
  let
    max_qx_sq = vmulq_f32(max_qx, max_qx)
    max_qy_sq = vmulq_f32(max_qy, max_qy)
    length_sq = vaddq_f32(max_qx_sq, max_qy_sq)
  
  # sqrt approximation using vsqrtq_f32 (available in ARMv8)
  when defined(arm64) or defined(aarch64):
    let length_vec = vsqrtq_f32(length_sq)
  else:
    # Fallback for older ARM processors
    var length_array: array[4, float32]
    vst1q_f32(length_array[0].addr, length_sq)
    for i in 0..3:
      length_array[i] = sqrt(length_array[i])
    let length_vec = vld1q_f32(length_array[0].addr)
  
  # min(max(q.x, q.y), 0.0) + length - cornerRadius
  let
    max_q = vmaxq_f32(qx, qy)
    min_max_q = vminq_f32(max_q, zero)
  
  result = vaddq_f32(vsubq_f32(vaddq_f32(min_max_q, length_vec), corner_radius), zero)

proc sdChamferBoxSimd*(px, py: float32x4, bx, by: float32, chamfer: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for chamfer box
  ## Processes 4 pixels at once
  
  let
    zero = vmovq_n_f32(0.0)
    one = vmovq_n_f32(1.0)
    sqrt_half = vmovq_n_f32(sqrt(0.5))
    k = vmovq_n_f32(1.0 - sqrt(2.0))
    chamfer_vec = vmovq_n_f32(chamfer)
    bx_vec = vmovq_n_f32(bx)
    by_vec = vmovq_n_f32(by)
  
  # Calculate p = abs(p) - b
  let
    abs_px = vabsq_f32(px)
    abs_py = vabsq_f32(py)
    p_x = vsubq_f32(abs_px, bx_vec)
    p_y = vsubq_f32(abs_py, by_vec)
  
  # Swap coordinates if needed to put largest coordinate in x
  # p = if p.y > p.x: vec2(p.y, p.x) else: vec2(p.x, p.y)
  let
    py_gt_px = vcgtq_f32(p_y, p_x) # p.y > p.x
    swapped_x = vbslq_f32(py_gt_px, p_y, p_x)
    swapped_y = vbslq_f32(py_gt_px, p_x, p_y)
  
  # p.y += chamfer
  let adjusted_y = vaddq_f32(swapped_y, chamfer_vec)
  
  # Check condition: p.y < 0.0 and p.y + p.x * k < 0.0
  let
    y_lt_zero = vcltq_f32(adjusted_y, zero) # p.y < 0.0
    px_times_k = vmulq_f32(swapped_x, k)
    sum_condition = vaddq_f32(adjusted_y, px_times_k)
    sum_lt_zero = vcltq_f32(sum_condition, zero) # p.y + p.x * k < 0.0
    both_conditions = vandq_u32(y_lt_zero, sum_lt_zero)
  
  # Check if p.x < p.y (after adjustment)
  let x_lt_y = vcltq_f32(swapped_x, adjusted_y)
  
  # Calculate different return values based on conditions
  # Case 1: both_conditions = true -> return p.x
  let case1_result = swapped_x
  
  # Case 2: x_lt_y = true -> return (p.x + p.y) * sqrt(0.5)
  let case2_result = vmulq_f32(vaddq_f32(swapped_x, adjusted_y), sqrt_half)
  
  # Case 3: default -> return length(p) = sqrt(p.x^2 + p.y^2)
  let
    x_sq = vmulq_f32(swapped_x, swapped_x)
    y_sq = vmulq_f32(adjusted_y, adjusted_y)
    length_sq = vaddq_f32(x_sq, y_sq)
  
  # sqrt approximation using vsqrtq_f32 (available in ARMv8)
  when defined(arm64) or defined(aarch64):
    let case3_result = vsqrtq_f32(length_sq)
  else:
    # Fallback for older ARM processors
    var length_array: array[4, float32]
    vst1q_f32(length_array[0].addr, length_sq)
    for i in 0..3:
      length_array[i] = sqrt(length_array[i])
    let case3_result = vld1q_f32(length_array[0].addr)
  
  # Select the appropriate result based on conditions
  # First select between case2 and case3 based on x_lt_y
  let case23_result = vbslq_f32(x_lt_y, case2_result, case3_result)
  
  # Then select between case1 and case23 based on both_conditions
  result = vbslq_f32(both_conditions, case1_result, case23_result)

proc sdCircleSimd*(px, py: float32x4, r: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for circle
  ## Processes 4 pixels at once
  
  let
    r_vec = vmovq_n_f32(r)
  
  # Calculate length(p) = sqrt(px^2 + py^2)
  let
    px_sq = vmulq_f32(px, px)
    py_sq = vmulq_f32(py, py)
    length_sq = vaddq_f32(px_sq, py_sq)
  
  # sqrt approximation using vsqrtq_f32 (available in ARMv8)
  when defined(arm64) or defined(aarch64):
    let length_vec = vsqrtq_f32(length_sq)
  else:
    # Fallback for older ARM processors
    var length_array: array[4, float32]
    vst1q_f32(length_array[0].addr, length_sq)
    for i in 0..3:
      length_array[i] = sqrt(length_array[i])
    let length_vec = vld1q_f32(length_array[0].addr)
  
  # Return length(p) - r
  result = vsubq_f32(length_vec, r_vec)

proc sdBoxSimd*(px, py: float32x4, bx, by: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for box
  ## Processes 4 pixels at once
  
  let
    zero = vmovq_n_f32(0.0)
    bx_vec = vmovq_n_f32(bx)
    by_vec = vmovq_n_f32(by)
  
  # Calculate d = abs(p) - b
  let
    abs_px = vabsq_f32(px)
    abs_py = vabsq_f32(py)
    dx = vsubq_f32(abs_px, bx_vec)
    dy = vsubq_f32(abs_py, by_vec)
  
  # max(d, 0.0)
  let
    max_dx = vmaxq_f32(dx, zero)
    max_dy = vmaxq_f32(dy, zero)
  
  # length(max(d, 0.0)) = sqrt(max_dx^2 + max_dy^2)
  let
    max_dx_sq = vmulq_f32(max_dx, max_dx)
    max_dy_sq = vmulq_f32(max_dy, max_dy)
    length_sq = vaddq_f32(max_dx_sq, max_dy_sq)
  
  # sqrt approximation using vsqrtq_f32
  when defined(arm64) or defined(aarch64):
    let length_vec = vsqrtq_f32(length_sq)
  else:
    # Fallback for older ARM processors
    var length_array: array[4, float32]
    vst1q_f32(length_array[0].addr, length_sq)
    for i in 0..3:
      length_array[i] = sqrt(length_array[i])
    let length_vec = vld1q_f32(length_array[0].addr)
  
  # min(max(d.x, d.y), 0.0)
  let
    max_d = vmaxq_f32(dx, dy)
    min_max_d = vminq_f32(max_d, zero)
  
  # Return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0)
  result = vaddq_f32(length_vec, min_max_d)


proc sdArcSimd*(px, py: float32x4, scx, scy, ra, rb: float32): float32x4 {.inline, raises: [].} =
  ## SIMD version of signed distance function for arc
  ## Processes 4 pixels at once using pure NEON SIMD operations
  
  let
    zero = vmovq_n_f32(0.0)
    scx_vec = vmovq_n_f32(scx)
    scy_vec = vmovq_n_f32(scy)
    ra_vec = vmovq_n_f32(ra)
    rb_vec = vmovq_n_f32(rb)
  
  # p.x = abs(p.x) - take absolute value of x coordinate
  let px_abs = vabsq_f32(px)
  
  # Calculate the condition: sc.y * px_abs > sc.x * py
  let
    left_side = vmulq_f32(scy_vec, px_abs)   # sc.y * px_abs
    right_side = vmulq_f32(scx_vec, py)      # sc.x * py
    condition = vcgtq_f32(left_side, right_side)  # left > right
  
  # Calculate length(p_mod) = sqrt(px_abs^2 + py^2) for both cases
  let
    px_abs_sq = vmulq_f32(px_abs, px_abs)
    py_sq = vmulq_f32(py, py)
    length_p_sq = vaddq_f32(px_abs_sq, py_sq)
  
  # sqrt approximation using vsqrtq_f32
  when defined(arm64) or defined(aarch64):
    let length_p = vsqrtq_f32(length_p_sq)
  else:
    # Fallback for older ARM processors
    var length_array: array[4, float32]
    vst1q_f32(length_array[0].addr, length_p_sq)
    for i in 0..3:
      length_array[i] = sqrt(length_array[i])
    let length_p = vld1q_f32(length_array[0].addr)
  
  # Case 1 (condition true): length(p_mod - sc*ra) - rb
  # Calculate sc * ra
  let
    sc_ra_x = vmulq_f32(scx_vec, ra_vec)  # sc.x * ra
    sc_ra_y = vmulq_f32(scy_vec, ra_vec)  # sc.y * ra
  
  # Calculate p_mod - sc*ra
  let
    diff_x = vsubq_f32(px_abs, sc_ra_x)   # px_abs - sc.x*ra
    diff_y = vsubq_f32(py, sc_ra_y)       # py - sc.y*ra
  
  # Calculate length(p_mod - sc*ra)
  let
    diff_x_sq = vmulq_f32(diff_x, diff_x)
    diff_y_sq = vmulq_f32(diff_y, diff_y)
    diff_length_sq = vaddq_f32(diff_x_sq, diff_y_sq)
  
  when defined(arm64) or defined(aarch64):
    let diff_length = vsqrtq_f32(diff_length_sq)
  else:
    # Fallback for older ARM processors
    var diff_length_array: array[4, float32]
    vst1q_f32(diff_length_array[0].addr, diff_length_sq)
    for i in 0..3:
      diff_length_array[i] = sqrt(diff_length_array[i])
    let diff_length = vld1q_f32(diff_length_array[0].addr)
  
  let case1_result = vsubq_f32(diff_length, rb_vec)  # length(p_mod - sc*ra) - rb
  
  # Case 2 (condition false): abs(length(p_mod) - ra) - rb
  let
    length_minus_ra = vsubq_f32(length_p, ra_vec)    # length(p_mod) - ra
    abs_length_minus_ra = vabsq_f32(length_minus_ra) # abs(length(p_mod) - ra)
    case2_result = vsubq_f32(abs_length_minus_ra, rb_vec)  # abs(length(p_mod) - ra) - rb
  
  # Select result based on condition: condition ? case1_result : case2_result
  result = vbslq_f32(condition, case1_result, case2_result)

when defined(release):
  {.pop.}
