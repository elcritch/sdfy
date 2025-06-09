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

proc sdChamferBoxSimd*(px, py: M128, bx, by: float32, chamfer: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for chamfer box
  ## Processes 4 pixels at once
  
  let
    zero = mm_setzero_ps()
    one = mm_set1_ps(1.0)
    sqrt_half = mm_set1_ps(sqrt(0.5))
    k = mm_set1_ps(1.0 - sqrt(2.0))
    chamfer_vec = mm_set1_ps(chamfer)
    bx_vec = mm_set1_ps(bx)
    by_vec = mm_set1_ps(by)
  
  # Calculate p = abs(p) - b
  let
    neg_px = mm_sub_ps(zero, px)
    neg_py = mm_sub_ps(zero, py)
    abs_px = mm_max_ps(px, neg_px)
    abs_py = mm_max_ps(py, neg_py)
    p_x = mm_sub_ps(abs_px, bx_vec)
    p_y = mm_sub_ps(abs_py, by_vec)
  
  # Swap coordinates if needed to put largest coordinate in x
  # p = if p.y > p.x: vec2(p.y, p.x) else: vec2(p.x, p.y)
  let
    py_gt_px = mm_cmpgt_ps(p_y, p_x) # p.y > p.x
    swapped_x = mm_or_ps(mm_and_ps(py_gt_px, p_y), mm_andnot_ps(py_gt_px, p_x))
    swapped_y = mm_or_ps(mm_and_ps(py_gt_px, p_x), mm_andnot_ps(py_gt_px, p_y))
  
  # p.y += chamfer
  let adjusted_y = mm_add_ps(swapped_y, chamfer_vec)
  
  # Check condition: p.y < 0.0 and p.y + p.x * k < 0.0
  let
    y_lt_zero = mm_cmplt_ps(adjusted_y, zero) # p.y < 0.0
    px_times_k = mm_mul_ps(swapped_x, k)
    sum_condition = mm_add_ps(adjusted_y, px_times_k)
    sum_lt_zero = mm_cmplt_ps(sum_condition, zero) # p.y + p.x * k < 0.0
    both_conditions = mm_and_ps(y_lt_zero, sum_lt_zero)
  
  # Check if p.x < p.y (after adjustment)
  let x_lt_y = mm_cmplt_ps(swapped_x, adjusted_y)
  
  # Calculate different return values based on conditions
  # Case 1: both_conditions = true -> return p.x
  let case1_result = swapped_x
  
  # Case 2: x_lt_y = true -> return (p.x + p.y) * sqrt(0.5)
  let case2_result = mm_mul_ps(mm_add_ps(swapped_x, adjusted_y), sqrt_half)
  
  # Case 3: default -> return length(p) = sqrt(p.x^2 + p.y^2)
  let
    x_sq = mm_mul_ps(swapped_x, swapped_x)
    y_sq = mm_mul_ps(adjusted_y, adjusted_y)
    length_sq = mm_add_ps(x_sq, y_sq)
    case3_result = mm_sqrt_ps(length_sq)
  
  # Select the appropriate result based on conditions
  # First select between case2 and case3 based on x_lt_y
  let case23_result = mm_or_ps(mm_and_ps(x_lt_y, case2_result), mm_andnot_ps(x_lt_y, case3_result))
  
  # Then select between case1 and case23 based on both_conditions
  result = mm_or_ps(mm_and_ps(both_conditions, case1_result), mm_andnot_ps(both_conditions, case23_result))

proc sdCircleSimd*(px, py: M128, r: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for circle
  ## Processes 4 pixels at once
  
  let
    r_vec = mm_set1_ps(r)
  
  # Calculate length(p) = sqrt(px^2 + py^2)
  let
    px_sq = mm_mul_ps(px, px)
    py_sq = mm_mul_ps(py, py)
    length_sq = mm_add_ps(px_sq, py_sq)
    length_vec = mm_sqrt_ps(length_sq)
  
  # Return length(p) - r
  result = mm_sub_ps(length_vec, r_vec)

proc sdBoxSimd*(px, py: M128, bx, by: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for box
  ## Processes 4 pixels at once
  
  let
    zero = mm_setzero_ps()
    bx_vec = mm_set1_ps(bx)
    by_vec = mm_set1_ps(by)
  
  # Calculate d = abs(p) - b
  let
    neg_px = mm_sub_ps(zero, px)
    neg_py = mm_sub_ps(zero, py)
    abs_px = mm_max_ps(px, neg_px)
    abs_py = mm_max_ps(py, neg_py)
    dx = mm_sub_ps(abs_px, bx_vec)
    dy = mm_sub_ps(abs_py, by_vec)
  
  # max(d, 0.0)
  let
    max_dx = mm_max_ps(dx, zero)
    max_dy = mm_max_ps(dy, zero)
  
  # length(max(d, 0.0)) = sqrt(max_dx^2 + max_dy^2)
  let
    max_dx_sq = mm_mul_ps(max_dx, max_dx)
    max_dy_sq = mm_mul_ps(max_dy, max_dy)
    length_sq = mm_add_ps(max_dx_sq, max_dy_sq)
    length_vec = mm_sqrt_ps(length_sq)
  
  # min(max(d.x, d.y), 0.0)
  let
    max_d = mm_max_ps(dx, dy)
    min_max_d = mm_min_ps(max_d, zero)
  
  # Return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0)
  result = mm_add_ps(length_vec, min_max_d)

proc sdArcSimd*(px, py: M128, scx, scy, ra, rb: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for arc
  ## Processes 4 pixels at once using pure SSE2 SIMD operations
  
  let
    zero = mm_setzero_ps()
    scx_vec = mm_set1_ps(scx)
    scy_vec = mm_set1_ps(scy)
    ra_vec = mm_set1_ps(ra)
    rb_vec = mm_set1_ps(rb)
  
  # p.x = abs(p.x) - take absolute value of x coordinate
  let 
    neg_px = mm_sub_ps(zero, px)
    px_abs = mm_max_ps(px, neg_px)
  
  # Calculate the condition: sc.y * px_abs > sc.x * py
  let
    left_side = mm_mul_ps(scy_vec, px_abs)   # sc.y * px_abs
    right_side = mm_mul_ps(scx_vec, py)      # sc.x * py
    condition = mm_cmpgt_ps(left_side, right_side)  # left > right
  
  # Calculate length(p_mod) = sqrt(px_abs^2 + py^2) for both cases
  let
    px_abs_sq = mm_mul_ps(px_abs, px_abs)
    py_sq = mm_mul_ps(py, py)
    length_p_sq = mm_add_ps(px_abs_sq, py_sq)
    length_p = mm_sqrt_ps(length_p_sq)
  
  # Case 1 (condition true): length(p_mod - sc*ra) - rb
  # Calculate sc * ra
  let
    sc_ra_x = mm_mul_ps(scx_vec, ra_vec)  # sc.x * ra
    sc_ra_y = mm_mul_ps(scy_vec, ra_vec)  # sc.y * ra
  
  # Calculate p_mod - sc*ra
  let
    diff_x = mm_sub_ps(px_abs, sc_ra_x)   # px_abs - sc.x*ra
    diff_y = mm_sub_ps(py, sc_ra_y)       # py - sc.y*ra
  
  # Calculate length(p_mod - sc*ra)
  let
    diff_x_sq = mm_mul_ps(diff_x, diff_x)
    diff_y_sq = mm_mul_ps(diff_y, diff_y)
    diff_length_sq = mm_add_ps(diff_x_sq, diff_y_sq)
    diff_length = mm_sqrt_ps(diff_length_sq)
  
  let case1_result = mm_sub_ps(diff_length, rb_vec)  # length(p_mod - sc*ra) - rb
  
  # Case 2 (condition false): abs(length(p_mod) - ra) - rb
  let
    length_minus_ra = mm_sub_ps(length_p, ra_vec)    # length(p_mod) - ra
    neg_length_minus_ra = mm_sub_ps(zero, length_minus_ra)
    abs_length_minus_ra = mm_max_ps(length_minus_ra, neg_length_minus_ra) # abs(length(p_mod) - ra)
    case2_result = mm_sub_ps(abs_length_minus_ra, rb_vec)  # abs(length(p_mod) - ra) - rb
  
  # Select result based on condition: condition ? case1_result : case2_result
  result = mm_or_ps(mm_and_ps(condition, case1_result), mm_andnot_ps(condition, case2_result))

proc sdParallelogramSimd*(px, py: M128, wi, he, sk: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for parallelogram
  ## Processes 4 pixels at once using pure SSE2 SIMD operations
  
  let
    zero = mm_setzero_ps()
    one = mm_set1_ps(1.0)
    neg_one = mm_set1_ps(-1.0)
    wi_vec = mm_set1_ps(wi)
    he_vec = mm_set1_ps(he)
    sk_vec = mm_set1_ps(sk)
    neg_wi_vec = mm_set1_ps(-wi)
  
  # let e = vec2(sk, he)
  let ex = sk_vec
  let ey = he_vec
  
  # var p = if p.y < 0.0: -p else: p
  let
    py_lt_zero = mm_cmplt_ps(py, zero)  # py < 0.0
    neg_px = mm_sub_ps(zero, px)
    neg_py = mm_sub_ps(zero, py)
    p_x = mm_or_ps(mm_and_ps(py_lt_zero, neg_px), mm_andnot_ps(py_lt_zero, px))  # if py < 0: -px else: px
    p_y = mm_or_ps(mm_and_ps(py_lt_zero, neg_py), mm_andnot_ps(py_lt_zero, py))  # if py < 0: -py else: py
  
  # var w = p - e
  let
    w_x = mm_sub_ps(p_x, ex)  # p.x - e.x
    w_y = mm_sub_ps(p_y, ey)  # p.y - e.y
  
  # w.x -= clamp(w.x, -wi, wi)
  let
    clamped_wx = mm_max_ps(mm_min_ps(w_x, wi_vec), neg_wi_vec)  # clamp(w.x, -wi, wi)
    final_w_x = mm_sub_ps(w_x, clamped_wx)  # w.x - clamp(w.x, -wi, wi)
  
  # var d = vec2(dot(w, w), -w.y)
  let
    w_dot_w = mm_add_ps(mm_mul_ps(final_w_x, final_w_x), mm_mul_ps(w_y, w_y))  # w.x^2 + w.y^2
    d_x = w_dot_w  # dot(w, w)
    d_y = mm_sub_ps(zero, w_y)  # -w.y
  
  # let s = p.x * e.y - p.y * e.x
  let s = mm_sub_ps(mm_mul_ps(p_x, ey), mm_mul_ps(p_y, ex))  # p.x * e.y - p.y * e.x
  
  # p = if s < 0.0: -p else: p
  let
    s_lt_zero = mm_cmplt_ps(s, zero)  # s < 0.0
    neg_pp_x = mm_sub_ps(zero, p_x)
    neg_pp_y = mm_sub_ps(zero, p_y)
    final_p_x = mm_or_ps(mm_and_ps(s_lt_zero, neg_pp_x), mm_andnot_ps(s_lt_zero, p_x))  # if s < 0: -p.x else: p.x
    final_p_y = mm_or_ps(mm_and_ps(s_lt_zero, neg_pp_y), mm_andnot_ps(s_lt_zero, p_y))  # if s < 0: -p.y else: p.y
  
  # var v = p - vec2(wi, 0.0)
  let
    v_x = mm_sub_ps(final_p_x, wi_vec)  # p.x - wi
    v_y = final_p_y  # p.y - 0.0 = p.y
  
  # dot(v, e) / dot(e, e)
  let
    v_dot_e = mm_add_ps(mm_mul_ps(v_x, ex), mm_mul_ps(v_y, ey))  # v.x * e.x + v.y * e.y
    e_dot_e = mm_add_ps(mm_mul_ps(ex, ex), mm_mul_ps(ey, ey))  # e.x^2 + e.y^2
    ratio = mm_div_ps(v_dot_e, e_dot_e)
  
  # clamp(dot(v,e)/dot(e,e), -1.0, 1.0)
  let clamped_ratio = mm_max_ps(mm_min_ps(ratio, one), neg_one)  # clamp(ratio, -1.0, 1.0)
  
  # v -= e * clamp(dot(v,e)/dot(e,e), -1.0, 1.0)
  let
    e_scaled_x = mm_mul_ps(ex, clamped_ratio)  # e.x * clamped_ratio
    e_scaled_y = mm_mul_ps(ey, clamped_ratio)  # e.y * clamped_ratio
    final_v_x = mm_sub_ps(v_x, e_scaled_x)  # v.x - e.x * clamped_ratio
    final_v_y = mm_sub_ps(v_y, e_scaled_y)  # v.y - e.y * clamped_ratio
  
  # dot(v, v)
  let v_dot_v = mm_add_ps(mm_mul_ps(final_v_x, final_v_x), mm_mul_ps(final_v_y, final_v_y))
  
  # wi * he - abs(s)
  let
    wi_times_he = mm_mul_ps(wi_vec, he_vec)  # wi * he
    neg_s = mm_sub_ps(zero, s)
    abs_s = mm_max_ps(s, neg_s)  # abs(s)
    wi_he_minus_abs_s = mm_sub_ps(wi_times_he, abs_s)  # wi * he - abs(s)
  
  # d = min(d, vec2(dot(v,v), wi*he-abs(s)))
  let
    final_d_x = mm_min_ps(d_x, v_dot_v)  # min(d.x, dot(v,v))
    final_d_y = mm_min_ps(d_y, wi_he_minus_abs_s)  # min(d.y, wi*he-abs(s))
  
  # sqrt(d.x) * sign(-d.y)
  # Calculate sqrt(d.x)
  let sqrt_dx = mm_sqrt_ps(final_d_x)
  
  # Calculate sign(-d.y)
  let
    neg_d_y = mm_sub_ps(zero, final_d_y)  # -d.y
    # sign function: returns 1.0 if x > 0, -1.0 if x < 0, 0.0 if x == 0
    # Using comparison and selection
    pos_mask = mm_cmpgt_ps(neg_d_y, zero)  # neg_d_y > 0
    neg_mask = mm_cmplt_ps(neg_d_y, zero)  # neg_d_y < 0
    sign_val = mm_or_ps(mm_and_ps(pos_mask, one), mm_or_ps(mm_and_ps(neg_mask, neg_one), mm_andnot_ps(mm_or_ps(pos_mask, neg_mask), zero)))
  
  # Return sqrt(d.x) * sign(-d.y)
  result = mm_mul_ps(sqrt_dx, sign_val)

proc sdPieSimd*(px, py: M128, cx, cy, r: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for pie slice
  ## Processes 4 pixels at once using pure SSE2 SIMD operations
  
  let
    zero = mm_setzero_ps()
    one = mm_set1_ps(1.0)
    neg_one = mm_set1_ps(-1.0)
    cx_vec = mm_set1_ps(cx)
    cy_vec = mm_set1_ps(cy)
    r_vec = mm_set1_ps(r)
  
  # p.x = abs(p.x) - take absolute value of x coordinate
  let 
    neg_px = mm_sub_ps(zero, px)
    px_abs = mm_max_ps(px, neg_px)
  
  # Calculate l = length(p) - r
  let
    px_abs_sq = mm_mul_ps(px_abs, px_abs)
    py_sq = mm_mul_ps(py, py)
    length_p_sq = mm_add_ps(px_abs_sq, py_sq)
    length_p = mm_sqrt_ps(length_p_sq)
  
  let l = mm_sub_ps(length_p, r_vec)  # length(p) - r
  
  # Calculate m = length(p - c*clamp(dot(p,c), 0.0, r))
  # First calculate dot(p, c)
  let
    dot_pc = mm_add_ps(mm_mul_ps(px_abs, cx_vec), mm_mul_ps(py, cy_vec))  # px_abs * cx + py * cy
  
  # clamp(dot_pc, 0.0, r)
  let clamped_dot = mm_max_ps(mm_min_ps(dot_pc, r_vec), zero)  # clamp(dot_pc, 0.0, r)
  
  # Calculate c * clamped_dot
  let
    c_scaled_x = mm_mul_ps(cx_vec, clamped_dot)  # cx * clamped_dot
    c_scaled_y = mm_mul_ps(cy_vec, clamped_dot)  # cy * clamped_dot
  
  # Calculate p - c*clamped_dot
  let
    diff_x = mm_sub_ps(px_abs, c_scaled_x)  # px_abs - cx*clamped_dot
    diff_y = mm_sub_ps(py, c_scaled_y)      # py - cy*clamped_dot
  
  # Calculate length(p - c*clamped_dot)
  let
    diff_x_sq = mm_mul_ps(diff_x, diff_x)
    diff_y_sq = mm_mul_ps(diff_y, diff_y)
    diff_length_sq = mm_add_ps(diff_x_sq, diff_y_sq)
    m = mm_sqrt_ps(diff_length_sq)
  
  # Calculate sign(c.y*p.x - c.x*p.y)
  let
    sign_calc = mm_sub_ps(mm_mul_ps(cy_vec, px_abs), mm_mul_ps(cx_vec, py))  # cy*px_abs - cx*py
    # sign function: returns 1.0 if x > 0, -1.0 if x < 0, 0.0 if x == 0
    pos_mask = mm_cmpgt_ps(sign_calc, zero)  # sign_calc > 0
    neg_mask = mm_cmplt_ps(sign_calc, zero)  # sign_calc < 0
    sign_val = mm_or_ps(mm_and_ps(pos_mask, one), mm_or_ps(mm_and_ps(neg_mask, neg_one), mm_andnot_ps(mm_or_ps(pos_mask, neg_mask), zero)))
  
  # Calculate m * sign_val
  let m_signed = mm_mul_ps(m, sign_val)
  
  # Return max(l, m*sign_val)
  result = mm_max_ps(l, m_signed)

proc sdRingSimd*(px, py: M128, nx, ny, r, th: float32): M128 {.inline, raises: [].} =
  ## SSE2 SIMD version of signed distance function for ring
  ## Processes 4 pixels at once using pure SSE2 SIMD operations
  
  let
    zero = mm_setzero_ps()
    one = mm_set1_ps(1.0)
    neg_one = mm_set1_ps(-1.0)
    nx_vec = mm_set1_ps(nx)
    ny_vec = mm_set1_ps(ny)
    neg_ny_vec = mm_set1_ps(-ny)
    r_vec = mm_set1_ps(r)
    th_vec = mm_set1_ps(th)
    th_half = mm_set1_ps(th * 0.5)
  
  # p.x = abs(p.x)
  let 
    neg_px = mm_sub_ps(zero, px)
    px_abs = mm_max_ps(px, neg_px)
  
  # Apply 2x2 rotation matrix: mat2x2(n.x,n.y,-n.y,n.x) * p
  # rotated_p.x = n.x * px_abs + n.y * py
  # rotated_p.y = -n.y * px_abs + n.x * py
  let
    rotated_px = mm_add_ps(mm_mul_ps(nx_vec, px_abs), mm_mul_ps(ny_vec, py))
    rotated_py = mm_add_ps(mm_mul_ps(neg_ny_vec, px_abs), mm_mul_ps(nx_vec, py))
  
  # Calculate length(rotated_p) = sqrt(rotated_px^2 + rotated_py^2)
  let
    rotated_px_sq = mm_mul_ps(rotated_px, rotated_px)
    rotated_py_sq = mm_mul_ps(rotated_py, rotated_py)
    length_rotated_sq = mm_add_ps(rotated_px_sq, rotated_py_sq)
    length_rotated = mm_sqrt_ps(length_rotated_sq)
  
  # Calculate d1 = abs(length(rotated_p) - r) - th*0.5
  let
    length_minus_r = mm_sub_ps(length_rotated, r_vec)
    neg_length_minus_r = mm_sub_ps(zero, length_minus_r)
    abs_length_minus_r = mm_max_ps(length_minus_r, neg_length_minus_r)
    d1 = mm_sub_ps(abs_length_minus_r, th_half)
  
  # Calculate d2 = length(vec2(rotated_p.x, max(0.0, abs(r - rotated_p.y) - th*0.5))) * sign(rotated_p.x)
  let
    r_minus_py = mm_sub_ps(r_vec, rotated_py)  # r - rotated_p.y
    neg_r_minus_py = mm_sub_ps(zero, r_minus_py)
    abs_r_minus_py = mm_max_ps(r_minus_py, neg_r_minus_py)     # abs(r - rotated_p.y)
    abs_r_minus_py_minus_th_half = mm_sub_ps(abs_r_minus_py, th_half)  # abs(r - rotated_p.y) - th*0.5
    max_val = mm_max_ps(abs_r_minus_py_minus_th_half, zero)  # max(0.0, abs(r - rotated_p.y) - th*0.5)
  
  # Calculate length(vec2(rotated_p.x, max_val))
  let
    d2_length_sq = mm_add_ps(mm_mul_ps(rotated_px, rotated_px), mm_mul_ps(max_val, max_val))
    d2_length = mm_sqrt_ps(d2_length_sq)
  
  # Calculate sign(rotated_p.x)
  let
    pos_mask = mm_cmpgt_ps(rotated_px, zero)  # rotated_px > 0
    neg_mask = mm_cmplt_ps(rotated_px, zero)  # rotated_px < 0
    sign_rotated_px = mm_or_ps(mm_and_ps(pos_mask, one), mm_or_ps(mm_and_ps(neg_mask, neg_one), mm_andnot_ps(mm_or_ps(pos_mask, neg_mask), zero)))
  
  # d2 = d2_length * sign(rotated_p.x)
  let d2 = mm_mul_ps(d2_length, sign_rotated_px)
  
  # Return max(d1, d2)
  result = mm_max_ps(d1, d2)

when defined(release):
  {.pop.}
