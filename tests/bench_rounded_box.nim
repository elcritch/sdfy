## nim r -d:release tests/bench_rounded_box.nim
## Circular rounded boxes versus elliptical boxes with equal/unequal axes.
## Includes the previous circular SIMD implementation as a baseline.
## Each sample evaluates 16,384,000 points at the default repetition count;
## reports the minimum of five samples and alternates measurement order.
import std/[math, monotimes, strformat, times]
import vmath
import sdfy

when defined(amd64):
  import nimsimd/sse2
  type Vector = M128
  proc load(p: ptr float32): Vector {.inline.} =
    mm_loadu_ps(p)

  proc store(p: ptr float32, value: Vector) {.inline.} =
    mm_storeu_ps(p, value)

  proc zero(): Vector {.inline.} =
    mm_setzero_ps()

  proc add(a, b: Vector): Vector {.inline.} =
    mm_add_ps(a, b)

elif defined(arm64) or defined(aarch64) or defined(arm):
  import nimsimd/neon
  type Vector = float32x4
  proc load(p: ptr float32): Vector {.inline.} =
    vld1q_f32(p)

  proc store(p: ptr float32, value: Vector) {.inline.} =
    vst1q_f32(p, value)

  proc zero(): Vector {.inline.} =
    vmovq_n_f32(0)

  proc add(a, b: Vector): Vector {.inline.} =
    vaddq_f32(a, b)

else:
  {.error: "This benchmark requires SSE2 or NEON".}

when defined(amd64):
  proc previousRoundedBoxSimd(
      px, py: M128, bx, by: float32, r: Vec4
  ): M128 {.inline, raises: [].} =
    let
      zero = mm_setzero_ps()
      rx_xy = mm_set1_ps(r.x) # top-right radius
      ry_xy = mm_set1_ps(r.y) # bottom-right radius
      rz_zw = mm_set1_ps(r.z) # bottom-left radius
      rw_zw = mm_set1_ps(r.w) # top-left radius
      bx_vec = mm_set1_ps(bx)
      by_vec = mm_set1_ps(by)
    let
      px_pos = mm_cmpgt_ps(px, zero) # px > 0
      py_pos = mm_cmpgt_ps(py, zero) # py > 0
    let
      radius_x = mm_or_ps(mm_and_ps(px_pos, rx_xy), mm_andnot_ps(px_pos, rz_zw))
      radius_y = mm_or_ps(mm_and_ps(px_pos, ry_xy), mm_andnot_ps(px_pos, rw_zw))
    let corner_radius =
      mm_or_ps(mm_and_ps(py_pos, radius_x), mm_andnot_ps(py_pos, radius_y))
    let
      neg_px = mm_sub_ps(zero, px)
      neg_py = mm_sub_ps(zero, py)
      abs_px = mm_max_ps(px, neg_px)
      abs_py = mm_max_ps(py, neg_py)
    let
      qx = mm_add_ps(mm_sub_ps(abs_px, bx_vec), corner_radius)
      qy = mm_add_ps(mm_sub_ps(abs_py, by_vec), corner_radius)
    let
      max_qx = mm_max_ps(qx, zero)
      max_qy = mm_max_ps(qy, zero)
    let
      max_qx_sq = mm_mul_ps(max_qx, max_qx)
      max_qy_sq = mm_mul_ps(max_qy, max_qy)
      length_sq = mm_add_ps(max_qx_sq, max_qy_sq)
      length_vec = mm_sqrt_ps(length_sq)
    let
      max_q = mm_max_ps(qx, qy)
      min_max_q = mm_min_ps(max_q, zero)
    result = mm_sub_ps(mm_add_ps(min_max_q, length_vec), corner_radius)

elif defined(arm64) or defined(aarch64) or defined(arm):
  when not compiles(vabsq_f32(float32x4(0))):
    func vabsq_f32(a: float32x4): float32x4 {.header: "arm_neon.h".}
  when not compiles(vsqrtq_f32(float32x4(0))):
    func vsqrtq_f32(a: float32x4): float32x4 {.header: "arm_neon.h".}
  proc previousRoundedBoxSimd(
      px, py: float32x4, bx, by: float32, r: Vec4
  ): float32x4 {.inline, raises: [].} =
    let
      zero = vmovq_n_f32(0.0)
      rx_xy = vmovq_n_f32(r.x) # top-right radius
      ry_xy = vmovq_n_f32(r.y) # bottom-right radius
      rz_zw = vmovq_n_f32(r.z) # bottom-left radius
      rw_zw = vmovq_n_f32(r.w) # top-left radius
      bx_vec = vmovq_n_f32(bx)
      by_vec = vmovq_n_f32(by)
    let
      px_pos = vcgtq_f32(px, zero) # px > 0
      py_pos = vcgtq_f32(py, zero) # py > 0
    let radius_x = vbslq_f32(px_pos, rx_xy, rz_zw)
    let radius_y = vbslq_f32(px_pos, ry_xy, rw_zw)
    let corner_radius = vbslq_f32(py_pos, radius_x, radius_y)
    let
      abs_px = vabsq_f32(px)
      abs_py = vabsq_f32(py)
      qx = vaddq_f32(vsubq_f32(abs_px, bx_vec), corner_radius)
      qy = vaddq_f32(vsubq_f32(abs_py, by_vec), corner_radius)
    let
      max_qx = vmaxq_f32(qx, zero)
      max_qy = vmaxq_f32(qy, zero)
    let
      max_qx_sq = vmulq_f32(max_qx, max_qx)
      max_qy_sq = vmulq_f32(max_qy, max_qy)
      length_sq = vaddq_f32(max_qx_sq, max_qy_sq)
    when defined(arm64) or defined(aarch64):
      let length_vec = vsqrtq_f32(length_sq)
    else:
      var length_array: array[4, float32]
      vst1q_f32(length_array[0].addr, length_sq)
      for i in 0 .. 3:
        length_array[i] = sqrt(length_array[i])
      let length_vec = vld1q_f32(length_array[0].addr)
    let
      max_q = vmaxq_f32(qx, qy)
      min_max_q = vminq_f32(max_q, zero)
    result = vaddq_f32(vsubq_f32(vaddq_f32(min_max_q, length_vec), corner_radius), zero)

const Repetitions {.intdefine.} = 1000
const BatchCount = 4096

type Mode = enum
  circularScalar
  ellipticalScalar
  circularSimd
  ellipticalSimd
  previousCircularSimd

proc measure(
    mode: static Mode, xs, ys: var seq[array[4, float32]], bounds: Vec2, rx, ry: Vec4
): tuple[seconds: float64, checksum: float32] =
  var total = zero()
  var scalarTotals: array[4, float32]
  let start = getMonoTime()
  for iteration in 0 ..< Repetitions:
    for batch in 0 ..< xs.len:
      when mode in {circularScalar, ellipticalScalar}:
        for lane in 0 ..< 4:
          let point = vec2(xs[batch][lane], ys[batch][lane])
          when mode == circularScalar:
            scalarTotals[lane] += sdRoundedBox(point, bounds, rx)
          else:
            scalarTotals[lane] += sdEllipticalRoundedBox(point, bounds, rx, ry)
      else:
        let px = load(xs[batch][0].addr)
        let py = load(ys[batch][0].addr)
        let distance =
          when mode == previousCircularSimd:
            previousRoundedBoxSimd(px, py, bounds.x, bounds.y, rx)
          elif mode == circularSimd:
            sdRoundedBoxSimd(px, py, bounds.x, bounds.y, rx)
          else:
            sdEllipticalRoundedBoxSimd(px, py, bounds.x, bounds.y, rx, ry)
        total = add(total, distance)
  result.seconds = (getMonoTime() - start).inNanoseconds.float64 / 1e9
  var lanes: array[4, float32]
  when mode in {circularScalar, ellipticalScalar}:
    lanes = scalarTotals
  else:
    store(lanes[0].addr, total)
  for lane in lanes:
    result.checksum += lane

proc matching(a, b: float32) =
  doAssert abs(a - b) <= max(1'f32, abs(a)) * 0.00001'f32

for scenario in ["mixed field", "corners", "straight edges"]:
  var xs, ys = newSeq[array[4, float32]](BatchCount)
  let bounds = vec2(50, 35)
  let rx = vec4(14, 22, 9, 17)
  let ry = vec4(6, 12, 21, 4)
  for batch in 0 ..< BatchCount:
    for lane in 0 ..< 4:
      let index = batch * 4 + lane
      case scenario
      of "corners":
        xs[batch][lane] =
          (48'f32 + (index mod 16).float32 * 0.25'f32) *
          (if index mod 2 == 0: 1'f32 else: -1'f32)
        ys[batch][lane] =
          (33'f32 + (index mod 8).float32 * 0.25'f32) *
          (if batch mod 2 == 0: 1'f32 else: -1'f32)
      of "straight edges":
        xs[batch][lane] = (index mod 50).float32 - 25
        ys[batch][lane] = (batch mod 30).float32 - 15
      else:
        xs[batch][lane] = (index mod 128).float32 - 64
        ys[batch][lane] = (batch mod 96).float32 - 48
  var best: array[7, float64]
  for i in 0 ..< best.len:
    best[i] = Inf
  for trial in 0 ..< 5:
    # Alternate measurement order to reduce order-related temperature bias.
    var values: array[7, tuple[seconds: float64, checksum: float32]]
    for step in 0 ..< 7:
      let index =
        if trial mod 2 == 0:
          step
        else:
          6 - step
      case index
      of 0:
        values[index] = measure(circularScalar, xs, ys, bounds, rx, rx)
      of 1:
        values[index] = measure(ellipticalScalar, xs, ys, bounds, rx, rx)
      of 2:
        values[index] = measure(ellipticalScalar, xs, ys, bounds, rx, ry)
      of 3:
        values[index] = measure(circularSimd, xs, ys, bounds, rx, rx)
      of 4:
        values[index] = measure(ellipticalSimd, xs, ys, bounds, rx, rx)
      of 5:
        values[index] = measure(ellipticalSimd, xs, ys, bounds, rx, ry)
      else:
        values[index] = measure(previousCircularSimd, xs, ys, bounds, rx, rx)
      best[index] = min(best[index], values[index].seconds)
    matching(values[0].checksum, values[1].checksum)
    matching(values[0].checksum, values[3].checksum)
    matching(values[0].checksum, values[4].checksum)
    matching(values[2].checksum, values[5].checksum)
    matching(values[0].checksum, values[6].checksum)
  echo scenario
  echo &"  scalar: circular {best[0]*1000:.2f} ms, elliptical equal axes {best[1]*1000:.2f} ms, elliptical unequal axes {best[2]*1000:.2f} ms"
  echo &"  SIMD:   circular {best[3]*1000:.2f} ms, elliptical equal axes {best[4]*1000:.2f} ms, elliptical unequal axes {best[5]*1000:.2f} ms"
  echo &"  previous circular SIMD {best[6]*1000:.2f} ms, circular speedup {best[6]/best[3]:.2f}x"
