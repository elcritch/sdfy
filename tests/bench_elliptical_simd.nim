## nim r -d:release tests/bench_elliptical_simd.nim
## Compares the vector distance function to the former four-scalar-lane path.
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

const Repetitions {.intdefine.} = 100
const BatchCount = 4096

proc scalarLanes(px, py: Vector, bounds: Vec2, rx, ry: Vec4): Vector {.inline.} =
  var xs, ys, output: array[4, float32]
  store(xs[0].addr, px)
  store(ys[0].addr, py)
  for lane in 0 ..< 4:
    output[lane] = sdEllipticalRoundedBox(vec2(xs[lane], ys[lane]), bounds, rx, ry)
  load(output[0].addr)

proc measure(
    vectorized: static bool,
    xs, ys: var seq[array[4, float32]],
    bounds: Vec2,
    rx, ry: Vec4,
): tuple[seconds: float64, checksum: float32] =
  var total = zero()
  let start = getMonoTime()
  for iteration in 0 ..< Repetitions:
    for batch in 0 ..< xs.len:
      let px = load(xs[batch][0].addr)
      let py = load(ys[batch][0].addr)
      let distance =
        when vectorized:
          sdEllipticalRoundedBoxSimd(px, py, bounds.x, bounds.y, rx, ry)
        else:
          scalarLanes(px, py, bounds, rx, ry)
      total = add(total, distance)
  result.seconds = (getMonoTime() - start).inNanoseconds.float64 / 1e9
  var lanes: array[4, float32]
  store(lanes[0].addr, total)
  for lane in lanes:
    result.checksum += lane

for scenario in [
  "mixed field", "elliptical corners", "straight edges", "circular corners"
]:
  var xs, ys = newSeq[array[4, float32]](BatchCount)
  let bounds = vec2(50, 35)
  let rx = vec4(14, 22, 9, 17)
  let ry =
    if scenario == "circular corners":
      rx
    else:
      vec4(6, 12, 21, 4)
  for batch in 0 ..< BatchCount:
    for lane in 0 ..< 4:
      let index = batch * 4 + lane
      case scenario
      of "elliptical corners", "circular corners":
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
  var scalarSeconds, vectorSeconds = Inf
  for trial in 0 ..< 5:
    let scalar = measure(false, xs, ys, bounds, rx, ry)
    let vector = measure(true, xs, ys, bounds, rx, ry)
    doAssert abs(scalar.checksum - vector.checksum) <=
      max(1'f32, abs(scalar.checksum)) * 0.00001'f32
    scalarSeconds = min(scalarSeconds, scalar.seconds)
    vectorSeconds = min(vectorSeconds, vector.seconds)
  echo &"{scenario}: scalar lanes {scalarSeconds*1000:.2f} ms, SIMD {vectorSeconds*1000:.2f} ms, {scalarSeconds/vectorSeconds:.2f}x"
