import std/[math, random, unittest]
import vmath
import sdfy

when allowSimd:
  when defined(amd64):
    import nimsimd/sse2
    proc distances(
        xs, ys: var array[4, float32], bounds: Vec2, radii: Vec4
    ): array[4, float32] =
      let value = sdRoundedBoxSimd(
        mm_loadu_ps(xs[0].addr), mm_loadu_ps(ys[0].addr), bounds.x, bounds.y, radii
      )
      mm_storeu_ps(result[0].addr, value)

  elif defined(arm64) or defined(aarch64) or defined(arm):
    import nimsimd/neon
    proc distances(
        xs, ys: var array[4, float32], bounds: Vec2, radii: Vec4
    ): array[4, float32] =
      let value = sdRoundedBoxSimd(
        vld1q_f32(xs[0].addr), vld1q_f32(ys[0].addr), bounds.x, bounds.y, radii
      )
      vst1q_f32(result[0].addr, value)

  proc checkPacket(xs, ys: var array[4, float32], bounds: Vec2, radii: Vec4) =
    let actual = distances(xs, ys, bounds, radii)
    for lane in 0 ..< 4:
      let expected = sdRoundedBox(vec2(xs[lane], ys[lane]), bounds, radii)
      check classify(actual[lane]) notin {fcNan, fcInf, fcNegInf}
      check abs(actual[lane] - expected) <= 0.00002'f32 + abs(expected) * 0.000002'f32

  suite "Circular rounded-box SIMD":
    test "preserves quadrants and signed zero with per-corner radii":
      for bounds in [vec2(10, 8), vec2(0, 8), vec2(0), vec2(-3, 8)]:
        for radii in [vec4(0), vec4(3), vec4(2, 0, 40, -3)]:
          var xs = [0'f32, -0'f32, 1e-9'f32, -1e-9'f32]
          var ys = [-0'f32, 0'f32, -1e-9'f32, 1e-9'f32]
          checkPacket(xs, ys, bounds, radii)
          for y in -12 .. 12:
            for x in countup(-16, 16, 4):
              xs = [x.float32, (x + 1).float32, (x + 2).float32, (x + 3).float32]
              ys = [y.float32, -y.float32, y.float32 + 0.25'f32, -y.float32 - 0.5'f32]
              checkPacket(xs, ys, bounds, radii)

    test "agrees with scalar on deterministic randomized packets":
      var rng = initRand(62819)
      for trial in 0 ..< 2000:
        let bounds = vec2(rand(rng, 50.0).float32, rand(rng, 50.0).float32)
        var radii: Vec4
        var xs, ys: array[4, float32]
        for lane in 0 ..< 4:
          radii[lane] = rand(rng, 90.0).float32 - 10
          xs[lane] = rand(rng, 160.0).float32 - 80
          ys[lane] = rand(rng, 160.0).float32 - 80
        checkPacket(xs, ys, bounds, radii)
