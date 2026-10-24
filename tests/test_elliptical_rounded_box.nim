import std/[math, random, unittest]

import pixie, vmath
import sdfy

proc closeTo(a, b: float32): bool =
  abs(a - b) < 0.00002'f32

when allowSimd:
  when defined(amd64):
    import nimsimd/sse2
    proc distances(
        xs, ys: var array[4, float32], bounds: Vec2, rx, ry: Vec4
    ): array[4, float32] =
      let value = sdEllipticalRoundedBoxSimd(
        mm_loadu_ps(xs[0].addr), mm_loadu_ps(ys[0].addr), bounds.x, bounds.y, rx, ry
      )
      mm_storeu_ps(result[0].addr, value)

  elif defined(arm64) or defined(aarch64) or defined(arm):
    import nimsimd/neon
    proc distances(
        xs, ys: var array[4, float32], bounds: Vec2, rx, ry: Vec4
    ): array[4, float32] =
      let value = sdEllipticalRoundedBoxSimd(
        vld1q_f32(xs[0].addr), vld1q_f32(ys[0].addr), bounds.x, bounds.y, rx, ry
      )
      vst1q_f32(result[0].addr, value)

suite "Elliptical rounded boxes":
  when allowSimd:
    test "SIMD lanes preserve mixed circular elliptical and sharp corners":
      for bounds in [vec2(10, 8), vec2(0, 8), vec2(0, 0), vec2(-3, 8)]:
        for axes in [
          (vec4(2, 0, 40, 6), vec4(2, 3, 40, 3)),
          (vec4(40, -2, 0, 7), vec4(30, 5, -4, 7)),
          (vec4(0.001, 3, 4, 5), vec4(6, 0.002, 8, 2)),
        ]:
          for y in -12 .. 12:
            for x in countup(-16, 16, 4):
              var xs = [x.float32, (x + 1).float32, (x + 2).float32, (x + 3).float32]
              var ys =
                [y.float32, -y.float32, y.float32 + 0.25'f32, -y.float32 - 0.5'f32]
              let actual = distances(xs, ys, bounds, axes[0], axes[1])
              for lane in 0 ..< 4:
                let expected = sdEllipticalRoundedBox(
                  vec2(xs[lane], ys[lane]), bounds, axes[0], axes[1]
                )
                check classify(actual[lane]) notin {fcNan, fcInf, fcNegInf}
                check abs(actual[lane] - expected) <=
                  0.00002'f32 + abs(expected) * 0.000002'f32

    test "SIMD agrees with scalar on deterministic randomized fields":
      var rng = initRand(48193)
      for trial in 0 ..< 2000:
        let bounds = vec2(rand(rng, 50.0).float32, rand(rng, 50.0).float32)
        var rx, ry: Vec4
        var xs, ys: array[4, float32]
        for lane in 0 ..< 4:
          rx[lane] = rand(rng, 90.0).float32 - 10
          ry[lane] =
            if trial mod 3 == 0:
              rx[lane]
            else:
              rand(rng, 90.0).float32 - 10
          xs[lane] = rand(rng, 160.0).float32 - 80
          ys[lane] = rand(rng, 160.0).float32 - 80
        let actual = distances(xs, ys, bounds, rx, ry)
        for lane in 0 ..< 4:
          let expected =
            sdEllipticalRoundedBox(vec2(xs[lane], ys[lane]), bounds, rx, ry)
          check classify(actual[lane]) notin {fcNan, fcInf, fcNegInf}
          check abs(actual[lane] - expected) <=
            0.00002'f32 + abs(expected) * 0.000002'f32

    test "SIMD preserves the near-center epsilon branch and quadrant boundaries":
      var xs = [0'f32, -0'f32, 1e-9'f32, 2e-9'f32]
      var ys = [0'f32, 1e-9'f32, 0'f32, 2e-9'f32]
      let bounds = vec2(0.006, 0.003)
      let actual = distances(xs, ys, bounds, vec4(0.006), vec4(0.003))
      for lane in 0 ..< 4:
        check closeTo(
          actual[lane],
          sdEllipticalRoundedBox(
            vec2(xs[lane], ys[lane]), bounds, vec4(0.006), vec4(0.003)
          ),
        )

  test "equal axes retain the circular distance field":
    let
      bounds = vec2(10, 8)
      radii = vec4(2, 5, 4, 7)
    for y in -12 .. 12:
      for x in -14 .. 14:
        let p = vec2(x.float32 + 0.2'f32, y.float32 + 0.3'f32)
        check closeTo(
          sdEllipticalRoundedBox(p, bounds, radii, radii),
          sdRoundedBox(p, bounds, radii),
        )

  test "each Vec4 component selects the same quadrant as circular corners":
    let bounds = vec2(10, 8)
    let signs = [vec2(1, 1), vec2(1, -1), vec2(-1, 1), vec2(-1, -1)]
    for corner in 0 ..< 4:
      var rx, ry: Vec4
      rx[corner] = 6
      ry[corner] = 3
      for probe in 0 ..< 4:
        let distance =
          sdEllipticalRoundedBox(signs[probe] * vec2(9.5, 7.5), bounds, rx, ry)
        if probe == corner:
          check distance > 0
        else:
          check distance < 0

  test "elliptical quarter arcs have the expected zero contour":
    let
      bounds = vec2(10, 8)
      axes = vec2(6, 3)
      signs = [vec2(1, 1), vec2(1, -1), vec2(-1, 1), vec2(-1, -1)]
    for quadrant in signs:
      for step in 1 .. 9:
        let
          angle = step.float32 * PI.float32 / 20
          local = axes * vec2(cos(angle), sin(angle))
          p = quadrant * (bounds - axes + local)
        check closeTo(sdEllipticalRoundedBox(p, bounds, vec4(6), vec4(3)), 0)

  test "zero or negative axes make a sharp corner":
    for axes in [vec2(0, 3), vec2(6, 0), vec2(-2, 3), vec2(6, -2)]:
      for p in [vec2(0, 0), vec2(9, 7), vec2(12, 9)]:
        check closeTo(
          sdEllipticalRoundedBox(p, vec2(10, 8), vec4(axes.x), vec4(axes.y)),
          sdBox(p, vec2(10, 8)),
        )

  test "oversized axes clamp and equal inputs remain circular":
    let bounds = vec2(10, 8)
    for p in [vec2(0, 0), vec2(9, 7), vec2(12, 9)]:
      check closeTo(
        sdEllipticalRoundedBox(p, bounds, vec4(40), vec4(30)),
        sdEllipticalRoundedBox(p, bounds, vec4(10), vec4(8)),
      )
      check closeTo(
        sdEllipticalRoundedBox(p, bounds, vec4(40), vec4(40)),
        sdRoundedBox(p, bounds, vec4(8)),
      )

  test "distance stays finite at axes, centers, and degenerate bounds":
    for bounds in [vec2(10, 8), vec2(0, 8), vec2(0, 0)]:
      for p in [vec2(0, 0), vec2(4, 5), vec2(10, 5), vec2(4, 8)]:
        let d = sdEllipticalRoundedBox(p, bounds, vec4(6), vec4(3))
        check classify(d) notin {fcNan, fcInf, fcNegInf}

  test "raster dispatch handles odd-width images and agrees with scalar coverage":
    let
      params = EllipticalRoundedBoxParams(rx: vec4(8, 5, 7, 4), ry: vec4(3, 6, 2, 5))
      foreground = rgba(255, 255, 255, 255)
      background = rgba(0, 0, 0, 0)
    for mode in [sdfModeClip, sdfModeClipAA]:
      let
        scalar = newImage(37, 29)
        dispatched = newImage(37, 29)
      drawSdfShapeNonSimd(
        scalar, vec2(18.5, 14.5), vec2(28, 20), params, foreground, background, mode
      )
      drawSdfShape(
        dispatched, vec2(18.5, 14.5), vec2(28, 20), params, foreground, background, mode
      )
      check dispatched[18, 14].a == 255
      check dispatched[0, 0].a == 0
      for i in 0 ..< scalar.data.len:
        check abs(scalar.data[i].a.int - dispatched.data[i].a.int) <= 1

  test "elliptical corners support stroke and shadow modes":
    for mode in [sdfModeAnnularAA, sdfModeDropShadowAA, sdfModeInsetShadowAnnular]:
      let image = newImage(37, 29)
      drawSdfShape(
        image,
        vec2(18.5, 14.5),
        vec2(24, 16),
        EllipticalRoundedBoxParams(rx: vec4(8), ry: vec4(3)),
        rgba(255, 255, 255, 255),
        rgba(0, 0, 0, 0),
        mode,
        factor = 3,
      )
      var painted = false
      for pixel in image.data:
        painted = painted or pixel.a > 0
      check painted
