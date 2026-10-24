import std/[math, unittest]

import pixie, vmath
import sdfy

proc closeTo(a, b: float32): bool =
  abs(a - b) < 0.00002'f32

suite "Elliptical rounded boxes":
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
