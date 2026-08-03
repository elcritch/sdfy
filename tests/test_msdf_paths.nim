import std/unittest

include ../src/sdfy/msdfgen

const epsilon = 0.000001

proc closeTo(a, b: DVec2): bool =
  abs(a.x - b.x) <= epsilon and abs(a.y - b.y) <= epsilon

suite "MSDF path conversion":
  test "absolute SVG arcs become cubic edges":
    let shape = shapeFromPath(parsePath("M 10 20 A 30 15 0 0 1 70 20"))

    check shape.contours.len == 1
    check shape.contours[0].edges.len == 3
    check shape.contours[0].edges[0].kind == esCubic
    check shape.contours[0].edges[1].kind == esCubic
    check closeTo(shape.contours[0].edges[0].p[0], dvec2(10, 20))
    check closeTo(shape.contours[0].edges[0].p[3], dvec2(40, 5))
    check closeTo(shape.contours[0].edges[1].p[3], dvec2(70, 20))

  test "relative rotated SVG arcs preserve their endpoint":
    let shape = shapeFromPath(parsePath("M 4 7 a 40 20 35 1 0 31 13"))

    check shape.contours.len == 1
    check shape.contours[0].edges.len >= 4
    check closeTo(shape.contours[0].edges[^2].p[3], dvec2(35, 20))

  test "SVG arc radii expand to reach the endpoint":
    let shape = shapeFromPath(parsePath("M 0 0 A 10 10 0 0 1 100 0"))

    check shape.contours.len == 1
    check shape.contours[0].edges.len == 3
    check closeTo(shape.contours[0].edges[0].p[3], dvec2(50, -50))
    check closeTo(shape.contours[0].edges[1].p[3], dvec2(100, 0))

  test "zero-radius SVG arcs become lines":
    let shape = shapeFromPath(parsePath("M 1 2 A 0 5 45 0 1 8 9"))

    check shape.contours.len == 1
    check shape.contours[0].edges[0].kind == esLine
    check closeTo(shape.contours[0].edges[0].p[1], dvec2(8, 9))

  test "unsupported commands fail when parsing makes no progress":
    expect PixieError:
      discard shapeFromPathString("M 0 0 X 1 1")
