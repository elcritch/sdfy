import std/math, std/monotimes, std/times

import pixie

import sdfy

template timeIt(name: string, body: untyped) =
  let start = getMonoTime()
  body
  let stop = getMonoTime()
  echo name, ": ", inMilliseconds(stop - start), " ms"

let
  image* = newImage(300, 300)
  center* = vec2(image.width / 2, image.height / 2)
  pos = rgba(255, 0, 0, 255)
  neg = rgba(0, 0, 255, 255)
  corners* = vec4(0.0, 20.0, 40.0, 80.0)
  wh* = vec2(200.0, 200.0)

# Define test modes
let testModes* = [
  (mode: sdfModeClip, name: "clip", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeClipAA, name: "clip_aa", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeAnnular, name: "annular", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeAnnularAA, name: "annular_aa", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeFeather, name: "feather", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeFeatherInv, name: "feather_inv", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeFeatherGaussian, name: "feather_gaussian", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeDropShadow, name: "drop_shadow", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
  (mode: sdfModeInsetShadow, name: "inset_shadow", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
  (mode: sdfModeInsetShadowAnnular, name: "inset_shadow_annular", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
]


proc main() =

  timeIt "base":
    let rect = newImage(300, 300)
    let ctx = newContext(rect)
    ctx.fillStyle = pos
    ctx.fillRoundedRect(rect(center - wh/2, wh), 20.0)
    let shadow = rect.shadow(
      offset = vec2(0, 0),
      spread = 20.0,
      blur = 10.0,
      color = neg
      )
    
    image.draw(shadow)
    image.draw(rect)

  image.writeFile("tests/outputs/rounded_box_pixie.png")

  # Test rounded boxes
  for testMode in testModes:
    let testName = "rounded - " & testMode.name
    let fileName = "tests/outputs/rounded_box_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,
                  params = RoundedBoxParams(r: corners),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test chamfer boxes
  for testMode in testModes:
    let testName = "chamfer - " & testMode.name
    let fileName = "tests/outputs/chamfer_box_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,
                  params = ChamferBoxParams(chamfer: 20.0),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test circles
  for testMode in testModes:
    let testName = "circle - " & testMode.name
    let fileName = "tests/outputs/circle_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for circles, but kept for API consistency
                  params = CircleParams(r: 100.0),  # 100 pixel radius
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test Bézier curves
  for testMode in testModes:
    let testName = "bezier - " & testMode.name
    let fileName = "tests/outputs/bezier_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for Bézier curves, but kept for API consistency
                  params = BezierParams(
                    A: vec2(50.0, 100.0),   # Start point (relative to center)
                    B: vec2(0.0, -50.0),    # Control point
                    C: vec2(-50.0, 100.0)   # End point
                  ),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test boxes
  for testMode in testModes:
    let testName = "box - " & testMode.name
    let fileName = "tests/outputs/box_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for boxes since we specify half-extents directly
                  params = BoxParams(b: vec2(80.0, 60.0)),  # 160x120 pixel box
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test ellipses
  for testMode in testModes:
    let testName = "ellipse - " & testMode.name
    let fileName = "tests/outputs/ellipse_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for ellipses since we specify semi-axes directly
                  params = EllipseParams(ab: vec2(90.0, 60.0)),  # 180x120 pixel ellipse
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test arcs
  for testMode in testModes:
    let testName = "arc - " & testMode.name
    let fileName = "tests/outputs/arc_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for arcs, but kept for API consistency
                  params = ArcParams(
                    sc: vec2(sin(PI/4), cos(PI/4)),  # 45 degree aperture
                    ra: 80.0,   # inner radius
                    rb: 20.0    # thickness
                  ),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test parallelograms
  for testMode in testModes:
    let testName = "parallelogram - " & testMode.name
    let fileName = "tests/outputs/parallelogram_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for parallelograms, but kept for API consistency
                  params = ParallelogramParams(
                    wi: 80.0,   # width
                    he: 60.0,   # height
                    sk: 20.0    # skew
                  ),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test pies
  for testMode in testModes:
    let testName = "pie - " & testMode.name
    let fileName = "tests/outputs/pie_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for pies, but kept for API consistency
                  params = PieParams(
                    c: vec2(sin(PI/3), cos(PI/3)),  # 60 degree aperture
                    r: 80.0   # radius
                  ),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

  # Test rings
  for testMode in testModes:
    let testName = "ring - " & testMode.name
    let fileName = "tests/outputs/ring_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShape(image,
                  center = center,
                  wh = wh,  # wh is ignored for rings, but kept for API consistency
                  params = RingParams(
                    n: vec2(sin(PI/4), cos(PI/4)),  # 45 degree aperture
                    r: 80.0,   # radius
                    th: 20.0   # thickness
                  ),
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

for i in 0 ..< 1:
  main()
