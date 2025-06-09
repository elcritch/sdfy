include test1

proc ensureNonSimdWorks() =
  echo "run non-simd tests"

  # Test ellipses
  for testMode in testModes:
    let testName = "ellipse - " & testMode.name
    let fileName = "tests/outputs/ellipse_non_simd_" & testMode.name & ".png"
    
    timeIt testName:
      drawSdfShapeNonSimd(image,
                  center = center,
                  wh = wh,  # wh is ignored for ellipses since we specify semi-axes directly
                  params = EllipseParams(ab: vec2(90.0, 60.0)),  # 180x120 pixel ellipse
                  pos = testMode.posColor,
                  neg = testMode.negColor,
                  factor = testMode.factor,
                  spread = testMode.spread,
                  mode = testMode.mode)

    image.writeFile(fileName)

ensureNonSimdWorks()
