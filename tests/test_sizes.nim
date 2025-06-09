import std/math, std/monotimes, std/times

import pixie

import sdfy

var extraInfo = ""

template timeIt(name: string, body: untyped) =
  extraInfo = ""
  let start = getMonoTime()
  body
  let stop = getMonoTime()
  echo name, ": ", inMilliseconds(stop - start), " ms", " :: ", extraInfo

var
  image* = newImage(300, 300)
  center* = vec2(image.width / 2, image.height / 2)
  pos = rgba(255, 0, 0, 255)
  neg = rgba(0, 0, 255, 255)
  corners* = vec4(0.0, 2.0, 4.0, 8.0)
  wh* = vec2(200.0, 200.0)

# 4 => 7
# 10 => 19

# Define test modes
let testModes* = [
  (mode: sdfModeClip, name: "clip", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeClipAA, name: "clip_aa", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeAnnular, name: "annular", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  (mode: sdfModeAnnularAA, name: "annular_aa", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  # (mode: sdfModeAnnular, name: "annular_fat", factor: 10.0, spread: 10.0, posColor: pos, negColor: neg),
  # (mode: sdfModeAnnular, name: "annular_fatter", factor: 20.0, spread: 10.0, posColor: pos, negColor: neg),
  # (mode: sdfModeAnnular, name: "annular_small", factor: 1.0, spread: 10.0, posColor: pos, negColor: neg),
  # (mode: sdfModeAnnular, name: "annular_small1_5", factor: 1.5, spread: 10.0, posColor: pos, negColor: neg),
  # (mode: sdfModeAnnular, name: "annular_small2", factor: 2.0, spread: 10.0, posColor: pos, negColor: neg),
  # (mode: sdfModeFeather, name: "feather", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  # (mode: sdfModeFeatherInv, name: "feather_inv", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  # (mode: sdfModeFeatherGaussian, name: "feather_gaussian", factor: 4.0, spread: 0.0, posColor: pos, negColor: neg),
  # (mode: sdfModeDropShadow, name: "drop_shadow", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
  # (mode: sdfModeInsetShadow, name: "inset_shadow", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
  # (mode: sdfModeInsetShadowAnnular, name: "inset_shadow_annular", factor: 10.0, spread: 20.0, posColor: pos, negColor: pos),
]

proc measureWidthRow*(image: Image, row: int, color: ColorRGBA): int =

  var counts = 0
  var first = -1
  var last = -1

  for x in 0 ..< image.width:
    let idx = image.dataIndex(x, row)
    let c = image.data[idx]
    let cd = c.distance(color)
    if cd < 40.0:
      counts += 1
      if first == -1:
        first = x
      last = x

  result = last - first + 1
  # echo "row: ", row, " counts: ", counts, " first: ", first, " last: ", last
  # echo "row: ", row, " counts: ", counts


proc main() =

  echo "\n### Tests for width: ", wh.x, "x", wh.y, " ###\n"

  # Test rounded boxes
  for testMode in testModes:
    let testName = "rounded - " & testMode.name & "_" & $wh.x & "x" & $wh.y
    let fileName = "tests/outputs/sizes_rounded_box_" & testName & ".png"
    
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

      let width = measureWidthRow(image, 150, testMode.posColor)
      extraInfo = "\tmeasuredWidth: " & $width & "px"
    # echo "width: ", width

    image.writeFile(fileName)

  # # Test chamfer boxes
  # for testMode in testModes:
  #   let testName = "chamfer - " & testMode.name
  #   let fileName = "tests/outputs/sizes_chamfer_box_" & testMode.name & "_wh" & $wh.x & "x" & $wh.y & ".png"
  #   timeIt testName:
  #     drawSdfShape(image,
  #                 center = center,
  #                 wh = wh,
  #                 params = ChamferBoxParams(chamfer: 20.0),
  #                 pos = testMode.posColor,
  #                 neg = testMode.negColor,
  #                 factor = testMode.factor,
  #                 spread = testMode.spread,
  #                 mode = testMode.mode)
  #   image.writeFile(fileName)

  # # Test circles
  # for testMode in testModes:
  #   let testName = "circle - " & testMode.name
  #   let fileName = "tests/outputs/sizes_circle_" & testMode.name & "_wh" & $wh.x & "x" & $wh.y & ".png"
  #   timeIt testName:
  #     drawSdfShape(image,
  #                 center = center,
  #                 wh = wh,  # wh is ignored for circles, but kept for API consistency
  #                 params = CircleParams(r: 100.0),  # 100 pixel radius
  #                 pos = testMode.posColor,
  #                 neg = testMode.negColor,
  #                 factor = testMode.factor,
  #                 spread = testMode.spread,
  #                 mode = testMode.mode)
  #   image.writeFile(fileName)

wh = vec2(20.0, 20.0)
main()

wh = vec2(50.0, 50.0)
main()

wh = vec2(100.0, 100.0)
main()

wh = vec2(200.0, 200.0)
main()
