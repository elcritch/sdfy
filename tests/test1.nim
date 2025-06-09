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
  (mode: sdfModeAnnular, name: "annular_fat", factor: 10.0, spread: 10.0, posColor: pos, negColor: neg),
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


for i in 0 ..< 1:
  main()
