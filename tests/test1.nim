import std/math, std/monotimes, std/times

import pixie

import sdfy
import sdfy/roundedbox

template timeIt(name: string, body: untyped) =
  let start = getMonoTime()
  body
  let stop = getMonoTime()
  echo name, ": ", inMilliseconds(stop - start), " ms"


proc main() =
  let image = newImage(300, 300)
  let center = vec2(image.width / 2, image.height / 2)
  let pos = rgba(255, 0, 0, 255)
  let neg = rgba(0, 0, 255, 255)
  let corners = vec4(0.0, 20.0, 40.0, 80.0)
  let wh = vec2(200.0, 200.0)

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

  timeIt "clip - rounded":
    drawSdfShape(image,
              center = center,
              wh = wh,
              params = RoundedBoxParams(r: corners),
              pos = pos,
              neg = neg,
              mode = sdfModeClip)

  image.writeFile("tests/outputs/rounded_box_clip.png")

  timeIt "clipAliased - rounded":
    drawSdfShape(image,
              center = center,
              wh = wh,
              params = RoundedBoxParams(r: corners),
              pos = pos,
              neg = neg,
              mode = sdfModeClipAntiAlias)

  image.writeFile("tests/outputs/rounded_box_clip_aliased.png")

  timeIt "clip - chamfer":
    drawSdfShape(image,
              center = center,
              wh = wh,
              params = ChamferBoxParams(chamfer: 20.0),
              pos = pos,
              neg = neg,
              mode = sdfModeClip)

  image.writeFile("tests/outputs/chamfer_box_clip.png")

  timeIt "clipAliased - chamfer":
    drawSdfShape(image,
              center = center,
              wh = wh,
              params = ChamferBoxParams(chamfer: 20.0),
              pos = pos,
              neg = neg,
              mode = sdfModeClipAntiAlias)

  image.writeFile("tests/outputs/chamfer_box_clip_aliased.png")

  # timeIt "feather":
  #   signedBox(image,
  #             center = center,
  #             wh = wh,
  #             params = RoundedBoxParams(r: corners),
  #             pos = pos,
  #             neg = neg,
  #             mode = sdfModeFeather)

  # image.writeFile("tests/outputs/rounded_box_feather.png")

  # timeIt "featherInv":
  #   signedBox(image,
  #             center = center,
  #             wh = wh,
  #             params = RoundedBoxParams(r: corners),
  #             pos = pos,
  #             neg = neg,
  #             mode = sdfModeFeatherInv)

  # image.writeFile("tests/outputs/rounded_box_feather_inv.png")

  # timeIt "featherGaussian":
  #   signedBox(image,
  #             center = center,
  #             wh = wh,
  #             params = RoundedBoxParams(r: corners),
  #             pos = pos,
  #             neg = neg,
  #             mode = sdfModeFeatherGaussian)

  # image.writeFile("tests/outputs/rounded_box_feather_gaussian.png")

  # timeIt "dropShadow":
  #   signedBox(image,
  #             center = center,
  #             wh = wh,
  #             params = RoundedBoxParams(r: corners),
  #             pos = pos,
  #             neg = pos,
  #             factor = 10,
  #             spread = 20.0,
  #             mode = sdfModeDropShadow)

  # image.writeFile("tests/outputs/rounded_box_drop_shadow.png")

for i in 0 ..< 1:
  main()
