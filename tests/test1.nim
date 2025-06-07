import std/math, std/monotimes, std/times

import pixie

import sdfsy

template timeIt(name: string, body: untyped) =
  let start = getMonoTime()
  body
  let stop = getMonoTime()
  echo name, ": ", inMilliseconds(stop - start), " ms"

proc main() =
  let image = newImage(300, 300)
  let center = vec2(150.0, 150.0)
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

  image.writeFile("tests/rounded_box_base.png")

  timeIt "clip":
    signedRoundedBox(image,
                    center = center,
                    wh = wh,
                    r = corners,
                    pos = pos,
                    neg = neg,
                    mode = sdfModeClip)

  image.writeFile("tests/rounded_box.png")

  timeIt "feather":
    signedRoundedBox(image,
                    center = center,
                    wh = wh,
                    r = corners,
                    pos = pos,
                    neg = neg,
                    mode = sdfModeFeather)

  image.writeFile("tests/rounded_box_feather.png")

  timeIt "featherInv":
    signedRoundedBox(image,
                    center = center,
                    wh = wh,
                    r = corners,
                    pos = pos,
                    neg = neg,
                    mode = sdfModeFeatherInv)

  image.writeFile("tests/rounded_box_feather_inv.png")

  timeIt "featherGaussian":
    signedRoundedBox(image,
                    center = center,
                    wh = wh,
                    r = corners,
                    pos = pos,
                    neg = neg,
                    mode = sdfModeFeatherGaussian)

  image.writeFile("tests/rounded_box_feather_gaussian.png")

  timeIt "dropShadow":
    signedRoundedBox(image,
                    center = center,
                    wh = wh,
                    r = corners,
                    pos = pos,
                    neg = neg,
                    factor = 10,
                    spread = 20.0,
                    mode = sdfModeDropShadow)

  image.writeFile("tests/rounded_box_drop_shadow.png")

for i in 0 ..< 3:
  main()
