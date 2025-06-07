import std/math, std/monotimes, std/times
import pixie, vmath, pixie/simd

import ./sdfytypes
import ./simd/roundedboxSimd

export roundedboxSimd

proc sdRoundedBox*(p: Vec2, b: Vec2, r: Vec4): float32 {.inline.} =
  ## Signed distance function for a rounded box
  ## p: point to test
  ## b: box half-extents (width/2, height/2)
  ## r: corner radii as Vec4 (x=top-right, y=bottom-right, z=bottom-left, w=top-left)
  ## Returns: signed distance (negative inside, positive outside)
  var cornerRadius = r
  
  # Select appropriate corner radius based on quadrant
  cornerRadius.xy = if p.x > 0.0: r.xy else: r.zw
  cornerRadius.x = if p.y > 0.0: cornerRadius.x else: cornerRadius.y
  
  # Calculate distance
  let q = abs(p) - b + vec2(cornerRadius.x, cornerRadius.x)
  
  result = min(max(q.x, q.y), 0.0) + length(max(q, vec2(0.0, 0.0))) - cornerRadius.x

proc signedRoundedBox*[I](
    image: I,
    center: Vec2,
    wh: Vec2,
    r: Vec4,
    pos: ColorRGBA,
    neg: ColorRGBA,
    factor: float32 = 4,
    spread: float32 = 0.0,
    mode: SDFMode = sdfModeFeatherInv
) {.hasSimd, raises: [].} =
  ## Signed distance function for a rounded box
  ## p: point to test
  ## b: box half-extents (width/2, height/2)
  ## r: corner radii as Vec4 (x=top-right, y=bottom-right, z=bottom-left, w=top-left)
  ## Returns: signed distance (negative inside, positive outside)
  let
    b = wh / 2.0

  for y in 0 ..< image.height:
    echo ""
    for x in 0 ..< image.width:
      let p = vec2(x.float32, y.float32) - center
      let sd = sdRoundedBox(p, b, r)


      var c: ColorRGBA = if sd < 0.0: pos else: neg
      case mode:
      of sdfModeClip:
        discard
      of sdfModeClipAliased:
        # we offset by 0.5 to make the edges blur
        # the clamping makes the transition go by ~1 pixel
        # then we mix the pos and neg colors based on the clamped value
        let cl = clamp(sd + 0.5, 0.0, 1.0)
        c = mix(pos, neg, cl)
      of sdfModeFeather:
        c.a = uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherInv:
        c.a = 255 - uint8(max(0.0, min(255, (factor*sd) + 127)))
      of sdfModeFeatherGaussian:
        let sd = sd
        let s = 2.2
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = uint8(f * 255)
      of sdfModeDropShadow:
        let s = 2.2
        let sd = sd / factor * s - spread / 8.8
        let f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        c.a = if sd > 0.0: uint8(min(f * 255 * 6, 255)) else: 255
      let idx = image.dataIndex(x, y)
      image.data[idx] = c.rgbx()
