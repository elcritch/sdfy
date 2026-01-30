import std/unittest
import std/math
import std/unicode
import std/os
import std/strformat
import std/tables

import pixie
import sdfy
import sdfy/msdfgenSvg

const
  fontPath = "deps/pixie/tests/fonts/Inter-Regular.ttf"
  glyphH = Rune('H')
  glyphA = Rune('A')
  msdfSampleText = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\nabcdefghijklmnopqrstuvwxyz\n0123456789"
  msdfSampleSizes = [64, 48, 32, 24, 20, 18, 16, 15, 14, 12, 10, 8, 7, 6, 5]
  msdfGlyphSize = 64
  msdfPxRange = 4.0
  msdfIconSize = 128
  starSvgPath = "tests/Yellow_Star_with_rounded_edges.svg"
  meanDiffLimit = 0.002

proc median(a, b, c: float64): float64 =
  max(min(a, b), min(max(a, b), c))

proc diffImages(
    expected, actual: Image, diffPath: string
): tuple[mean: float64, max: float64] =
  doAssert expected.width == actual.width
  doAssert expected.height == actual.height

  var total = 0.0
  var maxDiff = 0.0
  let count = expected.data.len
  let diffImage = newImage(expected.width, expected.height)

  for i in 0 ..< count:
    let e = expected.data[i]
    let a = actual.data[i]
    let dr = abs(e.r.float64 - a.r.float64) / 255.0
    let dg = abs(e.g.float64 - a.g.float64) / 255.0
    let db = abs(e.b.float64 - a.b.float64) / 255.0
    total += dr + dg + db
    maxDiff = max(maxDiff, max(dr, max(dg, db)))

    if diffPath.len > 0:
      let diffColor = rgba(
        uint8(round(dr * 255.0)),
        uint8(round(dg * 255.0)),
        uint8(round(db * 255.0)),
        255,
      )
      diffImage.data[i] = diffColor.rgbx()

  if diffPath.len > 0:
    diffImage.writeFile(diffPath)
  (total / (count.float64 * 3.0), maxDiff)

proc verifyMsdf(typeface: Typeface, glyph: Rune, outputPath: string) =
  let result = generateMsdfGlyph(typeface, glyph, 64, 64, 4.0)

  doAssert result.image.width == 64
  doAssert result.image.height == 64

  let path = typeface.getGlyphPath(glyph)
  let mask = newImage(64, 64)
  mask.fill(rgba(0, 0, 0, 0))

  let flip = translate(vec2(0, result.image.height.float32)) * scale(vec2(1, -1))
  let transform =
    flip * scale(vec2(result.scale.float32, result.scale.float32)) *
    translate(vec2(result.translate.x.float32, result.translate.y.float32))
  mask.fillPath(path, rgba(255, 255, 255, 255), transform = transform)

  var insideIdx = -1
  var outsideIdx = -1

  for y in 0 ..< mask.height:
    for x in 0 ..< mask.width:
      let idx = y * mask.width + x
      let a = mask.data[idx].a
      if insideIdx < 0 and a > 0'u8:
        insideIdx = idx
      if outsideIdx < 0 and a == 0'u8:
        outsideIdx = idx
      if insideIdx >= 0 and outsideIdx >= 0:
        break
    if insideIdx >= 0 and outsideIdx >= 0:
      break

  doAssert insideIdx >= 0
  doAssert outsideIdx >= 0

  let inside = result.image.data[insideIdx]
  let insideMedian =
    median(inside.r.float64 / 255.0, inside.g.float64 / 255.0, inside.b.float64 / 255.0)

  let outside = result.image.data[outsideIdx]
  let outsideMedian = median(
    outside.r.float64 / 255.0, outside.g.float64 / 255.0, outside.b.float64 / 255.0
  )

  var minMedian = 1.0
  var maxMedian = 0.0
  for px in result.image.data:
    let m = median(px.r.float64 / 255.0, px.g.float64 / 255.0, px.b.float64 / 255.0)
    if m < minMedian:
      minMedian = m
    if m > maxMedian:
      maxMedian = m

  doAssert minMedian < 0.5
  doAssert maxMedian > 0.5
  doAssert abs(insideMedian - outsideMedian) > 0.05

  let rendered = renderMsdf(result)
  rendered.writeFile(outputPath)

suite "msdf glyph":
  test "generate msdf from glyph path":
    let typeface = readTypeface(fontPath)
    verifyMsdf(typeface, glyphH, "tests/outputs/msdf_H.png")
    verifyMsdf(typeface, glyphA, "tests/outputs/msdf_A.png")

  test "render alphabet and numbers at multiple sizes":
    let typeface = readTypeface(fontPath)
    if not dirExists("tests/outputs"):
      createDir("tests/outputs")

    var msdfCache = initTable[Rune, MsdfGlyphResult]()
    for size in msdfSampleSizes:
      let font = block:
        let f = newFont(typeface)
        f.size = size.float32
        f
      let arrangement = font.typeset(msdfSampleText)
      let bounds = arrangement.computeBounds()
      let padding = 8.0
      let imgWidth = max(1, int(ceil(bounds.w + padding * 2)))
      let imgHeight = max(1, int(ceil(bounds.h + padding * 2)))
      let offset = vec2((padding - bounds.x).float32, (padding - bounds.y).float32)

      let image = newImage(imgWidth, imgHeight)
      image.fill(rgba(0, 0, 0, 0))

      for i, rune in arrangement.runes:
        if rune == Rune(10) or rune == Rune(32):
          continue
        if not typeface.hasGlyph(rune):
          continue
        if not msdfCache.hasKey(rune):
          msdfCache[rune] =
            generateMsdfGlyph(typeface, rune, msdfGlyphSize, msdfGlyphSize, msdfPxRange)
        let glyph = msdfCache[rune]

        let scaleFactor = font.scale.float64 / glyph.scale
        let renderWidth = max(1, int(round(glyph.image.width.float64 * scaleFactor)))
        let renderHeight = max(1, int(round(glyph.image.height.float64 * scaleFactor)))
        let scaleX = renderWidth.float64 / glyph.image.width.float64
        let scaleY = renderHeight.float64 / glyph.image.height.float64
        let originX = glyph.scale * glyph.translate.x - 0.5
        let originY =
          glyph.image.height.float64 - (glyph.scale * glyph.translate.y + 0.5)
        let originXScaled = originX * scaleX
        let originYScaled = originY * scaleY
        let originYFlipped = (renderHeight.float64 - 1.0) - originYScaled

        let pos = arrangement.positions[i]
        let baseX = pos.x.float64 + offset.x.float64
        let baseY = pos.y.float64 + offset.y.float64
        let destX = int(round(baseX - originXScaled))
        let destY = int(round(baseY - originYFlipped))

        blitMsdfGlyph(
          image,
          glyph.image,
          destX,
          destY,
          renderWidth,
          renderHeight,
          msdfPxRange,
          bgColor = rgba(0, 0, 0, 0),
        )

      image.writeFile(&"tests/outputs/msdf_alnum_{size}.png")

  test "render alphabet and numbers at multiple sizes with drawSdfShape":
    let typeface = readTypeface(fontPath)
    if not dirExists("tests/outputs"):
      createDir("tests/outputs")

    var msdfCache = initTable[Rune, MsdfGlyphResult]()
    for size in msdfSampleSizes:
      let font = block:
        let f = newFont(typeface)
        f.size = size.float32
        f
      let arrangement = font.typeset(msdfSampleText)
      let bounds = arrangement.computeBounds()
      let padding = 8.0
      let imgWidth = max(1, int(ceil(bounds.w + padding * 2)))
      let imgHeight = max(1, int(ceil(bounds.h + padding * 2)))
      let offset = vec2((padding - bounds.x).float32, (padding - bounds.y).float32)

      let image = newImage(imgWidth, imgHeight)
      image.fill(rgba(0, 0, 0, 0))
      let ctx = newContext(image)

      for i, rune in arrangement.runes:
        if rune == Rune(10) or rune == Rune(32):
          continue
        if not typeface.hasGlyph(rune):
          continue
        if not msdfCache.hasKey(rune):
          msdfCache[rune] =
            generateMsdfGlyph(typeface, rune, msdfGlyphSize, msdfGlyphSize, msdfPxRange)
        let glyph = msdfCache[rune]

        let scaleFactor = font.scale.float64 / glyph.scale
        let renderWidth = max(1, int(round(glyph.image.width.float64 * scaleFactor)))
        let renderHeight = max(1, int(round(glyph.image.height.float64 * scaleFactor)))

        let scaleX = renderWidth.float64 / glyph.image.width.float64
        let scaleY = renderHeight.float64 / glyph.image.height.float64
        let originX = glyph.scale * glyph.translate.x - 0.5
        let originY =
          glyph.image.height.float64 - (glyph.scale * glyph.translate.y + 0.5)
        let originXScaled = originX * scaleX
        let originYScaled = originY * scaleY
        let originYFlipped = (renderHeight.float64 - 1.0) - originYScaled

        let pos = arrangement.positions[i]
        let baseX = pos.x.float64 + offset.x.float64
        let baseY = pos.y.float64 + offset.y.float64
        let destX = int(round(baseX - originXScaled))
        let destY = int(round(baseY - originYFlipped))

        let glyphImage = newImage(renderWidth, renderHeight)
        glyphImage.fill(rgba(0, 0, 0, 0))
        let params = MsdfBitmapParams(
          image: glyph.image,
          pxRange: (glyph.range * glyph.scale).float32,
          sdThreshold: 0.5,
          flipY: true,
        )
        drawSdfShape(
          glyphImage,
          vec2(renderWidth.float32 / 2.0, renderHeight.float32 / 2.0),
          vec2(renderWidth.float32, renderHeight.float32),
          params,
          rgba(0, 0, 0, 255),
          rgba(0, 0, 0, 0),
          sdfModeClipAA,
          pointOffset = vec2(0.0, 0.2),
        )

        ctx.drawImage(glyphImage, destX.float32, destY.float32)

      image.writeFile(&"tests/outputs/msdf_alnum_draw_{size}.png")

  test "render yellow star icon":
    if not dirExists("tests/outputs"):
      createDir("tests/outputs")

    let (path, elementCount) = loadSvgPath(starSvgPath)
    doAssert elementCount > 0
    let glyphSize = msdfIconSize div 2
    let glyph = generateMtsdfPath(path, glyphSize, glyphSize, msdfPxRange)

    glyph.image.writeFile("tests/outputs/msdf_star_field.png")

    ## medium star with shadow!
    let image = newImage(msdfIconSize, msdfIconSize)
    let params = MsdfBitmapParams(
      image: glyph.image,
      pxRange: (glyph.range * glyph.scale).float32,
      sdThreshold: 0.5,
      flipY: true,
    )
    let shadowParams = MsdfBitmapParams(
      image: glyph.image,
      pxRange: (glyph.range * glyph.scale).float32,
      sdThreshold: 0.5,
      flipY: true,
      useAlpha: true,
    )
    drawSdfShape(
      image,
      vec2(msdfIconSize.float32 / 2.0, msdfIconSize.float32 / 2.0),
      vec2(msdfIconSize.float32, msdfIconSize.float32),
      params,
      rgba(255, 215, 0, 255),
      rgba(0, 0, 0, 0),
      sdfModeClipAA,
      pointOffset = vec2(0.0, 0.2),
    )

    image.writeFile("tests/outputs/msdf_star_icon.png")

    let largeSize = 8 * msdfIconSize
    let largeCenter = vec2(largeSize.float32 / 2.0, largeSize.float32 / 2.0)
    let shadowOffset = vec2(20.0, 20.0)

    ## large star with shadow!
    let shadowImage = newImage(largeSize, largeSize)
    drawSdfShape(
      shadowImage,
      largeCenter + shadowOffset,
      vec2(largeSize.float32, largeSize.float32),
      shadowParams,
      rgba(0, 0, 0, 255),
      rgba(0, 0, 0, 0),
      sdfModeDropShadow,
      factor = 1,
    )

    let starImage = newImage(largeSize, largeSize)
    starImage.fill(rgba(0, 0, 0, 0))
    drawSdfShape(
      starImage,
      largeCenter,
      vec2(largeSize.float32, largeSize.float32),
      params,
      rgba(255, 215, 0, 255),
      rgba(0, 0, 0, 0),
      sdfModeClipAA,
      pointOffset = vec2(0.0, 0.2),
      aaFactor = 8,
    )

    let imagel = newImage(largeSize, largeSize)
    imagel.fill(rgba(255, 255, 255, 255))
    let ctxLarge = newContext(imagel)
    ctxLarge.drawImage(shadowImage, 0, 0)
    ctxLarge.drawImage(starImage, 0, 0)

    imagel.writeFile("tests/outputs/msdf_star_icon_large.png")

    ## large star using blitz
    let blitLarge = newImage(largeSize, largeSize)
    blitLarge.fill(rgba(0, 0, 0, 0))
    blitMsdfGlyph(
      blitLarge,
      glyph,
      0,
      0,
      largeSize,
      largeSize,
      fgColor = rgba(255, 215, 0, 255),
      bgColor = rgba(0, 0, 0, 0),
    )
    blitLarge.writeFile("tests/outputs/msdf_star_icon_large_blitz.png")

    let blitzCompare = newImage(largeSize, largeSize)
    blitzCompare.fill(rgba(0, 0, 0, 255))
    let ctxBlitz = newContext(blitzCompare)
    ctxBlitz.drawImage(blitLarge, 0, 0)
    blitzCompare.writeFile("tests/outputs/msdf_star_icon_large_blitz_compare.png")

    let expectedStarPath = "tests/expected/msdf_star_icon_large_blitz.png"
    doAssert fileExists(expectedStarPath)
    let expectedStar = readImage(expectedStarPath)
    let diffPath = "tests/outputs/msdf_star_icon_large_blitz_diff.png"
    let (meanDiff, _) = diffImages(expectedStar, blitzCompare, diffPath)
    check meanDiff <= meanDiffLimit
