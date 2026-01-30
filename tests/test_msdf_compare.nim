import std/unittest
import std/os
import std/osproc
import std/strformat
import std/strutils
import std/unicode
import std/sequtils

import pixie
import sdfy

const
  fontPath = "deps/pixie/tests/fonts/Inter-Regular.ttf"
  buildDir = "deps/msdfgen/build"
  outDir = "tests/outputs"
  expectedDir = "tests/expected"
  meanDiffLimit = 0.001
  msdfPxRange = 4.0
  msdfDim = 64
  alnumText = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

proc joinCmd(cmd: string; args: seq[string]): string =
  if args.len == 0:
    return cmd
  result = cmd & " " & args.mapIt(quoteShell(it)).join(" ")

proc runCmd(cmd: string; args: seq[string]; workdir: string) =
  let fullCmd = joinCmd(cmd, args)
  let res = execCmdEx(fullCmd, workingDir = workdir)
  if res.exitCode != 0:
    raise newException(IOError, &"Command failed ({res.exitCode}): {fullCmd}\n{res.output}")

proc findMsdfgenExe(): string =
  when defined(windows):
    let exeName = "msdfgen.exe"
  else:
    let exeName = "msdfgen"

  let direct = buildDir / exeName
  if fileExists(direct):
    return direct

  when defined(windows):
    let release = buildDir / "Release" / exeName
    if fileExists(release):
      return release

  for path in walkDirRec(buildDir):
    if splitFile(path).name == splitFile(exeName).name and fileExists(path):
      return path

  return direct

proc ensureMsdfgenBuilt() =
  if not dirExists(buildDir):
    createDir(buildDir)

  let exePath = findMsdfgenExe()
  if fileExists(exePath):
    return

  runCmd("cmake", @[
    "-S", "deps/msdfgen",
    "-B", buildDir,
    "-DMSDFGEN_BUILD_STANDALONE=ON",
    "-DMSDFGEN_USE_SKIA=OFF",
    "-DMSDFGEN_USE_VCPKG=OFF",
    "-DMSDFGEN_DISABLE_SVG=ON",
    "-DMSDFGEN_DISABLE_VARIABLE_FONTS=ON"
  ], getCurrentDir())

  runCmd("cmake", @["--build", buildDir, "--config", "Release"], getCurrentDir())

proc runMsdfgenGlyph(exePath: string; glyph: Rune; outputPath: string) =
  let name = $glyph.int
  runCmd(exePath, @[
    "msdf",
    "-font", fontPath, name,
    "-o", outputPath,
    "-dimensions", $msdfDim, $msdfDim,
    "-pxrange", $msdfPxRange,
    "-autoframe",
    "-angle", "3",
    "-errorcorrection", "disabled",
    "-noscanline",
    "-guesswinding",
    "-noemnormalize",
    "-yflip"
  ], getCurrentDir())

proc runMsdfgenGlyphRender(exePath: string; glyph: Rune; msdfPath: string; renderPath: string) =
  let name = $glyph.int
  runCmd(exePath, @[
    "msdf",
    "-font", fontPath, name,
    "-o", msdfPath,
    "-dimensions", $msdfDim, $msdfDim,
    "-pxrange", $msdfPxRange,
    "-autoframe",
    "-angle", "3",
    "-errorcorrection", "disabled",
    "-noscanline",
    "-guesswinding",
    "-noemnormalize",
    "-yflip",
    "-testrender", renderPath, $msdfDim, $msdfDim
  ], getCurrentDir())

proc writeMsdfyGlyph(typeface: Typeface; glyph: Rune; outputPath: string): Image =
  let glyphResult = generateMsdfGlyph(typeface, glyph, msdfDim, msdfDim, msdfPxRange)
  glyphResult.image.writeFile(outputPath)
  glyphResult.image

proc diffImages(expected, actual: Image; diffPath: string; invert = false; flipY = false): tuple[mean: float64, max: float64] =
  doAssert expected.width == actual.width
  doAssert expected.height == actual.height

  var total = 0.0
  var maxDiff = 0.0
  let count = expected.data.len
  let diffImage = newImage(expected.width, expected.height)

  for i in 0 ..< count:
    let e = expected.data[i]
    let ax = i mod expected.width
    let ay = if flipY: (expected.height - 1) - (i div expected.width) else: i div expected.width
    let aidx = ay * expected.width + ax
    let a = actual.data[aidx]
    let ar = if invert: 255.0 - a.r.float64 else: a.r.float64
    let ag = if invert: 255.0 - a.g.float64 else: a.g.float64
    let ab = if invert: 255.0 - a.b.float64 else: a.b.float64
    let dr = abs(e.r.float64 - ar) / 255.0
    let dg = abs(e.g.float64 - ag) / 255.0
    let db = abs(e.b.float64 - ab) / 255.0
    total += dr + dg + db
    maxDiff = max(maxDiff, max(dr, max(dg, db)))

    if diffPath.len > 0:
      let diffColor = rgba(
        uint8(round(dr * 255.0)),
        uint8(round(dg * 255.0)),
        uint8(round(db * 255.0)),
        255
      )
      diffImage.data[i] = diffColor.rgbx()

  if diffPath.len > 0:
    diffImage.writeFile(diffPath)
  (total / (count.float64 * 3.0), maxDiff)

suite "msdf glyph compare":
  test "compare msdfgen vs sdfy":
    if not dirExists(outDir):
      createDir(outDir)

    let typeface = readTypeface(fontPath)
    var glyphs: seq[Rune] = @[]
    for ch in alnumText:
      glyphs.add(Rune(ch))

    var missingExpected = false
    for glyph in glyphs:
      let name = $char(glyph.int)
      let msdfgenPath = expectedDir / &"msdfgen_{name}.png"
      if not fileExists(msdfgenPath):
        missingExpected = true
        break

    var exePath = ""
    if missingExpected:
      if not dirExists(expectedDir):
        createDir(expectedDir)
      ensureMsdfgenBuilt()
      exePath = findMsdfgenExe()
      doAssert fileExists(exePath)

    for glyph in glyphs:
      let name = $char(glyph.int)
      let msdfgenPath = expectedDir / &"msdfgen_{name}.png"
      let msdfyPath = outDir / &"msdfy_{name}.png"
      let diffPath = outDir / &"msdf_diff_{name}.png"

      if missingExpected and not fileExists(msdfgenPath):
        runMsdfgenGlyph(exePath, glyph, msdfgenPath)

      let expectedMsdf = readImage(msdfgenPath)
      let actualMsdf = writeMsdfyGlyph(typeface, glyph, msdfyPath)
      let expected = renderMsdf(expectedMsdf, msdfPxRange)
      let actual = renderMsdf(actualMsdf, msdfPxRange)
      expected.writeFile(outDir / &"msdfgen_render_{name}.png")
      actual.writeFile(outDir / &"msdfy_render_{name}.png")

      let (meanDiff, maxDiff) = diffImages(expected, actual, diffPath)
      let (meanInv, _) = diffImages(expected, actual, "", invert = true)
      let (meanFlip, _) = diffImages(expected, actual, "", flipY = true)
      let (meanFlipInv, _) = diffImages(expected, actual, "", invert = true, flipY = true)
      echo &"Glyph {name}: mean diff {meanDiff:.4f}, max diff {maxDiff:.4f} (inv {meanInv:.4f}, flip {meanFlip:.4f}, flip+inv {meanFlipInv:.4f})"
      check meanDiff <= meanDiffLimit

  test "compare rendered msdfgen vs sdfy":
    if not dirExists(outDir):
      createDir(outDir)

    let typeface = readTypeface(fontPath)
    var glyphs: seq[Rune] = @[]
    for ch in alnumText:
      glyphs.add(Rune(ch))

    var missingExpected = false
    for glyph in glyphs:
      let name = $char(glyph.int)
      let msdfgenRenderPath = expectedDir / &"msdfgen_testrender_{name}.png"
      if not fileExists(msdfgenRenderPath):
        missingExpected = true
        break

    var exePath = ""
    if missingExpected:
      if not dirExists(expectedDir):
        createDir(expectedDir)
      ensureMsdfgenBuilt()
      exePath = findMsdfgenExe()
      doAssert fileExists(exePath)

    for glyph in glyphs:
      let name = $char(glyph.int)
      let msdfgenPath = outDir / &"msdfgen_msdf_{name}.png"
      let msdfgenRenderPath = expectedDir / &"msdfgen_testrender_{name}.png"
      let msdfyPath = outDir / &"msdfy_{name}.png"
      let msdfyRenderPath = outDir / &"msdfy_testrender_{name}.png"
      let diffPath = outDir / &"render_diff_{name}.png"

      if missingExpected and not fileExists(msdfgenRenderPath):
        runMsdfgenGlyphRender(exePath, glyph, msdfgenPath, msdfgenRenderPath)

      let expectedRender = readImage(msdfgenRenderPath)
      let actualMsdf = writeMsdfyGlyph(typeface, glyph, msdfyPath)
      let actualRender = renderMsdf(actualMsdf, msdfPxRange)
      actualRender.writeFile(msdfyRenderPath)

      let (meanDiff, maxDiff) = diffImages(expectedRender, actualRender, diffPath)
      let (meanInv, _) = diffImages(expectedRender, actualRender, "", invert = true)
      let (meanFlip, _) = diffImages(expectedRender, actualRender, "", flipY = true)
      let (meanFlipInv, _) = diffImages(expectedRender, actualRender, "", invert = true, flipY = true)
      echo &"Glyph {name}: mean render diff {meanDiff:.4f}, max diff {maxDiff:.4f} (inv {meanInv:.4f}, flip {meanFlip:.4f}, flip+inv {meanFlipInv:.4f})"
      check meanDiff <= meanDiffLimit
