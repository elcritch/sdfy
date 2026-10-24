## nim r -d:release tests/bench_spans.nim
## Complete span pipeline; minimum of five samples, 30 frames of 257x193 pixels.
import std/[monotimes, strformat, times]
import chroma, pixie, vmath
import sdfy/spans

const Frames = 30

for scenario in ["solid", "linear + mask", "bilinear stroke", "three-stop inset shadow"]:
  let
    image = newImage(257, 193)
    mask = newImage(257, 193)
    field =
      initRoundedBoxField(vec2(104, 72), vec4(28, 14, 32, 8), vec4(12, 22, 10, 16))
    colors = [
      rgba(210, 40, 70, 190),
      rgba(20, 200, 150, 110),
      rgba(40, 70, 230, 225),
      rgba(180, 140, 30, 170),
    ]
    transform = translate(vec2(128.5, 96.5)) * rotate(0.11'f32)
    paint =
      case scenario
      of "solid":
        initSolidPaint(colors[0])
      of "linear + mask":
        initLinearPaint(colors[0], colors[1], vec2(-104, 0), vec2(208, 0))
      of "bilinear stroke":
        initBilinearPaint(colors, vec2(-104, -72), vec2(208, 144))
      else:
        initLinearPaint(
          colors[0], colors[1], colors[2], vec2(-104, -72), vec2(208, 144), 0.4
        )
    mode =
      if scenario == "bilinear stroke":
        smStrokeAA
      elif scenario == "three-stop inset shadow":
        smInsetShadow
      else:
        smFill
    effect =
      initSpanEffect(mode, strokeWidth = 5, blurRadius = 6, insetOffset = vec2(2, -3))
    clip = initSpanClip(vec2(-118.5, -87.5), vec2(118.5, 87.5))
    useMask = scenario == "linear + mask"
  for i in 0 ..< mask.data.len:
    mask.data[i] = rgbx(0, 0, 0, (i mod 256).uint8)
  var best = [high(float64), high(float64)]
  for trial in 0 ..< 5:
    for step in 0 ..< 2:
      let mode =
        if trial mod 2 == 0:
          step
        else:
          1 - step
      let start = getMonoTime()
      for frame in 0 ..< Frames:
        image.fill(rgba(30, 60, 90, 120))
        drawRoundedBox(
          image,
          field,
          paint,
          transform,
          effect,
          clip,
          mask = (if useMask: mask else: nil),
          useSimd = mode == 1,
        )
      best[mode] = min(best[mode], (getMonoTime() - start).inNanoseconds.float64 / 1e6)
  var checksum = 0'u64
  for pixel in image.data:
    checksum += pixel.r.uint64 + pixel.g.uint64 + pixel.b.uint64 + pixel.a.uint64
  echo &"{scenario}: scalar {best[0]:.2f} ms, SIMD {best[1]:.2f} ms, {best[0]/best[1]:.2f}x, checksum {checksum}"
