import std/[math, random, unittest]
import chroma, pixie, vmath
import sdfy/spans

proc maxDifference(a, b: openArray[ColorRGBX]): int =
  doAssert a.len == b.len
  for i in 0 ..< a.len:
    for delta in [
      abs(a[i].r.int - b[i].r.int),
      abs(a[i].g.int - b[i].g.int),
      abs(a[i].b.int - b[i].b.int),
      abs(a[i].a.int - b[i].a.int),
    ]:
      result = max(result, delta)

suite "SDF spans":
  test "SIMD matches scalar for gradients effects masks clipping and short spans":
    let
      a = rgba(240, 20, 80, 173)
      b = rgba(10, 220, 150, 63)
      c = rgba(90, 40, 250, 211)
      d = rgba(210, 100, 20, 127)
      paints = [
        initSolidPaint(a),
        initLinearPaint(a, b, vec2(-12, -8), vec2(24, 16)),
        initLinearPaint(a, b, c, vec2(-12, -8), vec2(24, 16), 0.37),
        initBilinearPaint([a, b, c, d], vec2(-12, -8), vec2(24, 16)),
      ]
      fields = [
        initRoundedBoxField(vec2(11, 9), vec4(4, 2, 7, 0)),
        initRoundedBoxField(vec2(11, 9), vec4(8, 0, 40, 6), vec4(3, 4, 40, 8)),
        initRoundedBoxField(vec2(0, 9), vec4(3), vec4(1)),
      ]
      coords = initSpanCoordinates(
        vec2(-18.25, 5.25),
        vec2(0.8, -0.13),
        translate(vec2(1.3, -0.7)) * rotate(0.31'f32) * scale(vec2(-0.9, 1.1)),
      )
      clip = initSpanClip(vec2(-14, -10), vec2(13, 9))
    var rng = initRand(98217)
    for count in [0, 1, 2, 3, 4, 5, 7, 15, 31, 64, 129]:
      var initial: seq[ColorRGBX]
      var byteMask: seq[uint8]
      var rgbaMask: seq[ColorRGBX]
      for i in 0 ..< count:
        initial.add(
          rgba(
            rand(rng, 255).uint8,
            rand(rng, 255).uint8,
            rand(rng, 255).uint8,
            rand(rng, 255).uint8,
          )
            .rgbx()
        )
        let alpha = rand(rng, 255).uint8
        byteMask.add(alpha)
        rgbaMask.add(rgba(255, 17, 95, alpha).rgbx())
      for field in fields:
        for paint in paints:
          for mode in SpanMode:
            let effect = initSpanEffect(
              mode,
              aaWidth = 0.7,
              strokeWidth = 3.5,
              blurRadius = 4.5,
              spread = -1.25,
              insetOffset = vec2(2, -3),
            )
            for blend in SpanBlend:
              for maskKind in 0 ..< 3:
                var scalar, vector: seq[ColorRGBX]
                for pixel in initial:
                  scalar.add(pixel)
                  vector.add(pixel)
                if maskKind == 0:
                  drawRoundedBoxSpan(
                    scalar, field, coords, paint, effect, clip, blend, useSimd = false
                  )
                  drawRoundedBoxSpan(vector, field, coords, paint, effect, clip, blend)
                elif maskKind == 1:
                  drawRoundedBoxSpan(
                    scalar,
                    field,
                    coords,
                    paint,
                    byteMask,
                    effect,
                    clip,
                    blend,
                    useSimd = false,
                  )
                  drawRoundedBoxSpan(
                    vector, field, coords, paint, byteMask, effect, clip, blend
                  )
                else:
                  drawRoundedBoxSpan(
                    scalar,
                    field,
                    coords,
                    paint,
                    effect,
                    clip,
                    blend,
                    rgbaMask,
                    useSimd = false,
                  )
                  drawRoundedBoxSpan(
                    vector, field, coords, paint, effect, clip, blend, rgbaMask
                  )
                check maxDifference(scalar, vector) <= 1

  test "solid coverage and compositing round byte products exactly":
    let field = initRoundedBoxField(vec2(100), vec4(0))
    let coords = initSpanCoordinates(vec2(0))
    for alpha in 0 .. 255:
      let paint =
        initSolidPaint(rgba(alpha.uint8, (255 - alpha).uint8, 127, alpha.uint8))
      for mask in countup(0, 255, 4):
        let masks = [mask.uint8, (mask + 1).uint8, (mask + 2).uint8, (mask + 3).uint8]
        for blend in SpanBlend:
          var scalar, vector: array[4, ColorRGBX]
          for i in 0 ..< 4:
            scalar[i] = rgba(43, 219, 174, 193).rgbx()
            vector[i] = scalar[i]
          drawRoundedBoxSpan(
            scalar, field, coords, paint, masks, blend = blend, useSimd = false
          )
          drawRoundedBoxSpan(vector, field, coords, paint, masks, blend = blend)
          check scalar == vector

  test "unaligned spans preserve guards and read only mask alpha":
    let field = initRoundedBoxField(vec2(100), vec4(0))
    let coords = initSpanCoordinates(vec2(0))
    let paint = initSolidPaint(rgba(255, 0, 0, 255))
    var pixels: array[11, ColorRGBX]
    var mask: array[9, ColorRGBX]
    for i in 0 ..< mask.len:
      mask[i] = rgbx(0, 17, 0, 127)
    pixels[0] = rgbx(1, 2, 3, 255)
    pixels[10] = rgbx(4, 5, 6, 255)
    drawRoundedBoxSpan(pixels.toOpenArray(1, 9), field, coords, paint, mask = mask)
    check pixels[0] == rgbx(1, 2, 3, 255)
    check pixels[10] == rgbx(4, 5, 6, 255)
    for i in 1 .. 9:
      check pixels[i] == rgbx(127, 0, 0, 127)

  test "image wrapper uses the same span pipeline with affine coordinates":
    let
      field = initRoundedBoxField(vec2(8, 5), vec4(3), vec4(2), center = vec2(1, 2))
      paint = initLinearPaint(
        rgba(255, 0, 0, 150), rgba(0, 0, 255, 220), vec2(-8, 0), vec2(16, 0)
      )
      mat = translate(vec2(9, 7)) * rotate(0.2'f32)
      inverse = mat.inverse()
      image = newImage(19, 15)
      expected = newImage(19, 15)
      mask = newImage(19, 15)
    mask.fill(rgba(0, 255, 0, 137))
    image.fill(rgba(20, 70, 90, 170))
    expected.fill(rgba(20, 70, 90, 170))
    drawRoundedBox(image, field, paint, mat, mask = mask)
    for y in 0 ..< image.height:
      let first = y * image.width
      let last = first + image.width - 1
      drawRoundedBoxSpan(
        expected.data.toOpenArray(first, last),
        field,
        initSpanCoordinates(vec2(0.5, y.float32 + 0.5'f32), transform = inverse),
        paint,
        mask = mask.data.toOpenArray(first, last),
        useSimd = false,
      )
    check maxDifference(image.data, expected.data) <= 1

  test "identical destination and RGBA mask views read original alpha":
    let field = initRoundedBoxField(vec2(100), vec4(0))
    let coords = initSpanCoordinates(vec2(0))
    let paint = initSolidPaint(rgba(210, 70, 120, 190))
    var mask: array[9, ColorRGBX]
    for i in 0 ..< mask.len:
      mask[i] = rgba(60, 170, 210, (i * 31).uint8).rgbx()
    for blend in SpanBlend:
      for vectorized in [false, true]:
        var expected = mask
        var actual = mask
        drawRoundedBoxSpan(
          expected,
          field,
          coords,
          paint,
          blend = blend,
          mask = mask,
          useSimd = vectorized,
        )
        drawRoundedBoxSpan(
          actual,
          field,
          coords,
          paint,
          blend = blend,
          mask = actual,
          useSimd = vectorized,
        )
        check actual == expected

  test "invalid public arguments fail before mutating destination":
    let field = initRoundedBoxField(vec2(5), vec4(2))
    let paint = initSolidPaint(rgba(255, 0, 0, 255))
    let coords = initSpanCoordinates(vec2(0))
    var pixels: array[5, ColorRGBX]
    expect ValueError:
      drawRoundedBoxSpan(pixels, field, coords, paint, mask = [1'u8, 2, 3])
    check pixels == default(array[5, ColorRGBX])
    expect ValueError:
      discard initSpanCoordinates(vec2(NaN.float32, 0))
    expect ValueError:
      discard
        initLinearPaint(rgba(0, 0, 0, 255), rgba(255, 255, 255, 255), vec2(0), vec2(0))
    expect ValueError:
      discard initLinearPaint(
        rgba(0, 0, 0, 255),
        rgba(0, 0, 0, 255),
        rgba(255, 255, 255, 255),
        vec2(0),
        vec2(1),
        midpoint = 1,
      )
    expect ValueError:
      discard initSpanEffect(aaWidth = 0)
    expect ValueError:
      discard initBilinearPaint(default(array[4, ColorRGBA]), vec2(0), vec2(1, 0))
    expect ValueError:
      discard initBilinearPaint(default(array[4, ColorRGBA]), vec2(0), vec2(1e-39'f32))
    expect ValueError:
      discard initLinearPaint(
        rgba(255, 0, 0, 255),
        rgba(0, 255, 0, 255),
        rgba(0, 0, 255, 255),
        vec2(0),
        vec2(1, 0),
        midpoint = 1e-39'f32,
      )
    # The smallest accepted intervals must still produce the exact start color.
    let tiny = initLinearPaint(
      rgba(255, 0, 0, 255),
      rgba(0, 255, 0, 255),
      rgba(0, 0, 255, 255),
      vec2(0),
      vec2(1, 0),
      midpoint = 1e-38'f32,
    )
    var startPixels: array[4, ColorRGBX]
    drawRoundedBoxSpan(
      startPixels, field, initSpanCoordinates(vec2(0), step = vec2(0)), tiny
    )
    check startPixels == [
      rgbx(255, 0, 0, 255),
      rgbx(255, 0, 0, 255),
      rgbx(255, 0, 0, 255),
      rgbx(255, 0, 0, 255),
    ]
    expect ValueError:
      discard initSpanClip(vec2(2), vec2(1))
    expect ValueError:
      drawRoundedBox(newImage(5, 5), field, paint, mask = newImage(4, 5))
    expect ValueError:
      drawRoundedBox(newImage(5, 5), field, paint, transform = scale(vec2(0)))
