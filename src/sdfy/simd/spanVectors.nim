## Internal four-pixel arithmetic for span shading and premultiplied blending.

when defined(amd64):
  import nimsimd/sse2
  type
    Float4* = M128
    Word4* = M128i
    Mask4* = M128

  proc splat*(x: float32): Float4 {.inline.} =
    mm_set1_ps(x)

  proc words*(x: uint32): Word4 {.inline.} =
    mm_set1_epi32(cast[int32](x))

  proc lanes*(): Float4 {.inline.} =
    mm_setr_ps(0, 1, 2, 3)

  proc `+`*(a, b: Float4): Float4 {.inline.} =
    mm_add_ps(a, b)

  proc `-`*(a, b: Float4): Float4 {.inline.} =
    mm_sub_ps(a, b)

  proc `*`*(a, b: Float4): Float4 {.inline.} =
    mm_mul_ps(a, b)

  proc min4*(a, b: Float4): Float4 {.inline.} =
    mm_min_ps(a, b)

  proc max4*(a, b: Float4): Float4 {.inline.} =
    mm_max_ps(a, b)

  proc le4*(a, b: Float4): Mask4 {.inline.} =
    mm_cmple_ps(a, b)

  proc lt4*(a, b: Float4): Mask4 {.inline.} =
    mm_cmplt_ps(a, b)

  proc and4*(a, b: Mask4): Mask4 {.inline.} =
    mm_and_ps(a, b)

  proc choose*(mask: Mask4, a, b: Float4): Float4 {.inline.} =
    mm_or_ps(mm_and_ps(mask, a), mm_andnot_ps(mask, b))

  proc chooseWords*(mask: Mask4, a, b: Word4): Word4 {.inline.} =
    mm_or_si128(
      mm_and_si128(mm_castps_si128(mask), a), mm_andnot_si128(mm_castps_si128(mask), b)
    )

  proc addWords*(a, b: Word4): Word4 {.inline.} =
    mm_add_epi32(a, b)

  proc subWords*(a, b: Word4): Word4 {.inline.} =
    mm_sub_epi32(a, b)

  proc mulWords*(a, b: Word4): Word4 {.inline.} =
    # Inputs are byte channels; products fit in each low 16-bit half.
    mm_mullo_epi16(a, b)

  proc andWords*(a, b: Word4): Word4 {.inline.} =
    mm_and_si128(a, b)

  proc orWords*(a, b: Word4): Word4 {.inline.} =
    mm_or_si128(a, b)

  template shrWords*(a: Word4, n: static int): Word4 =
    mm_srli_epi32(a, n)

  template shlWords*(a: Word4, n: static int): Word4 =
    mm_slli_epi32(a, n)

  proc toWords*(a: Float4): Word4 {.inline.} =
    mm_cvttps_epi32(a)

  proc toFloats*(a: Word4): Float4 {.inline.} =
    mm_cvtepi32_ps(a)

  proc asFloats*(a: Word4): Float4 {.inline.} =
    mm_castsi128_ps(a)

  proc loadWords*(p: pointer): Word4 {.inline.} =
    mm_loadu_si128(p)

  proc storeWords*(p: pointer, a: Word4) {.inline.} =
    mm_storeu_si128(p, a)

  proc loadFloats*(p: ptr float32): Float4 {.inline.} =
    mm_loadu_ps(p)

  proc storeFloats*(p: ptr float32, a: Float4) {.inline.} =
    mm_storeu_ps(p, a)

  proc maskBytes*(p: ptr UncheckedArray[uint8]): Word4 {.inline.} =
    var packed: uint32
    copyMem(packed.addr, p, 4)
    let bytes = mm_cvtsi32_si128(cast[int32](packed))
    mm_unpacklo_epi16(mm_unpacklo_epi8(bytes, mm_setzero_si128()), mm_setzero_si128())

else:
  import nimsimd/neon
  type
    Float4* = float32x4
    Word4* = uint32x4
    Mask4* = uint32x4

  func vcvtq_f32_u32(a: uint32x4): float32x4 {.importc, header: "arm_neon.h".}
  func vcvtq_f32_s32(a: int32x4): float32x4 {.importc, header: "arm_neon.h".}
  func vcvtq_u32_f32(a: float32x4): uint32x4 {.importc, header: "arm_neon.h".}
  func vcvtq_s32_f32(a: float32x4): int32x4 {.importc, header: "arm_neon.h".}
  func vmulq_u32(a, b: uint32x4): uint32x4 {.importc, header: "arm_neon.h".}
  func vreinterpretq_f32_u32(a: uint32x4): float32x4 {.importc, header: "arm_neon.h".}

  proc splat*(x: float32): Float4 {.inline.} =
    vmovq_n_f32(x)

  proc words*(x: uint32): Word4 {.inline.} =
    vmovq_n_u32(x)

  proc lanes*(): Float4 {.inline.} =
    var values = [0'f32, 1, 2, 3]
    vld1q_f32(values[0].addr)

  proc `+`*(a, b: Float4): Float4 {.inline.} =
    vaddq_f32(a, b)

  proc `-`*(a, b: Float4): Float4 {.inline.} =
    vsubq_f32(a, b)

  proc `*`*(a, b: Float4): Float4 {.inline.} =
    vmulq_f32(a, b)

  proc min4*(a, b: Float4): Float4 {.inline.} =
    vminq_f32(a, b)

  proc max4*(a, b: Float4): Float4 {.inline.} =
    vmaxq_f32(a, b)

  proc le4*(a, b: Float4): Mask4 {.inline.} =
    vcleq_f32(a, b)

  proc lt4*(a, b: Float4): Mask4 {.inline.} =
    vcltq_f32(a, b)

  proc and4*(a, b: Mask4): Mask4 {.inline.} =
    vandq_u32(a, b)

  proc choose*(mask: Mask4, a, b: Float4): Float4 {.inline.} =
    vbslq_f32(mask, a, b)

  proc chooseWords*(mask: Mask4, a, b: Word4): Word4 {.inline.} =
    vbslq_u32(mask, a, b)

  proc addWords*(a, b: Word4): Word4 {.inline.} =
    vaddq_u32(a, b)

  proc subWords*(a, b: Word4): Word4 {.inline.} =
    vsubq_u32(a, b)

  proc mulWords*(a, b: Word4): Word4 {.inline.} =
    vmulq_u32(a, b)

  proc andWords*(a, b: Word4): Word4 {.inline.} =
    vandq_u32(a, b)

  proc orWords*(a, b: Word4): Word4 {.inline.} =
    vorrq_u32(a, b)

  template shrWords*(a: Word4, n: static int): Word4 =
    vshrq_n_u32(a, n)

  template shlWords*(a: Word4, n: static int): Word4 =
    vshlq_n_u32(a, n)

  proc toWords*(a: Float4): Word4 {.inline.} =
    vcvtq_u32_f32(a)

  proc toFloats*(a: Word4): Float4 {.inline.} =
    vcvtq_f32_u32(a)

  proc asFloats*(a: Word4): Float4 {.inline.} =
    vreinterpretq_f32_u32(a)

  proc loadWords*(p: pointer): Word4 {.inline.} =
    vld1q_u32(cast[ptr uint32](p))

  proc storeWords*(p: pointer, a: Word4) {.inline.} =
    vst1q_u32(cast[ptr uint32](p), a)

  proc loadFloats*(p: ptr float32): Float4 {.inline.} =
    vld1q_f32(p)

  proc storeFloats*(p: ptr float32, a: Float4) {.inline.} =
    vst1q_f32(p, a)

  proc maskBytes*(p: ptr UncheckedArray[uint8]): Word4 {.inline.} =
    var values = [p[0].uint32, p[1].uint32, p[2].uint32, p[3].uint32]
    vld1q_u32(values[0].addr)

proc rounded*(a: Float4): Float4 {.inline.} =
  toFloats(toWords(a + splat(0.5)))

proc div255*(a: Word4): Word4 {.inline.} =
  ## Exact rounded division for unsigned byte-channel products (0..65025).
  let biased = addWords(a, words(128))
  shrWords(addWords(biased, shrWords(biased, 8)), 8)

proc expNegative*(a: Float4): Float4 {.inline.} =
  ## Range reduction and a degree-five Taylor polynomial for exp on [-80, 0].
  ## Error is below byte quantization; tails below exp(-80) round to zero.
  let x = max4(splat(-80), min4(a, splat(0)))
  let exponent = x * splat(1.4426950408889634'f32) - splat(0.5)
  when defined(amd64):
    let nf = mm_cvtepi32_ps(mm_cvttps_epi32(exponent))
  else:
    let nf = vcvtq_f32_s32(vcvtq_s32_f32(exponent))
  let scale = asFloats(shlWords(toWords(nf + splat(127)), 23))
  let r = x - nf * splat(0.6931471805599453'f32)
  var p = splat(1'f32 / 120)
  p = p * r + splat(1'f32 / 24)
  p = p * r + splat(1'f32 / 6)
  p = p * r + splat(0.5)
  p = p * r + splat(1)
  p = p * r + splat(1)
  p * scale
