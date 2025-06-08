import nimsimd/hassimd

export hassimd

const allowSimd* = not defined(sdfyNoSimd) and not defined(tcc)

when allowSimd:
  when defined(amd64):
    import simd/sse2
    export sse2

    when not defined(sdfyNoAvx):
      import nimsimd/runtimecheck
      import roundedbox/avx, roundedbox/avx2
      export avx, avx2

      let
        cpuHasAvx* = checkInstructionSets({AVX})
        cpuHasAvx2* = checkInstructionSets({AVX, AVX2})

    import nimsimd/sse2 as nimsimdsse2
    export nimsimdsse2

  elif defined(arm64) or defined(aarch64) or defined(arm):
    import ./neon/drawSdfShapeNeon
    import ./neon/shapesNeon
    export drawSdfShapeNeon
    export shapesNeon

