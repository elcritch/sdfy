import nimsimd/hassimd

export hassimd

const allowSimd* = not defined(sdfyNoSimd) and not defined(tcc)

when allowSimd:
  when defined(amd64) or defined(x86_64):
    import ./sse2/shapesSse2
    # import ./sse2/fallbackShapesSse2
    import ./sse2/drawSdfShapeSse2
    export drawSdfShapeSse2
    export shapesSse2
    # export fallbackShapesSse2

  elif defined(arm64) or defined(aarch64) or defined(arm):
    import ./neon/shapesNeon
    import ./neon/fallbackShapesNeon
    import ./neon/drawSdfShapeNeon
    export drawSdfShapeNeon
    export shapesNeon
    export fallbackShapesNeon

