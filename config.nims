--nimcache:".nimcache"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"
  exec "nim c -r tests/test1_simd.nim"
  exec "nim c -r tests/test_msdf.nim"
  exec "nim c -r tests/test_msdf_compare.nim"
