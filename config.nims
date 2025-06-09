
task test, "Run tests":
  exec "nim c -r tests/test1.nim"
  exec "nim c -r tests/test1_simd.nim"
