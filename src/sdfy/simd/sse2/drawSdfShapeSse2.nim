import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/sse2

import ../../sdfytypes
import ./shapesSse2

when defined(release):
  {.push checks: off.}


when defined(release):
  {.pop.}
