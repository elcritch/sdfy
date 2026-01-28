# Package

version       = "0.7.8"
author        = "Jaremy Creechley"
description   = "A package implementing signed distance functions"
license       = "Apache-2.0"
srcDir        = "src"


# Dependencies
requires "nimsimd"
requires "chroma"
requires "vmath"

feature "test":
  requires "pixie >= 0.5.0"

feature "msdf":
  requires "pixie >= 0.5.0"
feature "msdf-reference":
  requires "https://github.com/Chlumsky/msdfgen"
  requires "https://github.com/ShoYamanishi/SDFont"

