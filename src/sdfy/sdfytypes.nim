import chroma

type
  SDFMode* = enum
    sdfModeFeather
    sdfModeFeatherInv
    sdfModeClip
    sdfModeClipAntiAlias
    sdfModeFeatherGaussian
    sdfModeDropShadow

  SdfImage* = ref object
    ## Image object that holds bitmap data in premultiplied alpha RGBA format.
    ## compatible with Pixie's Image type
    width*, height*: int
    data*: seq[ColorRGBX]

proc newSdfImage*(width, height: int): SdfImage {.raises: [ValueError].} =
  ## Creates a new image with the parameter dimensions.
  if width <= 0 or height <= 0:
    raise newException(ValueError, "SdfImage width and height must be > 0")

  result = SdfImage()
  result.width = width
  result.height = height
  result.data = newSeq[ColorRGBX](width * height)

proc copy*(image: SdfImage): SdfImage {.raises: [].} =
  ## Copies the image data into a new image.
  result = SdfImage()
  result.width = image.width
  result.height = image.height
  result.data = image.data

template dataIndex*[I](image: I, x, y: int): int =
  image.width * y + x
