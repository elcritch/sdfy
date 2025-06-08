import chroma
import vmath

type
  SDFMode* = enum
    sdfModeFeather
    sdfModeFeatherInv
    sdfModeClip
    sdfModeClipAA
    sdfModeFeatherGaussian
    sdfModeDropShadow
    sdfModeInsetShadow
    sdfModeInsetShadowAnnular
    sdfModeAnnular
    sdfModeAnnularAA

  SdfImage* = ref object
    ## Image object that holds bitmap data in premultiplied alpha RGBA format.
    ## compatible with Pixie's Image type
    width*, height*: int
    data*: seq[ColorRGBX]

  RoundedBoxParams* = object
    ## Parameters for rounded box SDF
    r*: Vec4  ## corner radii as Vec4 (x=top-right, y=bottom-right, z=bottom-left, w=top-left)
  
  ChamferBoxParams* = object
    ## Parameters for chamfer box SDF
    chamfer*: float32  ## chamfer amount

  CircleParams* = object
    ## Parameters for circle SDF
    r*: float32  ## radius

  BezierParams* = object
    ## Parameters for quadratic Bézier curve SDF
    A*: Vec2  ## first control point
    B*: Vec2  ## second control point  
    C*: Vec2  ## third control point

  BoxParams* = object
    ## Parameters for box SDF
    b*: Vec2  ## box half-extents (width/2, height/2)

  EllipseParams* = object
    ## Parameters for ellipse SDF
    ab*: Vec2  ## ellipse semi-axes (width/2, height/2)

  ArcParams* = object
    ## Parameters for arc SDF
    sc*: Vec2  ## sin/cos of the arc's aperture
    ra*: float32  ## inner radius
    rb*: float32  ## thickness (outer radius difference)

  ParallelogramParams* = object
    ## Parameters for parallelogram SDF
    wi*: float32  ## width
    he*: float32  ## height
    sk*: float32  ## skew

  PieParams* = object
    ## Parameters for pie SDF
    c*: Vec2  ## sin/cos of the pie's aperture
    r*: float32  ## radius

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
