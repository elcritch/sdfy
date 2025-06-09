import std/math
import pixie
import sdfy

let
  # Grid configuration
  gridCols = 5  # 5 columns for 5 shapes per row
  gridRows = 4  # 4 rows total: 2 modes x 2 rows of shapes
  cellWidth = 240
  cellHeight = 240
  margin = 20
  shapeSize = 160.0
  
  # Calculate total image dimensions
  imageWidth = gridCols * cellWidth + margin * (gridCols - 1)
  imageHeight = gridRows * cellHeight + margin * (gridRows - 1)
  
  image = newImage(imageWidth, imageHeight)
  
  # Colors
  pos = rgba(255, 100, 100, 255)  # Red for positive (inside)
  neg = rgba(0, 0, 0, 0)     # Dark gray for negative (outside)
  
  # Shape parameters - scaled to fit in cells nicely
  roundedBoxParams = RoundedBoxParams(r: vec4(20.0, 20.0, 20.0, 20.0))
  chamferBoxParams = ChamferBoxParams(chamfer: 15.0)
  circleParams = CircleParams(r: 60.0)
  bezierParams = BezierParams(
    A: vec2(40.0, 60.0),    # Start point (relative to center)
    B: vec2(0.0, -40.0),    # Control point
    C: vec2(-40.0, 60.0)    # End point
  )
  boxParams = BoxParams(b: vec2(60.0, 45.0))  # 120x90 pixel box
  ellipseParams = EllipseParams(ab: vec2(70.0, 45.0))  # 140x90 pixel ellipse
  arcParams = ArcParams(
    sc: vec2(sin(PI/4), cos(PI/4)),  # 45 degree aperture
    ra: 50.0,   # inner radius
    rb: 15.0    # thickness
  )
  parallelogramParams = ParallelogramParams(
    wi: 60.0,   # width
    he: 45.0,   # height
    sk: 15.0    # skew
  )
  pieParams = PieParams(
    c: vec2(sin(PI/3), cos(PI/3)),  # 60 degree aperture
    r: 60.0   # radius
  )
  ringParams = RingParams(
    n: vec2(sin(PI/4), cos(PI/4)),  # 45 degree aperture
    r: 60.0,   # radius
    th: 15.0   # thickness
  )

proc main() =
  # Fill background with white
  let ctx = newContext(image)
  ctx.fillStyle = rgba(255, 255, 255, 55)
  ctx.fillRect(rect(0, 0, imageWidth.float32, imageHeight.float32))
  
  # Define the modes to test
  let modes = [
    # (mode: sdfModeClipAA, name: "ClipAA", factor: 12.0, spread: 0.0, posColor: pos, negColor: neg),
    (mode: sdfModeClipRgbSubPixelAA, name: "ClipAA", factor: 12.0, spread: 0.0, posColor: pos, negColor: neg),
    (mode: sdfModeDropShadowAA, name: "DropShadowAA", factor: 12.0, spread: 5.0, posColor: pos, negColor: rgba(0, 0, 0, 255))
  ]
  
  # Define all shapes with their parameters
  let shapes = [
    ("RoundedBox", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), roundedBoxParams, posColor, negColor, mode, factor, spread)),
    ("ChamferBox", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), chamferBoxParams, posColor, negColor, mode, factor, spread)),
    ("Circle", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), circleParams, posColor, negColor, mode, factor, spread)),
    ("Bezier", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), bezierParams, posColor, negColor, mode, factor, spread)),
    ("Box", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), boxParams, posColor, negColor, mode, factor, spread)),
    ("Ellipse", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), ellipseParams, posColor, negColor, mode, factor, spread)),
    ("Arc", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), arcParams, posColor, negColor, mode, factor, spread)),
    ("Parallelogram", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), parallelogramParams, posColor, negColor, mode, factor, spread)),
    ("Pie", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), pieParams, posColor, negColor, mode, factor, spread)),
    ("Ring", proc(tempImage: Image, center: Vec2, mode: SDFMode, factor: float32, spread: float32, posColor: ColorRGBA, negColor: ColorRGBA) =
      drawSdfShape(tempImage, center, vec2(shapeSize, shapeSize), ringParams, posColor, negColor, mode, factor, spread))
  ]
  
  # Draw all shapes in grid layout
  for modeIndex, mode in modes:
    for shapeIndex, shape in shapes:
      let
        col = shapeIndex mod gridCols  # Wrap to next row after 5 shapes
        row = modeIndex * 2 + (shapeIndex div gridCols)  # Two rows per mode
        gridX = col * cellWidth + col * margin
        gridY = row * cellHeight + row * margin
        
        # Create a temporary image for this shape
        tempImage = newImage(cellWidth, cellHeight)
        tempCtx = newContext(tempImage)
        tempCenter = vec2(cellWidth.float32 / 2, cellHeight.float32 / 2)
      
      echo "Drawing ", shape[0], " at grid position (", gridX, ", ", gridY, ") with mode ", mode.name
      
      # Fill temp image with white background
      tempCtx.fillStyle = rgba(255, 255, 255, 255)
      tempCtx.fillRect(rect(0, 0, cellWidth.float32, cellHeight.float32))
      
      # Draw shape on temp image
      shape[1](tempImage, tempCenter, mode.mode, mode.factor, mode.spread, mode.posColor, mode.negColor)
      
      # Composite temp image onto main grid image using translation transform
      let translation = translate(vec2(gridX.float32, gridY.float32))
      image.draw(tempImage, translation)

  # Save the combined image
  image.writeFile("tests/outputs/all_shapes_grid.png")
  echo "Generated combined grid image: tests/outputs/all_shapes_grid.png"
  echo "Grid layout: All 10 shapes shown in both modes"
  echo "Row 1 (ClipAA): RoundedBox, ChamferBox, Circle, Bezier, Box"
  echo "Row 2 (ClipAA): Ellipse, Arc, Parallelogram, Pie, Ring"
  echo "Row 3 (DropShadow): RoundedBox, ChamferBox, Circle, Bezier, Box"
  echo "Row 4 (DropShadow): Ellipse, Arc, Parallelogram, Pie, Ring"

main() 