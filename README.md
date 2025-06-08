# SDFY - Signed Distance Functions for Nim

A high-performance Nim library implementing 2D signed distance functions (SDFs) with multiple rendering modes and SIMD optimizations.

## Features

- **Fast SDF Implementation**: Optimized implementations of common 2D shapes
- **Multiple Rendering Modes**: Support for various anti-aliasing and effect techniques
- **SIMD Acceleration**: Leverages SIMD instructions for maximum performance
- **Pixie Integration**: Seamless integration with the Pixie graphics library
- **Flexible API**: Easy-to-use interface for rendering SDFs to images

## Supported Shapes

- **Rounded Rectangle**: Fully configurable rounded rectangles with independent corner radii
- **Chamfer Box**: Rectangles with chamfered (cut) corners
- **Circle**: Perfect circles with configurable radius

## Rendering Modes

- **Clip**: Sharp edges without anti-aliasing
- **Clip AA**: Sharp edges with anti-aliasing  
- **Annular**: Creates ring/annular shapes
- **Annular AA**: Anti-aliased ring shapes
- **Feather**: Standard anti-aliased edges with customizable factor
- **Feather Inverse**: Inverted feather anti-aliasing
- **Feather Gaussian**: Gaussian-based anti-aliasing for smooth edges
- **Drop Shadow**: Gaussian-based drop shadow effects
- **Inset Shadow**: Inner shadow effects
- **Inset Shadow Annular**: Annular inner shadow effects

## Performance

| Mode | With SIMD | Without SIMD | Speedup |
|------|-----------|--------------|---------|
| **Pixie Shadow** | **456 ms** | **476 ms** | **1.0x** |
| Clip | 5 ms | 20 ms | 4.0x |
| Clip AA | 6 ms | 30 ms | 5.0x |
| Annular | 5 ms | 22 ms | 4.4x |
| Annular AA | 6 ms | 33 ms | 5.5x |
| Feather | 6 ms | 23 ms | 3.8x |
| Feather Inverse | 6 ms | 26 ms | 4.3x |
| Feather Gaussian | 7 ms | 24 ms | 3.4x |
| Drop Shadow | 7 ms | 24 ms | 3.4x |
| Inset Shadow | 8 ms | 24 ms | 3.0x |
| Inset Shadow Annular | 7 ms | 24 ms | 3.4x |

*Performance measured on rounded rectangles (300x300 image). SIMD provides 3-5x performance improvement. **SDF functions are 15-65x faster than traditional Pixie rendering with shadows.***

## Visual Examples

Here are examples of the different rendering modes applied to rounded rectangles:

### Clip Mode (Sharp Edges)
![Clip Mode](tests/outputs/rounded_box_clip.png)

### Clip AA Mode (Anti-aliased Edges)
![Clip AA Mode](tests/outputs/rounded_box_clip_aa.png)

### Annular Mode (Ring Shape)
![Annular Mode](tests/outputs/rounded_box_annular.png)

### Annular AA Mode (Anti-aliased Ring)
![Annular AA Mode](tests/outputs/rounded_box_annular_aa.png)

### Feather Mode (Standard Anti-aliasing)
![Feather Mode](tests/outputs/rounded_box_feather.png)

### Feather Inverse Mode (Inverted Anti-aliasing)
![Feather Inverse Mode](tests/outputs/rounded_box_feather_inv.png)

### Feather Gaussian Mode (Gaussian Anti-aliasing)
![Feather Gaussian Mode](tests/outputs/rounded_box_feather_gaussian.png)

### Drop Shadow Mode
![Drop Shadow Mode](tests/outputs/rounded_box_drop_shadow.png)

### Inset Shadow Mode
![Inset Shadow Mode](tests/outputs/rounded_box_inset_shadow.png)

### Inset Shadow Annular Mode
![Inset Shadow Annular Mode](tests/outputs/rounded_box_inset_shadow_annular.png)

### Pixie Comparison (Traditional Graphics)
![Pixie Comparison](tests/outputs/rounded_box_pixie.png)

## Installation

Add to your `.nimble` file:

```nim
requires "sdfy"
```

Or install directly:

```bash
nimble install sdfy
```

## Quick Start

```nim
import pixie
import sdfy

let image = newImage(300, 300)
let center = vec2(150.0, 150.0)
let size = vec2(200.0, 200.0)
let corners = vec4(0.0, 20.0, 40.0, 80.0)  # Different radius per corner
let fillColor = rgba(255, 0, 0, 255)        # Red fill
let bgColor = rgba(0, 0, 255, 255)          # Blue background

# Render a rounded rectangle with anti-aliasing
drawSdfShape(
  image,
  center = center,
  wh = size,
  params = RoundedBoxParams(r: corners),
  pos = fillColor,
  neg = bgColor,
  mode = sdfModeFeatherInv
)

image.writeFile("output.png")
```

## API Reference

### Core Functions

#### `sdRoundedBox(p: Vec2, b: Vec2, r: Vec4): float32`

Calculate the signed distance from a point to a rounded rectangle.

- `p`: Point to test
- `b`: Box half-extents (width/2, height/2)  
- `r`: Corner radii as Vec4 (x=top-right, y=bottom-right, z=bottom-left, w=top-left)
- Returns: Signed distance (negative inside, positive outside)

#### `sdChamferBox(p: Vec2, b: Vec2, chamfer: float32): float32`

Calculate the signed distance from a point to a chamfered rectangle.

- `p`: Point to test
- `b`: Box half-extents (width/2, height/2)
- `chamfer`: Chamfer amount
- Returns: Signed distance (negative inside, positive outside)

#### `sdCircle(p: Vec2, r: float32): float32`

Calculate the signed distance from a point to a circle.

- `p`: Point to test
- `r`: Circle radius
- Returns: Signed distance (negative inside, positive outside)

#### `drawSdfShape(image, center, wh, params, pos, neg, factor, spread, mode)`

Generic function to render shapes to an image using SDF.

- `image`: Target image to render to
- `center`: Center position of the shape
- `wh`: Width and height of the shape (ignored for circles)
- `params`: Shape parameters (RoundedBoxParams, ChamferBoxParams, or CircleParams)
- `pos`: Color for inside the shape
- `neg`: Color for outside the shape
- `factor`: Anti-aliasing factor (default: 4.0)
- `spread`: Spread amount for shadow effects (default: 0.0)
- `mode`: Rendering mode (see SDFMode enum)

### Shape Parameters

```nim
type
  RoundedBoxParams* = object
    r*: Vec4  # corner radii (top-right, bottom-right, bottom-left, top-left)
  
  ChamferBoxParams* = object
    chamfer*: float32  # chamfer amount
  
  CircleParams* = object
    r*: float32  # radius
```

### Rendering Modes

```nim
type SDFMode* = enum
  sdfModeFeather              # Standard anti-aliasing
  sdfModeFeatherInv           # Inverted anti-aliasing
  sdfModeClip                 # Sharp edges without anti-aliasing
  sdfModeClipAA               # Sharp edges with anti-aliasing
  sdfModeFeatherGaussian      # Gaussian anti-aliasing
  sdfModeDropShadow           # Drop shadow effect
  sdfModeInsetShadow          # Inset shadow effect
  sdfModeInsetShadowAnnular   # Annular inset shadow effect
  sdfModeAnnular              # Ring/annular shape
  sdfModeAnnularAA            # Anti-aliased ring/annular shape
```

## Examples

### Basic Rounded Rectangle

```nim
import pixie, sdfy

let image = newImage(300, 300)
drawSdfShape(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),
  params = RoundedBoxParams(r: vec4(20, 20, 20, 20)),  # 20px radius on all corners
  pos = rgba(255, 100, 100, 255),
  neg = rgba(50, 50, 50, 255),
  mode = sdfModeFeatherInv
)
```

### Asymmetric Corners

```nim
# Different radius for each corner
let corners = vec4(
  0.0,   # top-right: sharp corner
  20.0,  # bottom-right: small radius
  40.0,  # bottom-left: medium radius  
  80.0   # top-left: large radius
)

drawSdfShape(
  image,
  center = center,
  wh = size,
  params = RoundedBoxParams(r: corners),
  pos = fill,
  neg = bg,
  mode = sdfModeFeatherInv
)
```

### Drop Shadow Effect

```nim
drawSdfShape(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),
  params = RoundedBoxParams(r: vec4(20, 20, 20, 20)),
  pos = rgba(255, 255, 255, 255),
  neg = rgba(0, 0, 0, 0),
  factor = 10.0,
  spread = 20.0,
  mode = sdfModeDropShadow
)
```

### Chamfer Box

```nim
drawSdfShape(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),
  params = ChamferBoxParams(chamfer: 20.0),
  pos = rgba(255, 100, 100, 255),
  neg = rgba(50, 50, 50, 255),
  mode = sdfModeFeatherInv
)
```

### Circle

```nim
drawSdfShape(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),  # ignored for circles
  params = CircleParams(r: 100.0),
  pos = rgba(255, 100, 100, 255),
  neg = rgba(50, 50, 50, 255),
  mode = sdfModeFeatherInv
)
```

### Annular (Ring) Shapes

```nim
# Create a ring shape with the annular mode
drawSdfShape(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),
  params = CircleParams(r: 100.0),
  pos = rgba(255, 100, 100, 255),
  neg = rgba(50, 50, 50, 255),
  factor = 20.0,  # controls ring thickness
  mode = sdfModeAnnularAA
)
```

## Performance

SDFY is designed for high performance:

- **SIMD optimizations**: 3-5x performance improvement on supported platforms
- **Efficient memory layout**: Optimized data structures for cache efficiency
- **Minimal allocations**: Reuses memory where possible
- **Optimized math operations**: Fast implementations of mathematical functions

## Inspiration

This library is based on the excellent work by [Íñigo Quílez](https://iquilezles.org/articles/distfunctions2d/) on 2D distance functions.

## License

Licensed under the Apache License 2.0. See LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests. 