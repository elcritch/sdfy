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

## Rendering Modes

- **Clip**: Sharp edges (aliasing TBD)
- **Feather**: Anti-aliased edges with customizable factor
- **Feather Inverse**: Inverted feather anti-aliasing
- **Feather Gaussian**: Gaussian-based anti-aliasing for smooth edges  
- **Drop Shadow**: Gaussian-based drop shadow effects

## Visual Examples

Here are examples of the different rendering modes applied to rounded rectangles:

### Clip Mode (Sharp Edges)
![Clip Mode](data/rounded_box_clip.png)

### Feather Mode (Standard Anti-aliasing)
![Feather Mode](data/rounded_box_feather.png)

### Feather Inverse Mode (Inverted Anti-aliasing)
![Feather Inverse Mode](data/rounded_box_feather_inv.png)

### Feather Gaussian Mode (Gaussian Anti-aliasing)
![Feather Gaussian Mode](data/rounded_box_feather_gaussian.png)

### Drop Shadow Mode
![Drop Shadow Mode](data/rounded_box_drop_shadow.png)

### Pixie Comparison (Traditional Graphics)
![Pixie Comparison](data/rounded_box_pixie.png)

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
signedRoundedBox(
  image,
  center = center,
  wh = size,
  r = corners,
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

#### `signedRoundedBox(image, center, wh, r, pos, neg, factor, spread, mode)`

Render a rounded rectangle to an image using SDF.

- `image`: Target image to render to
- `center`: Center position of the rectangle
- `wh`: Width and height of the rectangle
- `r`: Corner radii (Vec4)
- `pos`: Color for inside the shape
- `neg`: Color for outside the shape
- `factor`: Anti-aliasing factor (default: 4.0)
- `spread`: Spread amount for shadow effects (default: 0.0)
- `mode`: Rendering mode (see SDFMode enum)

### Rendering Modes

```nim
type SDFMode = enum
  sdfModeClip           # Sharp edges, no anti-aliasing
  sdfModeFeather        # Standard anti-aliasing
  sdfModeFeatherInv     # Inverted anti-aliasing  
  sdfModeFeatherGaussian # Gaussian anti-aliasing
  sdfModeDropShadow     # Drop shadow effect
```

## Examples

### Basic Rounded Rectangle

```nim
import pixie, sdfy

let image = newImage(300, 300)
signedRoundedBox(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200), 
  r = vec4(20, 20, 20, 20),  # 20px radius on all corners
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

signedRoundedBox(image, center, size, corners, fill, bg, mode = sdfModeFeatherInv)
```

### Drop Shadow Effect

```nim
signedRoundedBox(
  image,
  center = vec2(150, 150),
  wh = vec2(200, 200),
  r = vec4(20, 20, 20, 20),
  pos = rgba(255, 255, 255, 255),
  neg = rgba(0, 0, 0, 0),
  factor = 10.0,
  spread = 20.0,
  mode = sdfModeDropShadow
)
```

## Performance

SDFY is designed for high performance:

- SIMD optimizations for batch processing
- Efficient memory layout
- Minimal allocations
- Optimized math operations

## Inspiration

This library is based on the excellent work by [Íñigo Quílez](https://iquilezles.org/articles/distfunctions2d/) on 2D distance functions.

## License

Licensed under the Apache License 2.0. See LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests. 