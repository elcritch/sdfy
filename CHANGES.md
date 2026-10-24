# Changes

## 0.8.5

- Evaluate elliptical rounded-box distances entirely in SSE2 and NEON vectors,
  including mixed circular, elliptical, and sharp corners. Skip expensive
  corner math for straight-edge batches and ellipse math for circular batches.
- Keep the low-level distance entry point available through the fallback
  modules for compatibility, without their former per-lane scalar calls.
- Reduce circular rounded-box absolute-value operations on SSE2 and replace
  ARM32's scalar square-root fallback with vector Newton refinement.
- Add a borrowed span renderer with solid, two-/three-stop linear, and
  four-corner paints, affine coordinates, clipping, byte/RGBA alpha masks,
  fill/stroke/shadow coverage, and premultiplied source-over or replacement.
  SSE2 and NEON vectorize coordinates, distances, paints, coverage, mask
  multiplication, and blending; short tails and disabled SIMD use scalar code.
- Add an image convenience wrapper and a distance-only packet API sharing the
  prepared rounded-box fields. Existing drawSdfShape behavior is unchanged.

## 0.8.4

- Add elliptical rounded boxes with independent horizontal and vertical corner
  radii, including scalar, NEON, and SSE2 raster dispatch support.
