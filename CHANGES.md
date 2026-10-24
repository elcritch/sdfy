# Changes

## 0.8.5

- Evaluate elliptical rounded-box distances entirely in SSE2 and NEON vectors,
  including mixed circular, elliptical, and sharp corners. Skip expensive
  corner math for straight-edge batches and ellipse math for circular batches.
- Keep the low-level distance entry point available through the fallback
  modules for compatibility, without their former per-lane scalar calls.

## 0.8.4

- Add elliptical rounded boxes with independent horizontal and vertical corner
  radii, including scalar, NEON, and SSE2 raster dispatch support.
