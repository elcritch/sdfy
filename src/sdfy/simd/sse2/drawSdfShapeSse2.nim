import std/math
import vmath, chroma, pixie
import nimsimd/hassimd, nimsimd/sse2

import ../../sdfytypes
import ./shapesSse2

when defined(release):
  {.push checks: off.}

proc drawSdfShapeSse2*[I, T](
    image: I,
    center: Vec2,
    wh: Vec2,
    params: T,
    pos: ColorRGBA,
    neg: ColorRGBA,
    mode: SDFMode,
    factor: float32 = 4.0,
    spread: float32 = 0.0,
    pointOffset: Vec2 = vec2(0.2, 0.2)
) {.simd, raises: [].} =
  ## SSE2 SIMD optimized version of drawSdfShape
  ## Generic function that supports rounded boxes (other shapes to be added later)
  ## Processes pixels in chunks of 4 with padding for remaining pixels
  ## T: RoundedBoxParams (other shape types to be added later)
  mixin dataIndex
  
  echo "SSE2 SIMD implemented"
  let
    pos_rgbx = pos.rgbx()
    neg_rgbx = neg.rgbx()
    center_x = center.x
    center_y = center.y
    # SSE2 constants
    zero_vec = mm_setzero_ps()
    factor_vec = mm_set1_ps(factor)
    offset_vec = mm_set1_ps(127.0)
    f255_vec = mm_set1_ps(255.0)
    half_vec = mm_set1_ps(0.5)
    one_vec = mm_set1_ps(1.0)
    # Point offset constants
    pointOffset_x = pointOffset.x
    pointOffset_y = pointOffset.y
  
  for y in 0 ..< image.height:
    let
      py_scalar = y.float32 - center_y + pointOffset_y
      py_vec = mm_set1_ps(py_scalar)
      row_start = image.dataIndex(0, y)
    
    var x = 0
    
    # Process all pixels in chunks of 4, with padding for the last chunk
    while x < image.width:
      # Calculate how many pixels we actually need to process in this chunk
      let remainingPixels = min(4, image.width - x)
      
      # Calculate px for up to 4 pixels, padding with the last valid pixel
      var px_array: array[4, float32]
      for i in 0..3:
        let actualX = if i < remainingPixels: x + i else: x + remainingPixels - 1
        px_array[i] = actualX.float32 - center_x + pointOffset_x
      
      let px_vec = mm_loadu_ps(px_array[0].addr)
      
      # Calculate signed distances for 4 pixels using appropriate SDF function
      let sd_vec = when T is RoundedBoxParams:
        let
          b_x = wh.x / 2.0
          b_y = wh.y / 2.0
        sdRoundedBoxSimd(px_vec, py_vec, b_x, b_y, params.r)
      elif T is ChamferBoxParams:
        let
          b_x = wh.x / 2.0
          b_y = wh.y / 2.0
        sdChamferBoxSimd(px_vec, py_vec, b_x, b_y, params.chamfer)
      elif T is CircleParams:
        sdCircleSimd(px_vec, py_vec, params.r)
      elif T is BoxParams:
        sdBoxSimd(px_vec, py_vec, params.b.x, params.b.y)
      elif T is ArcParams:
        sdArcSimd(px_vec, py_vec, params.sc.x, params.sc.y, params.ra, params.rb)
      elif T is ParallelogramParams:
        sdParallelogramSimd(px_vec, py_vec, params.wi, params.he, params.sk)
      elif T is PieParams:
        sdPieSimd(px_vec, py_vec, params.c.x, params.c.y, params.r)
      elif T is RingParams:
        sdRingSimd(px_vec, py_vec, params.n.x, params.n.y, params.r, params.th)
      else:
        {.error: "Unsupported shape parameter type in SSE2 implementation. BezierParams and EllipseParams use scalar fallbacks and are not yet implemented.".}
      
      # Extract individual values for color selection and mode processing
      var sd_array: array[4, float32]
      mm_storeu_ps(sd_array[0].addr, sd_vec)
      
      case mode:
      of sdfModeClip:
        # Clipped mode: use solid colors based on SDF sign
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            final_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            idx = row_start + x + i
          
          image.data[idx] = final_color

      of sdfModeClipAA:
        # Anti-aliased clip mode: mix colors based on clamped (sd + 0.5)
        # cl = clamp(sd + 0.5, 0.0, 1.0)
        # c = mix(pos, neg, cl)
        let
          offset_sd = mm_add_ps(sd_vec, half_vec)
          clamped_low = mm_max_ps(offset_sd, zero_vec)
          clamped = mm_min_ps(clamped_low, one_vec)
        
        # Extract clamped values for color mixing
        var clamped_array: array[4, float32]
        mm_storeu_ps(clamped_array[0].addr, clamped)
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            cl = clamped_array[i]
            # mix(pos, neg, cl) = pos * (1 - cl) + neg * cl
            mixed_color = mix(pos_rgbx, neg_rgbx, cl)
            idx = row_start + x + i
          
          image.data[idx] = mixed_color

      of sdfModeAnnular:
        # Annular mode: create ring shape
        # sd = abs(sd + factor) - factor
        # Calculate abs(sd + factor) using SSE2: abs(x) = max(x, -x)
        let
          offset_sd = mm_add_ps(sd_vec, factor_vec)
          neg_offset_sd = mm_sub_ps(zero_vec, offset_sd)
          abs_offset_sd = mm_max_ps(offset_sd, neg_offset_sd)
          annular_sd = mm_sub_ps(abs_offset_sd, factor_vec)
        
        # Extract annular sd values for color selection
        var annular_sd_array: array[4, float32]
        mm_storeu_ps(annular_sd_array[0].addr, annular_sd)
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = annular_sd_array[i]
            final_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            idx = row_start + x + i
          
          image.data[idx] = final_color

      of sdfModeAnnularAA:
        # Anti-aliased annular mode: create ring shape with anti-aliasing
        # sd = abs(sd + factor) - factor
        # cl = clamp(sd + 0.5, 0.0, 1.0)
        # c = mix(pos, neg, cl)
        let
          offset_sd = mm_add_ps(sd_vec, factor_vec)
          neg_offset_sd = mm_sub_ps(zero_vec, offset_sd)
          abs_offset_sd = mm_max_ps(offset_sd, neg_offset_sd)
          annular_sd = mm_sub_ps(abs_offset_sd, factor_vec)
          
          offset_annular_sd = mm_add_ps(annular_sd, half_vec)
          clamped_low = mm_max_ps(offset_annular_sd, zero_vec)
          clamped = mm_min_ps(clamped_low, one_vec)
        
        # Extract values for color selection and mixing
        var clamped_array: array[4, float32]
        mm_storeu_ps(clamped_array[0].addr, clamped)
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            cl = clamped_array[i]
            # mix(pos, neg, cl) = pos * (1 - cl) + neg * cl
            mixed_color = mix(pos_rgbx, neg_rgbx, cl)
            idx = row_start + x + i
          
          image.data[idx] = mixed_color

      of sdfModeFeather:
        # Feathered mode: calculate alpha values using SIMD
        # Calculate alpha values: uint8(max(0.0, min(255, (factor*sd) + 127)))
        let
          scaled_sd = mm_mul_ps(sd_vec, factor_vec)
          alpha_float = mm_add_ps(scaled_sd, offset_vec)
          alpha_clamped_low = mm_max_ps(alpha_float, zero_vec)
          alpha_clamped = mm_min_ps(alpha_clamped_low, f255_vec)
        
        # Convert to integers and extract
        let alpha_i32 = mm_cvtps_epi32(alpha_clamped)
        var alpha_array: array[4, int32]
        mm_storeu_si128(cast[ptr M128i](alpha_array[0].addr), alpha_i32)
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i].uint8
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color

      of sdfModeFeatherInv:
        # Inverted feathered mode: calculate alpha values using SIMD then invert
        # Calculate alpha values: 255 - uint8(max(0.0, min(255, (factor*sd) + 127)))
        let
          scaled_sd = mm_mul_ps(sd_vec, factor_vec)
          alpha_float = mm_add_ps(scaled_sd, offset_vec)
          alpha_clamped_low = mm_max_ps(alpha_float, zero_vec)
          alpha_clamped = mm_min_ps(alpha_clamped_low, f255_vec)
          # Invert alpha by subtracting from 255
          alpha_inverted = mm_sub_ps(f255_vec, alpha_clamped)
        
        # Convert to integers and extract
        let alpha_i32 = mm_cvtps_epi32(alpha_inverted)
        var alpha_array: array[4, int32]
        mm_storeu_si128(cast[ptr M128i](alpha_array[0].addr), alpha_i32)
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i].uint8
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color

      of sdfModeFeatherGaussian:
        # Gaussian feathered mode: calculate Gaussian alpha values
        # f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        const
          s = 2.2'f32
          s_squared = s * s
          two_s_squared = 2.0'f32 * s_squared
          gaussian_coeff = 1.0'f32 / sqrt(2.0'f32 * PI * s_squared)
        
        # Calculate sd^2 using SIMD for all 4 pixels
        let sd_squared = mm_mul_ps(sd_vec, sd_vec)
        
        # Extract values for exponential calculation (no efficient SIMD exp available)
        var sd_squared_array: array[4, float32]
        mm_storeu_ps(sd_squared_array[0].addr, sd_squared)
        
        var alpha_array: array[4, uint8]
        for i in 0 ..< 4:
          let
            exp_val = exp(-1.0'f32 * sd_squared_array[i] / two_s_squared)
            f = gaussian_coeff * exp_val
            alpha_val = f * 255.0'f32
          alpha_array[i] = uint8(min(255.0'f32, max(0.0'f32, alpha_val)))
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i]
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color

      of sdfModeDropShadow:
        # Drop shadow mode: transform sd and apply Gaussian with conditional alpha
        # sd = sd / factor * s - spread / 8.8
        # f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        # alpha = if sd > 0.0: min(f * 255 * 6, 255) else: 255
        const
          s = 2.2'f32
          s_squared = s * s
          two_s_squared = 2.0'f32 * s_squared
          gaussian_coeff = 1.0'f32 / sqrt(2.0'f32 * PI * s_squared)
        
        # Transform sd values using SIMD: sd = sd / factor * s - spread / 8.8
        let
          s_vec = mm_set1_ps(s)
          spread_offset = mm_set1_ps(spread / 8.8'f32)
          # Use multiplication by reciprocal instead of division
          factor_reciprocal = mm_set1_ps(1.0'f32 / factor)
          transformed_sd = mm_sub_ps(
            mm_mul_ps(mm_mul_ps(sd_vec, factor_reciprocal), s_vec),
            spread_offset
          )
        
        # Calculate sd^2 using SIMD for all 4 pixels
        let sd_squared = mm_mul_ps(transformed_sd, transformed_sd)
        
        # Extract values for exponential calculation and conditional logic
        var 
          sd_squared_array: array[4, float32]
          transformed_sd_array: array[4, float32]
        mm_storeu_ps(sd_squared_array[0].addr, sd_squared)
        mm_storeu_ps(transformed_sd_array[0].addr, transformed_sd)
        
        var alpha_array: array[4, uint8]
        for i in 0 ..< 4:
          let
            transformed_sd_val = transformed_sd_array[i]
            exp_val = exp(-1.0'f32 * sd_squared_array[i] / two_s_squared)
            f = gaussian_coeff * exp_val
          
          if transformed_sd_val > 0.0'f32:
            let alpha_val = min(f * 255.0'f32 * 6.0'f32, 255.0'f32)
            alpha_array[i] = uint8(alpha_val)
          else:
            alpha_array[i] = 255'u8
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i]
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color

      of sdfModeInsetShadow:
        # Inset shadow mode: transform sd and apply Gaussian with conditional alpha
        # sd = sd / factor * s - spread / 8.8
        # f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        # alpha = if sd < 0.0: min(f * 255 * 6, 255) else: 255
        const
          s = 2.2'f32
          s_squared = s * s
          two_s_squared = 2.0'f32 * s_squared
          gaussian_coeff = 1.0'f32 / sqrt(2.0'f32 * PI * s_squared)
        
        # Transform sd values using SIMD: sd = sd / factor * s - spread / 8.8
        let
          s_vec = mm_set1_ps(s)
          spread_offset = mm_set1_ps(spread / 8.8'f32)
          # Use multiplication by reciprocal instead of division
          factor_reciprocal = mm_set1_ps(1.0'f32 / factor)
          transformed_sd = mm_sub_ps(
            mm_mul_ps(mm_mul_ps(sd_vec, factor_reciprocal), s_vec),
            spread_offset
          )
        
        # Calculate sd^2 using SIMD for all 4 pixels
        let sd_squared = mm_mul_ps(transformed_sd, transformed_sd)
        
        # Extract values for exponential calculation and conditional logic
        var 
          sd_squared_array: array[4, float32]
          transformed_sd_array: array[4, float32]
        mm_storeu_ps(sd_squared_array[0].addr, sd_squared)
        mm_storeu_ps(transformed_sd_array[0].addr, transformed_sd)
        
        var alpha_array: array[4, uint8]
        for i in 0 ..< 4:
          let
            transformed_sd_val = transformed_sd_array[i]
            exp_val = exp(-1.0'f32 * sd_squared_array[i] / two_s_squared)
            f = gaussian_coeff * exp_val
          
          if transformed_sd_val < 0.0'f32:
            let alpha_val = min(f * 255.0'f32 * 6.0'f32, 255.0'f32)
            alpha_array[i] = uint8(alpha_val)
          else:
            alpha_array[i] = 255'u8
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i]
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color

      of sdfModeInsetShadowAnnular:
        # Inset shadow annular mode: transform sd and apply Gaussian with conditional alpha
        # sd = sd / factor * s - spread / 8.8
        # f = 1 / sqrt(2 * PI * s^2) * exp(-1 * sd^2 / (2 * s^2))
        # alpha = if sd < 0.0: min(f * 255 * 6, 255) else: 0
        const
          s = 2.2'f32
          s_squared = s * s
          two_s_squared = 2.0'f32 * s_squared
          gaussian_coeff = 1.0'f32 / sqrt(2.0'f32 * PI * s_squared)
        
        # Transform sd values using SIMD: sd = sd / factor * s - spread / 8.8
        let
          s_vec = mm_set1_ps(s)
          spread_offset = mm_set1_ps(spread / 8.8'f32)
          # Use multiplication by reciprocal instead of division
          factor_reciprocal = mm_set1_ps(1.0'f32 / factor)
          transformed_sd = mm_sub_ps(
            mm_mul_ps(mm_mul_ps(sd_vec, factor_reciprocal), s_vec),
            spread_offset
          )
        
        # Calculate sd^2 using SIMD for all 4 pixels
        let sd_squared = mm_mul_ps(transformed_sd, transformed_sd)
        
        # Extract values for exponential calculation and conditional logic
        var 
          sd_squared_array: array[4, float32]
          transformed_sd_array: array[4, float32]
        mm_storeu_ps(sd_squared_array[0].addr, sd_squared)
        mm_storeu_ps(transformed_sd_array[0].addr, transformed_sd)
        
        var alpha_array: array[4, uint8]
        for i in 0 ..< 4:
          let
            transformed_sd_val = transformed_sd_array[i]
            exp_val = exp(-1.0'f32 * sd_squared_array[i] / two_s_squared)
            f = gaussian_coeff * exp_val
          
          if transformed_sd_val < 0.0'f32:
            let alpha_val = min(f * 255.0'f32 * 6.0'f32, 255.0'f32)
            alpha_array[i] = uint8(alpha_val)
          else:
            alpha_array[i] = 0'u8
        
        # Process only the actual pixels (not the padded ones)
        for i in 0 ..< remainingPixels:
          let
            sd = sd_array[i]
            base_color = if sd < 0.0: pos_rgbx else: neg_rgbx
            alpha = alpha_array[i]
            idx = row_start + x + i
          
          var final_color = base_color
          final_color.a = alpha
          image.data[idx] = final_color
      
      x += remainingPixels

when defined(release):
  {.pop.}
