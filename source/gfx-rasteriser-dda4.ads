--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Experimental area rasterizer for a line's closed rectangular contour.
--  Vertices use a 64x64 subpixel grid; DDA intersections retain 16-bit
--  fractional precision. No pixel buffer or dynamic allocation is needed.
--
--  The rasterizer uses a DDA (Digital Differential Analyzer) approach to
--  compute intersections of the line with the pixel grid, maintaining
--  high precision for subpixel coverage calculations. Error correction is
--  *not* applied when stepping along the dominant axis.
--  When stepping by a truncated `Fixed_16` increment without remainder
--  correction, accumulated minor-axis error is less than N / 65_536 pixels
--  after N one-pixel steps along the dominant axis. Beyond 1_024 steps it
--  can exceed one 1/64 subpixel. Smaller errors can also change coverage
--  near subpixel grid boundaries. The step count refers to the processed
--  dominant-axis extent. Inverting a quantized slope can further amplify
--  intersection errors on nearly horizontal edges. Endpoint coordinates
--  are used without stepping beyond an edge. Coverage arithmetic also
--  truncates to Fixed_16 precision.

with GFX.Points;

package GFX.Rasteriser.DDA4
  with Preelaborate
is

   type Fill_Span_Subprogram is
     access procedure (X        : GFX.Rasteriser.Device_Pixel_Index;
                       Y        : GFX.Rasteriser.Device_Pixel_Index;
                       Width    : GFX.Rasteriser.Device_Pixel_Count;
                       Coverage : GFX.Rasteriser.Grayscale);

   procedure Set_Rendering_Area
     (Top    : GFX.Rasteriser.Device_Pixel_Index;
      Left   : GFX.Rasteriser.Device_Pixel_Index;
      Right  : GFX.Rasteriser.Device_Pixel_Index;
      Bottom : GFX.Rasteriser.Device_Pixel_Index)
     with Pre => Top >= 0 and Left >= 0 and Left <= Right and Top <= Bottom;

   procedure Rasterize_Line
     (Point_A   : GFX.Points.GF_Point;
      Point_B   : GFX.Points.GF_Point;
      Width     : GFX.GX_Real;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale));

end GFX.Rasteriser.DDA4;
