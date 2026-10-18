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
--  compute intersections of the line with the pixel grid. Intersections are
--  exact: they are represented by values rounded toward negative infinity
--  to `Fixed_16` precision together with remainders of this rounding, see
--  `GFX.Implementation.Fixed_Types.Exact_DDA`, so no error is accumulated
--  while stepping along the edge, and slopes are not quantized. Only the
--  rounded values are used by coverage computations, so each intersection
--  has an error less than 1 / 65_536 pixel. Coverage arithmetic truncates
--  to 1/64 subpixel precision.

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
