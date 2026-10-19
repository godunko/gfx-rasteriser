--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Experimental area rasterizer for a line's closed rectangular contour.
--  Vertices use a 64x64 subpixel grid; DDA intersections retain 16-bit
--  fractional precision. No pixel buffer or dynamic allocation is needed,
--  only a buffer of cells of one row provided by the caller.
--
--  Each edge of the contour is walked from top to bottom through the cells
--  it crosses, so every intersection with a column boundary is computed
--  once. Intersections are exact: they are represented by values rounded
--  toward negative infinity to `Fixed_16` precision together with
--  remainders of this rounding, see `GFX.Implementation.DDA_Operations`,
--  so no error is accumulated while walking along the edge, and slopes are
--  not quantized. Only the rounded values are used by coverage
--  computations, so each intersection has an error less than 1 / 65_536
--  pixel. Coverage arithmetic truncates to 1/64 subpixel precision.
--
--  Contributions of edges to cells of the row are accumulated in the buffer
--  of cells, then cells are integrated from left to right. The result is
--  identical to `GFX.Rasteriser.DDA4`.

with GFX.Points;

private with GFX.Rasteriser.Coverage;

package GFX.Rasteriser.DDA5
  with Preelaborate
is

   type Cell is private;
   --  Accumulated contribution of edges to the cell of the row.

   type Cell_Buffer is
     array (GFX.Rasteriser.Device_Pixel_Index range <>) of Cell;
   --  Cells of one row, indexed by the column. It must cover all columns of
   --  the rendering area: for `Set_Rendering_Area (Top, Left, Right,
   --  Bottom)` use `Cell_Buffer (Left .. Right)`. Objects are empty after
   --  default initialization, and `Rasterize_Line` leaves them empty, so one
   --  object can be used for any number of lines.

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
      Cells     : in out Cell_Buffer;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale));
   --  Cells must cover columns of the rendering area, see `Cell_Buffer`.

private

   type Cell is new GFX.Rasteriser.Coverage.Cell;

end GFX.Rasteriser.DDA5;
