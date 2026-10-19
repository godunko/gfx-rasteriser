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
--  `GFX.Implementation.DDA_Operations`, so no error is accumulated
--  while stepping along the edge, and slopes are not quantized. Only the
--  rounded values are used by coverage computations, so each intersection
--  has an error less than 1 / 65_536 pixel. Coverage arithmetic truncates
--  to 1/64 subpixel precision.
--
--  Two variants of `Rasterize_Line` produce identical results. The first one
--  walks each row from left to right and computes contributions of all edges
--  to each cell. The second one walks each edge of the contour from top to
--  bottom through the cells it crosses, so every intersection with a column
--  boundary is computed once; contributions are accumulated in the buffer of
--  cells of one row provided by the caller, then cells are integrated from
--  left to right.

pragma Restrictions (No_Elaboration_Code);

with GFX.Points;

private with GFX.Rasteriser.Coverage;

package GFX.Rasteriser.Primitives
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
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale));
   --  Draw straight line between given two points of the given width. It
   --  supports anti-aliasing.

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
   --  The same as above, but contributions of edges are accumulated in
   --  Cells. Cells must cover columns of the rendering area, see
   --  `Cell_Buffer`.

private

   type Cell is new GFX.Rasteriser.Coverage.Cell;

end GFX.Rasteriser.Primitives;
