--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Naming conventions:
--    `_Lower`  is lower (inclusive) bound of a range on corresponding axis.
--    `_Upper`  is upper (exclusive) bound of a range on corresponding axis.
--    `Row_`    is a row (Y axis) in the rendering area.
--    `Column_` is a column (X axis) in the rendering area.

pragma Ada_2022;

with GFX.Implementation.Clipping;
with GFX.Implementation.DDA_Operations;
with GFX.Implementation.Fixed_Types;
with GFX.Implementation.Subpixel_Operations;
with GFX.Vectors;

package body GFX.Rasteriser.DDA5 is

   use GFX.Implementation.Fixed_Types;
   use GFX.Implementation.DDA_Operations;
   use GFX.Implementation.Subpixel_Operations;

   type Vertex is record
      X            : Fixed_16;
      Y            : Fixed_16;
      Row_Slope    : Slope;
      --  Inverse slope of the segment to the next vertex, ΔX / ΔY: change
      --  of X when Y changes, used to compute intersections with row
      --  boundaries.
      Column_Slope : Slope;
      --  Slope of the segment to the next vertex, ΔY / ΔX, along increasing
      --  X: change of Y when X increases, used to compute intersections with
      --  column boundaries.
   end record;

   type Vertex_Array is array (Positive range <>) of Vertex;

   type Row_Cursor is record
      Segment      : Natural := 0;
      --  Zero means that the chain is exhausted.
      X            : Coordinate;
      Y            : Fixed_16 := Zero;
      --  First unconsumed point, possibly inside the first row.
      Y_Bottom     : Fixed_16 := Zero;
      --  Y coordinate of the bottom end of the segment.
      At_Row_Upper : Coordinate;
      --  Valid only when this segment extends below the current row.
      Has_Column   : Boolean := False;
      Column_Edge  : Fixed_16 := Zero;
      At_Column    : Coordinate;
      --  Last computed intersection of this segment with the column
      --  boundary Column_Edge, if any. Boundaries are crossed in one
      --  direction along the segment, so it is moved by single DDA steps.
   end record;

   package Ghost is

      --  Provides functions to makes predicates more readable.

      function Has_Strictly_Increasing_Y
        (Chain : Vertex_Array) return Boolean is
        (for all Index in Chain'First .. Chain'Last - 1 =>
            Chain (Index).Y < Chain (Index + 1).Y);
      --  Horizontal and zero-length segments have already been removed.

      function Belongs_To_Pixel_Upper_Bound
        (Value : Fixed_16; Upper : Fixed_16) return Boolean is
         (Upper - One <= Value and Value < Upper);

   end Ghost;

   procedure Rasterize_Contour
     (Left      : Vertex_Array;
      Right     : Vertex_Array;
      Cells     : in out Cell_Buffer;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
     with Pre =>
       Left'Length in 2 .. 4 and Right'Length in 2 .. 4
       --  Both vertex lists contain at least one edge.
       and Ghost.Has_Strictly_Increasing_Y (Left)
       and Ghost.Has_Strictly_Increasing_Y (Right)
       --  Vertices in both chains are strictly ordered from top to bottom.
       and Left (Left'First).Y = Right (Right'First).Y
       and Left (Left'Last).Y = Right (Right'Last).Y
       --  Top and bottom segments between left and right edge lists are
       --  horizontal.
       and Left (Left'Last).Y > Left (Left'First).Y;
   --  Rasterize the contour between two chains row by row.

   procedure Initialize_Slopes (Item : in out Vertex; Next : Vertex)
     with Pre => Item.Y <= Next.Y;
   --  Compute slopes of the segment from Item to Next.

   procedure Initialize_Row_Cursor
     (Chain     : Vertex_Array;
      Row_Upper : Fixed_16;
      Cursor    : out Row_Cursor)
     with
       Pre =>
         Chain'Length >= 2
         --  At least one segment on the edge.
         and Ghost.Has_Strictly_Increasing_Y (Chain)
         --  Vertices in the chain are strictly ordered from top to bottom.
         and Is_Upper_Bound (Row_Upper)
         --  `Row_Upper` is a valid upper bound for the current row.
         and Ghost.Belongs_To_Pixel_Upper_Bound
               (Chain (Chain'First).Y, Row_Upper);

   procedure Set_Column_Range
     (X_Min        : Fixed_16;
      X_Max        : Fixed_16;
      Column_First : out GX_Integer;
      Column_Last  : out GX_Integer)
     with Inline, Pre => X_Min <= X_Max;
   --  Set `Column_First` and `Column_Last` to the inclusive range of pixel
   --  columns for a portion with the given range of X coordinates. If the
   --  range is not empty, exclude the column merely touched by the
   --  rightmost endpoint when it lies on a column boundary. Otherwise, keep
   --  the single column containing the vertical portion; on a boundary,
   --  this is the column to its right.

   procedure Walk_Chain
     (Chain     : Vertex_Array;
      Row_Upper : Fixed_16;
      Downward  : Boolean;
      Cells     : in out Cell_Buffer;
      Cursor    : in out Row_Cursor;
      First     : out GX_Integer;
      Last      : out GX_Integer)
     with Inline_Always, Pre => Cursor.Segment /= 0;
   --  Walk the chain through cells of the row ending at Row_Upper, add
   --  contributions of its parts to Cells, and advance Cursor to the next
   --  row (or mark it exhausted). Downward is True when the contour
   --  traverses the chain from top to bottom. First .. Last is the range of
   --  cells crossed by the chain.

   Rendering_Area  : GFX.Implementation.Clipping.GF_Clipping_Rectangle;
   Rendering_Y_Max : Fixed_16;
   --  Bottom boundary of the rendering area.
   Rendering_Left  : Device_Pixel_Index := 0;
   Rendering_Right : Device_Pixel_Index := 0;
   --  Range of columns of the rendering area.

   function Is_Upper_Bound (A : Fixed_16) return Boolean;
   --  Check whether A lies on an exclusive upper pixel boundary.
   --  Every integral coordinate is the upper boundary of the previous pixel.

   -----------------------
   -- Initialize_Slopes --
   -----------------------

   procedure Initialize_Slopes (Item : in out Vertex; Next : Vertex) is
      DX : constant Fixed_16 := Next.X - Item.X;
      DY : constant Fixed_16 := Next.Y - Item.Y;

   begin
      Initialize (Item.Row_Slope, DX, DY);

      if DX >= Zero then
         Initialize (Item.Column_Slope, DY, DX);
      else
         Initialize (Item.Column_Slope, -DY, -DX);
      end if;
   end Initialize_Slopes;

   ---------------------------
   -- Initialize_Row_Cursor --
   ---------------------------

   procedure Initialize_Row_Cursor
     (Chain     : Vertex_Array;
      Row_Upper : Fixed_16;
      Cursor    : out Row_Cursor)
   is
      Index   : constant Positive := Chain'First;
      Current : Vertex renames Chain (Index);
      Next    : Vertex renames Chain (Index + 1);

   begin
      Cursor.Segment    := Index;
      Cursor.X          := To_Coordinate (Current.X);
      Cursor.Y          := Current.Y;
      Cursor.Y_Bottom   := Next.Y;
      Cursor.Has_Column := False;

      if Row_Upper < Next.Y then
         At_Offset
           (Cursor.At_Row_Upper,
            Current.X,
            Current.Row_Slope,
            Row_Upper - Current.Y);
      end if;
   end Initialize_Row_Cursor;

   --------------------
   -- Is_Upper_Bound --
   --------------------

   function Is_Upper_Bound (A : Fixed_16) return Boolean is
   begin
      --  To_Pixel rounds toward negative infinity; converting its result
      --  back to Fixed_16 therefore gives the greatest integral coordinate
      --  not greater than A. Equality holds exactly at pixel boundaries.
      --  Each such coordinate is the exclusive upper bound of the previous
      --  pixel, including for negative values: -2.0 is the upper bound of
      --  pixel -3, whereas -2.25 rounds down to -3.0 and fails the test.

      return A = To_Fixed_16 (To_Pixel (A));
   end Is_Upper_Bound;

   -----------------------
   -- Rasterize_Contour --
   -----------------------

   procedure Rasterize_Contour
     (Left      : Vertex_Array;
      Right     : Vertex_Array;
      Cells     : in out Cell_Buffer;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
   is
      use type Grayscale;

      Left_Cursor  : Row_Cursor;
      Right_Cursor : Row_Cursor;
      Row          : GX_Integer := To_Pixel (Left (Left'First).Y);
      Row_Upper    : Fixed_16   := Pixel_Lower_Bound (Row) + One;
      Accumulator  : Fixed_6;
      Left_First   : GX_Integer;
      Left_Last    : GX_Integer;
      Right_First  : GX_Integer;
      Right_Last   : GX_Integer;

      procedure Sweep (First : GX_Integer; Last : GX_Integer)
        with Inline_Always;
      --  Integrate and fill cells First .. Last, and clear them.

      procedure Fill_Solid (First : GX_Integer; Last : GX_Integer)
        with Inline_Always;
      --  Fill cells First .. Last not crossed by any edge. Their occupied
      --  row height is represented by Accumulator.

      ----------------
      -- Fill_Solid --
      ----------------

      procedure Fill_Solid (First : GX_Integer; Last : GX_Integer) is
         Acc   : Fixed_6 := Accumulator;
         Value : Grayscale;

      begin
         Integrate (Acc, (Zero, Zero), Value);

         if Value /= 0 then
            Fill_Span
              (First, Row, Device_Pixel_Count (Last - First + 1), Value);
         end if;
      end Fill_Solid;

      -----------
      -- Sweep --
      -----------

      procedure Sweep (First : GX_Integer; Last : GX_Integer) is
         Value : Grayscale;

      begin
         for Column in First .. Last loop
            declare
               Item : Cell renames Cells (Column);

            begin
               Integrate (Accumulator, Item, Value);
               Item := (Zero, Zero);

               if Value /= 0 then
                  Fill_Span (Column, Row, 1, Value);
               end if;
            end;
         end loop;
      end Sweep;

   begin
      Initialize_Row_Cursor (Left, Row_Upper, Left_Cursor);
      Initialize_Row_Cursor (Right, Row_Upper, Right_Cursor);

      loop
         Accumulator := Zero;

         Walk_Chain
           (Left, Row_Upper, False, Cells, Left_Cursor, Left_First, Left_Last);
         Walk_Chain
           (Right,
            Row_Upper,
            True,
            Cells,
            Right_Cursor,
            Right_First,
            Right_Last);

         --  Cells crossed by each chain are contiguous. Cells between two
         --  ranges are not crossed by edges.

         if Right_First < Left_First then
            declare
               First : constant GX_Integer := Left_First;
               Last  : constant GX_Integer := Left_Last;

            begin
               Left_First  := Right_First;
               Left_Last   := Right_Last;
               Right_First := First;
               Right_Last  := Last;
            end;
         end if;

         if Right_First > Left_Last + 1 then
            Sweep (Left_First, Left_Last);
            Fill_Solid (Left_Last + 1, Right_First - 1);
            Sweep (Right_First, Right_Last);

         else
            Sweep (Left_First, GX_Integer'Max (Left_Last, Right_Last));
         end if;

         pragma Assert
           ((Left_Cursor.Segment = 0) = (Right_Cursor.Segment = 0));
         exit when Left_Cursor.Segment = 0;
         Row       := @ + 1;
         Row_Upper := @ + One;
      end loop;
   end Rasterize_Contour;

   --------------------
   -- Rasterize_Line --
   --------------------

   procedure Rasterize_Line
     (Point_A   : GFX.Points.GF_Point;
      Point_B   : GFX.Points.GF_Point;
      Width     : GFX.GX_Real;
      Cells     : in out Cell_Buffer;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale))
   is
      use GFX.Implementation.Clipping;
      use type GFX.Points.GF_Point;
      use type GFX.Vectors.GF_Vector;

      Point_Top      : GFX.Points.GF_Point;
      Point_Bottom   : GFX.Points.GF_Point;
      Vertex_Top     : GFX.Points.GF_Point;
      Vertex_Right   : GFX.Points.GF_Point;
      Vertex_Bottom  : GFX.Points.GF_Point;
      Vertex_Left    : GFX.Points.GF_Point;
      Outcode_Top    : GFX.Implementation.Clipping.Outcode;
      Outcode_Right  : GFX.Implementation.Clipping.Outcode;
      Outcode_Bottom : GFX.Implementation.Clipping.Outcode;
      Outcode_Left   : GFX.Implementation.Clipping.Outcode;

      Left_Vertex  : Vertex_Array (1 .. 4);
      Left_First   : Positive := 1;
      Left_Last    : Natural := 0;
      Right_Vertex : Vertex_Array (1 .. 4);
      Right_First  : Positive := 1;
      Right_Last   : Natural := 0;
      Done         : Boolean := False;

      procedure Append_Left_Vertex (Point : GFX.Points.GF_Point);

      procedure Append_Right_Vertex (Point : GFX.Points.GF_Point);

      procedure Adjust_Y
        (Chain    : in out Vertex_Array;
         First    : in out Positive;
         Last     : in out Natural;
         Y_Top    : Fixed_16;
         Y_Bottom : Fixed_16);

      procedure Rasterize_Parallelogram;
      --  Rasterize the rectangle inside of the rendering area. Its bottom
      --  vertex is computed from the other ones, so the contour is an exact
      --  parallelogram on the subpixel grid and its opposite sides share
      --  slopes. Set Done unless the bottom vertex is outside of the
      --  rendering area due to rounding.

      ------------------------
      -- Append_Left_Vertex --
      ------------------------

      procedure Append_Left_Vertex (Point : GFX.Points.GF_Point) is
         X : constant Fixed_16 := Snap_To_Subpixel (Point.X);
         Y : constant Fixed_16 := Snap_To_Subpixel (Point.Y);
         pragma Warnings (Off);
         Item : constant Vertex := (X => X, Y => Y, others => <>);
         pragma Warnings (On);

      begin
         if Left_Last = 0
           or else Left_Vertex (Left_Last).X /= Item.X
           or else Left_Vertex (Left_Last).Y /= Item.Y
         then
            Left_Last := @ + 1;
            Left_Vertex (Left_Last) := Item;
         end if;
      end Append_Left_Vertex;

      -------------------------
      -- Append_Right_Vertex --
      -------------------------

      procedure Append_Right_Vertex (Point : GFX.Points.GF_Point) is
         X : constant Fixed_16 := Snap_To_Subpixel (Point.X);
         Y : constant Fixed_16 := Snap_To_Subpixel (Point.Y);

         pragma Warnings (Off);
         Item : constant Vertex := (X => X, Y => Y, others => <>);
         pragma Warnings (On);

      begin
         if Right_Last = 0
           or else Right_Vertex (Right_Last).X /= Item.X
           or else Right_Vertex (Right_Last).Y /= Item.Y
         then
            Right_Last := @ + 1;
            Right_Vertex (Right_Last) := Item;
         end if;
      end Append_Right_Vertex;

      --------------
      -- Adjust_Y --
      --------------

      procedure Adjust_Y
        (Chain    : in out Vertex_Array;
         First    : in out Positive;
         Last     : in out Natural;
         Y_Top    : Fixed_16;
         Y_Bottom : Fixed_16)
      is
      begin
         --  Only provisional vertical window-side segments extend beyond
         --  the other chain's Y range. Their X coordinates remain valid.

         if Chain (First).Y < Y_Top then
            pragma Assert
              (First < Last
                 and then Chain (First).X = Chain (First + 1).X
                 and then Y_Top <= Chain (First + 1).Y);
            Chain (First).Y := Y_Top;
         end if;

         if Y_Bottom < Chain (Last).Y then
            pragma Assert
              (First < Last
                 and then Chain (Last).X = Chain (Last - 1).X
                 and then Chain (Last - 1).Y <= Y_Bottom);
            Chain (Last).Y := Y_Bottom;
         end if;

         --  Snapping can make either end of either chain horizontal,
         --  independently of the other chain. Keep the endpoint belonging
         --  to the adjacent nonhorizontal segment. This also removes
         --  endpoints that coincide with their neighbours after adjustment.

         while First < Last and then Chain (First).Y = Chain (First + 1).Y loop
            First := @ + 1;
         end loop;

         while First < Last and then Chain (Last).Y = Chain (Last - 1).Y loop
            Last := @ - 1;
         end loop;
      end Adjust_Y;

      -----------------------------
      -- Rasterize_Parallelogram --
      -----------------------------

      procedure Rasterize_Parallelogram is
         Top          : Vertex renames Left_Vertex (1);
         Left         : Vertex renames Left_Vertex (2);
         Left_Bottom  : Vertex renames Left_Vertex (3);
         Right_Top    : Vertex renames Right_Vertex (1);
         Right        : Vertex renames Right_Vertex (2);
         Right_Bottom : Vertex renames Right_Vertex (3);

         procedure Set (Item : out Vertex; X : Fixed_16; Y : Fixed_16)
           with Inline;

         ---------
         -- Set --
         ---------

         procedure Set (Item : out Vertex; X : Fixed_16; Y : Fixed_16) is
         begin
            Item.X := X;
            Item.Y := Y;
         end Set;

      begin
         Set
           (Top,
            Snap_To_Subpixel (Vertex_Top.X),
            Snap_To_Subpixel (Vertex_Top.Y));
         Set
           (Left,
            Snap_To_Subpixel (Vertex_Left.X),
            Snap_To_Subpixel (Vertex_Left.Y));
         Set
           (Right,
            Snap_To_Subpixel (Vertex_Right.X),
            Snap_To_Subpixel (Vertex_Right.Y));

         if Left.Y + Right.Y - Top.Y > Rendering_Y_Max then
            return;
         end if;

         Done := True;

         if Left.Y = Top.Y and Right.Y = Top.Y then
            --  The rectangle is degenerated after snapping.

            return;
         end if;

         Set (Left_Bottom, Left.X + Right.X - Top.X, Left.Y + Right.Y - Top.Y);

         --  Sides Top-Left and Right-Bottom, as well as Left-Bottom and
         --  Top-Right, are parallel and have equal slopes.

         Initialize_Slopes (Top, Left);
         Initialize_Slopes (Left, Left_Bottom);
         Right.Row_Slope        := Top.Row_Slope;
         Right.Column_Slope     := Top.Column_Slope;
         Right_Top.X            := Top.X;
         Right_Top.Y            := Top.Y;
         Right_Top.Row_Slope    := Left.Row_Slope;
         Right_Top.Column_Slope := Left.Column_Slope;
         Right_Bottom.X         := Left_Bottom.X;
         Right_Bottom.Y         := Left_Bottom.Y;

         --  Horizontal sides are excluded from chains. When side Top-Left
         --  is horizontal, Right-Bottom is horizontal too, and so on.

         Rasterize_Contour
           (Left_Vertex
              ((if Left.Y = Top.Y then 2 else 1)
               .. (if Right.Y = Top.Y then 2 else 3)),
            Right_Vertex
              ((if Right.Y = Top.Y then 2 else 1)
               .. (if Left.Y = Top.Y then 2 else 3)),
            Cells,
            Fill_Span);
      end Rasterize_Parallelogram;

   begin
      pragma Assert
        (Cells'First <= Rendering_Left and Rendering_Right <= Cells'Last);

      if Width <= 0.0 or else Point_A = Point_B
        or else X_Min (Rendering_Area) = X_Max (Rendering_Area)
        or else Y_Min (Rendering_Area) = Y_Max (Rendering_Area)
      then
         return;
      end if;

      --  1. Compute rectangle to be drawn.

      --  Find top and bottom points

      if Point_A.Y < Point_B.Y then
         Point_Top    := Point_A;
         Point_Bottom := Point_B;

      else
         Point_Top    := Point_B;
         Point_Bottom := Point_A;
      end if;

      declare
         V : constant GFX.Vectors.GF_Vector := Point_Bottom - Point_Top;
         N : constant GFX.Vectors.GF_Vector :=
           GFX.Vectors.Normalize ((V.Y, -V.X)) * (Width / 2.0);

      begin
         --  Endpoints are ordered by Y. The horizontal direction
         --  selects one of two arrangements of the rectangle vertices.

         if V.X > 0.0 then
            Vertex_Top    := Point_Top + N;
            Vertex_Right  := Point_Bottom + N;
            Vertex_Bottom := Point_Bottom - N;
            Vertex_Left   := Point_Top - N;

         else
            Vertex_Top    := Point_Top - N;
            Vertex_Right  := Point_Top + N;
            Vertex_Bottom := Point_Bottom + N;
            Vertex_Left   := Point_Bottom - N;
         end if;

         --  Fast path: the rectangle is inside of the rendering area, so
         --  clipping is not needed and coordinates are valid. Rounding is
         --  monotonic, so Vertex_Left and Vertex_Right are the leftmost and
         --  the rightmost vertices, and Vertex_Top and Vertex_Bottom are the
         --  topmost and the bottommost ones.

         if X_Min (Rendering_Area) <= Vertex_Left.X
           and Vertex_Right.X <= X_Max (Rendering_Area)
           and Y_Min (Rendering_Area) <= Vertex_Top.Y
           and Vertex_Bottom.Y <= Y_Max (Rendering_Area)
         then
            Rasterize_Parallelogram;

            if Done then
               return;
            end if;
         end if;

         if not Is_Valid_Device_Pixel (Vertex_Top.X)
           or not Is_Valid_Device_Pixel (Vertex_Top.Y)
           or not Is_Valid_Device_Pixel (Vertex_Right.X)
           or not Is_Valid_Device_Pixel (Vertex_Right.Y)
           or not Is_Valid_Device_Pixel (Vertex_Bottom.X)
           or not Is_Valid_Device_Pixel (Vertex_Bottom.Y)
           or not Is_Valid_Device_Pixel (Vertex_Left.X)
           or not Is_Valid_Device_Pixel (Vertex_Left.Y)
         then
            --  One of coordinate of rectangle is outside of the allowed
            --  drawing area.

            --  XXX It is stong check, however, is it necessary really?
            --  Clipping also do checks like this, may be they can be
            --  merged with some assumptions?

            return;
         end if;
      end;

      --  2. Clip rectangle by rendering area.

      Outcode_Top    := Compute_Outcode (Rendering_Area, Vertex_Top);
      Outcode_Right  := Compute_Outcode (Rendering_Area, Vertex_Right);
      Outcode_Bottom := Compute_Outcode (Rendering_Area, Vertex_Bottom);
      Outcode_Left   := Compute_Outcode (Rendering_Area, Vertex_Left);

      if Outcode_Top (Y_Increase)
        or Outcode_Bottom (Y_Decrease)
        or Outcode_Left (X_Increase)
        or Outcode_Right (X_Decrease)
      then
         --  Rectangle is completely outside of the rendering area.

         return;
      end if;

      if Outcode_Top /= GFX.Implementation.Clipping.Inside
        or Outcode_Right /= GFX.Implementation.Clipping.Inside
        or Outcode_Bottom /= GFX.Implementation.Clipping.Inside
        or Outcode_Left /= GFX.Implementation.Clipping.Inside
      then
         --  At least one vertex is outside of rendering area.

         declare
            Point_Top     : GFX.Points.GF_Point;
            Point_Bottom  : GFX.Points.GF_Point;
            Side_Top      : Clip_Side;
            Side_Bottom   : Clip_Side;
            Reject_Code   : Outcode;
            Left_Complete : Boolean := False;

         begin
            if Clip
              (Rendering_Area,
               Vertex_Top,
               Vertex_Left,
               Point_Top,
               Point_Bottom,
               Side_Top,
               Side_Bottom,
               Reject_Code)
            then
               Append_Left_Vertex (Point_Top);
               Append_Left_Vertex (Point_Bottom);

               --  Side_Bottom is None when the original endpoint is
               --  already on the lower boundary.

               Left_Complete :=
                 Side_Bottom = Y_Max
                   or else Point_Bottom.Y = Y_Max (Rendering_Area);

            else
               if Reject_Code (X_Increase)
                 or else Reject_Code (Y_Increase)
               then
                  --  Top -> Left was rejected to the right or below the
                  --  window. The rectangle interior extends in those
                  --  directions, so no part of the rectangle is visible.

                  return;
               end if;
            end if;

            if not Left_Complete then
               if Clip
                 (Rendering_Area,
                  Vertex_Left,
                  Vertex_Bottom,
                  Point_Top,
                  Point_Bottom,
                  Side_Top,
                  Side_Bottom,
                  Reject_Code)
               then
                  if Side_Top = X_Min and then Left_Last = 0 then
                     --  The upper edge is invisible. Start a provisional
                     --  window-side segment; its Y range will be refined
                     --  when the right chain is available.

                     Append_Left_Vertex
                       ((X_Min (Rendering_Area), Y_Min (Rendering_Area)));
                  end if;

                  --  If the upper edge ended on X_Min, its endpoint and
                  --  Point_Top delimit the intervening vertical segment.

                  Append_Left_Vertex (Point_Top);
                  Append_Left_Vertex (Point_Bottom);

               elsif Reject_Code (X_Increase)
                 or else Reject_Code (Y_Decrease)
               then
                  --  Left -> Bottom was rejected to the right or above
                  --  the window, excluding the entire rectangle.

                  return;

               else
                  if Left_Last = 0 then
                     Append_Left_Vertex
                       ((X_Min (Rendering_Area), Y_Min (Rendering_Area)));
                  end if;

                  Append_Left_Vertex
                    ((X_Min (Rendering_Area), Y_Max (Rendering_Area)));
               end if;
            end if;
         end;

         declare
            Point_Top      : GFX.Points.GF_Point;
            Point_Bottom   : GFX.Points.GF_Point;
            Side_Top       : Clip_Side;
            Side_Bottom    : Clip_Side;
            Reject_Code    : Outcode;
            Right_Complete : Boolean := False;

         begin
            if Clip
              (Rendering_Area,
               Vertex_Top,
               Vertex_Right,
               Point_Top,
               Point_Bottom,
               Side_Top,
               Side_Bottom,
               Reject_Code)
            then
               Append_Right_Vertex (Point_Top);
               Append_Right_Vertex (Point_Bottom);

               --  Side_Bottom is None when the original endpoint is
               --  already on the lower boundary.

               Right_Complete :=
                 Side_Bottom = Y_Max
                   or else Point_Bottom.Y = Y_Max (Rendering_Area);

            else
               if Reject_Code (X_Decrease)
                 or else Reject_Code (Y_Increase)
               then
                  --  Top -> Right was rejected to the left or below the
                  --  window. The rectangle interior extends in those
                  --  directions, so no part of the rectangle is visible.

                  return;
               end if;
            end if;

            if not Right_Complete then
               if Clip
                 (Rendering_Area,
                  Vertex_Right,
                  Vertex_Bottom,
                  Point_Top,
                  Point_Bottom,
                  Side_Top,
                  Side_Bottom,
                  Reject_Code)
               then
                  if Side_Top = X_Max and then Right_Last = 0 then
                     --  The upper edge is invisible. Start a provisional
                     --  window-side segment; its Y range will be refined
                     --  when both chain Y ranges are reconciled.

                     Append_Right_Vertex
                       ((X_Max (Rendering_Area), Y_Min (Rendering_Area)));
                  end if;

                  --  If the upper edge ended on X_Max, its endpoint and
                  --  Point_Top delimit the intervening vertical segment.

                  Append_Right_Vertex (Point_Top);
                  Append_Right_Vertex (Point_Bottom);

               elsif Reject_Code (X_Decrease)
                 or else Reject_Code (Y_Decrease)
               then
                  --  Right -> Bottom was rejected to the left or above
                  --  the window, excluding the entire rectangle.

                  return;

               else
                  if Right_Last = 0 then
                     Append_Right_Vertex
                       ((X_Max (Rendering_Area), Y_Min (Rendering_Area)));
                  end if;

                  Append_Right_Vertex
                    ((X_Max (Rendering_Area), Y_Max (Rendering_Area)));
               end if;
            end if;
         end;

      else
         Append_Left_Vertex (Vertex_Top);
         Append_Left_Vertex (Vertex_Left);
         Append_Left_Vertex (Vertex_Bottom);
         Append_Right_Vertex (Vertex_Top);
         Append_Right_Vertex (Vertex_Right);
         Append_Right_Vertex (Vertex_Bottom);
      end if;

      --  3. Restrict both chains to their common Y range.

      if Left_Last = 0 or else Right_Last = 0 then
         return;
      end if;

      declare
         Y_Top : constant Fixed_16 :=
           Max (Left_Vertex (Left_First).Y, Right_Vertex (Right_First).Y);
         Y_Bottom : constant Fixed_16 :=
           Min (Left_Vertex (Left_Last).Y, Right_Vertex (Right_Last).Y);

      begin
         if Y_Bottom <= Y_Top then
            return;
         end if;

         Adjust_Y (Left_Vertex, Left_First, Left_Last, Y_Top, Y_Bottom);
         Adjust_Y (Right_Vertex, Right_First, Right_Last, Y_Top, Y_Bottom);
      end;

      --  4. Update edges' information

      for J in Left_First .. Left_Last - 1 loop
         Initialize_Slopes (Left_Vertex (J), Left_Vertex (J + 1));
      end loop;

      for J in Right_First .. Right_Last - 1 loop
         Initialize_Slopes (Right_Vertex (J), Right_Vertex (J + 1));
      end loop;

      --  5. Rasterize the closed contour between the two clipped chains.

      Rasterize_Contour
        (Left_Vertex (Left_First .. Left_Last),
         Right_Vertex (Right_First .. Right_Last),
         Cells,
         Fill_Span);
   end Rasterize_Line;

   ----------------------
   -- Set_Column_Range --
   ----------------------

   procedure Set_Column_Range
     (X_Min        : Fixed_16;
      X_Max        : Fixed_16;
      Column_First : out GX_Integer;
      Column_Last  : out GX_Integer) is
   begin
      Column_First := To_Pixel (X_Min);
      Column_Last  := To_Pixel (X_Max);

      if X_Min /= X_Max and Is_Upper_Bound (X_Max) then
         --  The portion spans a nonzero X range and ends on a column
         --  boundary. Exclude the column to its right, which it only touches.

         Column_Last := @ - 1;
      end if;
   end Set_Column_Range;

   ------------------------
   -- Set_Rendering_Area --
   ------------------------

   procedure Set_Rendering_Area
     (Top    : GFX.Rasteriser.Device_Pixel_Index;
      Left   : GFX.Rasteriser.Device_Pixel_Index;
      Right  : GFX.Rasteriser.Device_Pixel_Index;
      Bottom : GFX.Rasteriser.Device_Pixel_Index) is
   begin
      GFX.Implementation.Clipping.Set_Bound
        (Self  => Rendering_Area,
         X_Min => Left,
         X_Max => Right,
         Y_Min => Top,
         Y_Max => Bottom);
      Rendering_Y_Max := Pixel_Lower_Bound (Bottom);
      Rendering_Left  := Left;
      Rendering_Right := Right;
   end Set_Rendering_Area;

   procedure Add_Part
     (Cells    : in out Cell_Buffer;
      Downward : Boolean;
      Column   : GX_Integer;
      X_A    : Fixed_16;
      Y_Top  : Fixed_16;
      X_B    : Fixed_16;
      Y_Down : Fixed_16)
     with Inline_Always;
   --  Add contribution of the part of the segment inside the cell Column
   --  between points with X coordinates X_A and X_B and Y coordinates
   --  Y_Top and Y_Down (Y_Top <= Y_Down).

   procedure Walk_Piece
     (Cells    : in out Cell_Buffer;
      Downward : Boolean;
      Cursor   : in out Row_Cursor;
      First    : in out GX_Integer;
      Last     : in out GX_Integer;
      Segment  : Vertex;
      X_Top   : Coordinate;
      Y_Top   : Fixed_16;
      X_Down  : Coordinate;
      Y_Down  : Fixed_16)
     with Inline_Always;
   --  Walk the piece of the segment inside the row from top to bottom
   --  through cells, and add contributions of its parts.

   --------------
   -- Add_Part --
   --------------

   procedure Add_Part
     (Cells    : in out Cell_Buffer;
      Downward : Boolean;
      Column   : GX_Integer;
      X_A    : Fixed_16;
      Y_Top  : Fixed_16;
      X_B    : Fixed_16;
      Y_Down : Fixed_16)
   is
      Upper  : constant Fixed_16 := Pixel_Lower_Bound (Column) + One;
      Width  : constant Fixed_6  := Floor ((Upper - X_A) + (Upper - X_B));
      Height : Fixed_6           := Floor (Y_Down) - Floor (Y_Top);
      Item   : Cell renames Cells (Column);

   begin
      if not Downward then
         Height := Zero - Height;
      end if;

      Add (Item, Height, Width);
   end Add_Part;

   ----------------
   -- Walk_Piece --
   ----------------

   procedure Walk_Piece
     (Cells    : in out Cell_Buffer;
      Downward : Boolean;
      Cursor   : in out Row_Cursor;
      First    : in out GX_Integer;
      Last     : in out GX_Integer;
      Segment  : Vertex;
      X_Top   : Coordinate;
      Y_Top   : Fixed_16;
      X_Down  : Coordinate;
      Y_Down  : Fixed_16)
   is
      X_Start : constant Fixed_16 := Value (X_Top);
      X_End   : constant Fixed_16 := Value (X_Down);
      Low     : GX_Integer;
      High    : GX_Integer;

   begin
      Set_Column_Range
        (Min (X_Start, X_End), Max (X_Start, X_End), Low, High);
      First := GX_Integer'Min (First, Low);
      Last  := GX_Integer'Max (Last, High);

      if Low = High then
         Add_Part (Cells, Downward, Low, X_Start, Y_Top, X_End, Y_Down);

         return;
      end if;

      declare
         Through : Slope renames Segment.Column_Slope;
         At_Edge : Coordinate;
         Edge    : Fixed_16;
         X       : Fixed_16 := X_Start;
         Y       : Fixed_16 := Y_Top;
         Y_Edge  : Fixed_16;

      begin
         if not Cursor.Has_Column then
            --  Compute the intersection with the right boundary of the
            --  first cell exactly, from the left endpoint.

            Edge := Pixel_Lower_Bound (Low) + One;

            if X_Start < X_End then
               Cross (At_Edge, Y_Top, X_Top, Through, Edge);
            else
               Cross (At_Edge, Y_Down, X_Down, Through, Edge);
            end if;

            Cursor.Has_Column := True;

         else
            At_Edge := Cursor.At_Column;
            Edge    := Cursor.Column_Edge;
         end if;

         if X_Start < X_End then
            --  The segment moves right: column boundaries are crossed
            --  from left to right.

            while Edge <= X_Start loop
               Step (At_Edge, Through);
               Edge := Edge + One;
            end loop;

            for Column in Low .. High - 1 loop
               Y_Edge := Value (At_Edge);
               Add_Part (Cells, Downward, Column, X, Y, Edge, Y_Edge);
               X := Edge;
               Y := Y_Edge;

               if Column < High - 1 then
                  Step (At_Edge, Through);
                  Edge := Edge + One;
               end if;
            end loop;

            Add_Part (Cells, Downward, High, X, Y, X_End, Y_Down);

         else
            --  The segment moves left: column boundaries are crossed
            --  from right to left.

            while Edge >= X_Start loop
               Step_Back (At_Edge, Through);
               Edge := Edge - One;
            end loop;

            while Edge < Pixel_Lower_Bound (High) loop
               Step (At_Edge, Through);
               Edge := Edge + One;
            end loop;

            for Column in reverse Low + 1 .. High loop
               Y_Edge := Value (At_Edge);
               Add_Part (Cells, Downward, Column, Edge, Y, X, Y_Edge);
               X := Edge;
               Y := Y_Edge;

               if Column > Low + 1 then
                  Step_Back (At_Edge, Through);
                  Edge := Edge - One;
               end if;
            end loop;

            Add_Part (Cells, Downward, Low, X_End, Y, X, Y_Down);
         end if;

         Cursor.At_Column   := At_Edge;
         Cursor.Column_Edge := Edge;
      end;
   end Walk_Piece;

   ----------------
   -- Walk_Chain --
   ----------------

   procedure Walk_Chain
     (Chain     : Vertex_Array;
      Row_Upper : Fixed_16;
      Downward  : Boolean;
      Cells     : in out Cell_Buffer;
      Cursor    : in out Row_Cursor;
      First     : out GX_Integer;
      Last      : out GX_Integer) is
   begin
      First := GX_Integer'Last;
      Last  := GX_Integer'First;

      loop
         declare
            Index    : constant Positive   := Cursor.Segment;
            Y_Bottom : constant Fixed_16   := Cursor.Y_Bottom;
            X_Top    : constant Coordinate := Cursor.X;
            Y_Top    : constant Fixed_16   := Cursor.Y;
            --  Local copies allow the compiler to keep values in registers.

         begin
            if Row_Upper < Y_Bottom then
               --  The segment continues in the next row.

               declare
                  X_Down : constant Coordinate := Cursor.At_Row_Upper;

               begin
                  Walk_Piece
                    (Cells,
                     Downward,
                     Cursor,
                     First,
                     Last,
                     Chain (Index),
                     X_Top,
                     Y_Top,
                     X_Down,
                     Row_Upper);
                  Cursor.X := X_Down;
                  Cursor.Y := Row_Upper;

                  if Row_Upper + One < Y_Bottom then
                     Step (Cursor.At_Row_Upper, Chain (Index).Row_Slope);
                  end if;
               end;

               exit;
            end if;

            --  The segment ends in this row.

            declare
               Next : Vertex renames Chain (Index + 1);

            begin
               Walk_Piece
                 (Cells,
                  Downward,
                  Cursor,
                  First,
                  Last,
                  Chain (Index),
                  X_Top,
                  Y_Top,
                  To_Coordinate (Next.X),
                  Next.Y);

               if Index + 1 = Chain'Last then
                  Cursor.Segment := 0;

                  exit;
               end if;

               Cursor.Segment    := Index + 1;
               Cursor.X          := To_Coordinate (Next.X);
               Cursor.Y          := Next.Y;
               Cursor.Y_Bottom   := Chain (Index + 2).Y;
               Cursor.Has_Column := False;

               --  Compute the intersection of the next segment with the
               --  lower boundary of the row where it continues, if it ends
               --  below.

               declare
                  Upper : constant Fixed_16 :=
                    (if Next.Y = Row_Upper
                     then Row_Upper + One
                     else Row_Upper);

               begin
                  if Upper < Chain (Index + 2).Y then
                     At_Offset
                       (Cursor.At_Row_Upper,
                        Next.X,
                        Next.Row_Slope,
                        Upper - Next.Y);
                  end if;
               end;

               exit when Next.Y = Row_Upper;
            end;
         end;
      end loop;
   end Walk_Chain;

end GFX.Rasteriser.DDA5;
