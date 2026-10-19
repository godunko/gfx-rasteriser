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
pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Clipping;
with GFX.Implementation.DDA_Operations;
with GFX.Implementation.Fixed_Types;
with GFX.Implementation.Subpixel_Operations;
with GFX.Rasteriser.Coverage;
with GFX.Vectors;

package body GFX.Rasteriser.Primitives is

   use GFX.Implementation.DDA_Operations;
   use GFX.Implementation.Fixed_Types;
   use GFX.Implementation.Subpixel_Operations;
   use GFX.Rasteriser.Coverage;

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

   subtype Chain_Storage is Vertex_Array (1 .. 4);
   --  Storage of the chain of vertices of the line's contour.

   type Row_Cursor is record
      Segment      : Natural := 0;
      --  Zero means that the chain is exhausted.
      X            : Coordinate;
      Y            : Fixed_16 := Zero;
      --  First unconsumed point, possibly inside the first row.
      At_Row_Upper : Coordinate;
      --  Valid only when this segment extends below the current row.
      Has_Column   : Boolean := False;
      Column_Upper : Fixed_16 := Zero;
      At_Column    : Coordinate;
      --  Last computed intersection of this segment with a column boundary,
      --  if any. It is kept between rows to compute intersections in the
      --  next row by DDA steps.
   end record;

   package Ghost is

      --  Provides functions to makes predicates more readable.

      function Has_Strictly_Increasing_Y
        (Chain : Vertex_Array) return Boolean is
        (for all Index in Chain'First .. Chain'Last - 1 =>
            Chain (Index).Y < Chain (Index + 1).Y);
      --  Horizontal and zero-length segments have already been removed.

      function Only_First_Segment_Can_Move_Left
        (Chain : Vertex_Array) return Boolean is
        (for all Index in Chain'First + 1 .. Chain'Last - 1 =>
            Chain (Index).X <= Chain (Index + 1).X);
      --  The first segment may move left; all later segments are vertical
      --  or move right.

      function Belongs_To_Pixel_Upper_Bound
        (Value : Fixed_16; Upper : Fixed_16) return Boolean is
         (Upper - One <= Value and Value < Upper);

   end Ghost;

   procedure Rasterize_Contour
     (Left      : Vertex_Array;
      Right     : Vertex_Array;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
     with
       No_Inline,
       --  Inlining into the only caller increases register pressure in the
       --  row loop; it was measured to be slower on MCUs.
       Pre =>
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

   procedure Initialize_Slopes (Item : in out Vertex; Next : Vertex)
     with Pre => Item.Y <= Next.Y;
   --  Compute slopes of the segment from Item to Next.

   function Build_Contour
     (Point_A      : GFX.Points.GF_Point;
      Point_B      : GFX.Points.GF_Point;
      Width        : GFX.GX_Real;
      Left_Vertex  : out Chain_Storage;
      Left_First   : out Positive;
      Left_Last    : out Natural;
      Right_Vertex : out Chain_Storage;
      Right_First  : out Positive;
      Right_Last   : out Natural) return Boolean
     with Inline;
   --  Build the closed contour of the line of the given width between two
   --  points, clipped by the rendering area: its left and right chains of
   --  vertices from top to bottom with initialized slopes. Return False when
   --  nothing is visible; otherwise, Left_Vertex (Left_First .. Left_Last)
   --  and Right_Vertex (Right_First .. Right_Last) satisfy the precondition
   --  of Rasterize_Contour.

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

   type Portion_State is record
      Column_First : GX_Integer;
      Column_Last  : GX_Integer;
      --  Inclusive range of cells crossed by the portion.
      X_In         : Fixed_16;
      Y_In         : Fixed_16;
      --  Point where the portion enters the current cell: its left endpoint
      --  in the first cell, then the intersection with the cell's left
      --  boundary.
      X_End        : Fixed_16;
      Y_End        : Fixed_16;
      --  Right endpoint of the portion.
      Y_Next       : Coordinate;
      --  Exact intersection with the right boundary of the current cell;
      --  valid only when the portion spans several cells.
      Slope        : GFX.Implementation.DDA_Operations.Slope;
      --  Slope of the segment along increasing X.
      Y_Lower      : Fixed_16;
      Y_Upper      : Fixed_16;
      --  Bounds of the portion along the Y axis.
      Positive     : Boolean;
      --  Whether the contour traverses the portion from left to right.
   end record;
   --  All state of the portion used while walking along the row. Objects of
   --  this type are local, so the compiler can allocate them to registers.
   --  Components have no default values to avoid initialization overhead;
   --  all of them are set by Initialize_Portion.

   type Portion_Array is array (Positive range <>) of Portion_State;

   procedure Initialize_Portion
     (Chain    : Vertex_Array;
      X_Top    : Coordinate;
      Y_Top    : Fixed_16;
      X_Bottom : Coordinate;
      Y_Bottom : Fixed_16;
      Reversed : Boolean;
      Cursor   : in out Row_Cursor;
      State    : out Portion_State)
     with Inline_Always,
          Pre => Cursor.Segment /= 0 and Y_Top < Y_Bottom;
   --  Initialize State to walk the portion of the current segment of Cursor
   --  between the given top and bottom endpoints, from its first cell. Each
   --  endpoint may be an original segment endpoint or an intersection with a
   --  row boundary; X coordinates are exact, computed by the segment's
   --  Row_Slope. Reversed traverses the segment against chain order. Cursor
   --  is the cursor of the chain before it is advanced to the next segment
   --  or row.

   procedure Contribute
     (State        : in out Portion_State;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Coverage     : in out Cell)
     with Inline_Always,
          Pre => Column in State.Column_First .. State.Column_Last;
   --  Add the contribution of the portion to the cell Column and advance
   --  State to the next cell. Cells must be visited in order.

   --  Initialize once before the first row.

   Rendering_Area : GFX.Implementation.Clipping.GF_Clipping_Rectangle;
   Rendering_Y_Max : Fixed_16;
   --  Bottom boundary of the rendering area.

   function Can_Prepare_Simple_Row
     (Chain       : Vertex_Array;
      Cursor      : Row_Cursor;
      Row_Lower   : Fixed_16;
      Row_Upper   : Fixed_16) return Boolean;
   --  Return True when the current segment crosses the complete row and no
   --  segment switch can occur in it.

   procedure Advance_Simple_Cursor
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Cursor    : in out Row_Cursor)
     with Pre => Row_Upper = Row_Lower + One
       and Can_Prepare_Simple_Row (Chain, Cursor, Row_Lower, Row_Upper);
   --  Advance Cursor to the next row, when the current segment crosses the
   --  complete row.

   procedure Collect_Portions
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Reversed  : Boolean;
      Cursor    : in out Row_Cursor;
      Portions  : in out Portion_Array;
      Count     : in out Natural)
     with Pre => Row_Upper = Row_Lower + One and Cursor.Segment /= 0;
   --  Append all portions of the chain belonging to this row to Portions,
   --  in chain order, and advance Cursor to the next row (or mark it
   --  exhausted).

   function Is_Upper_Bound (A : Fixed_16) return Boolean;
   --  Check whether A lies on an exclusive upper pixel boundary.
   --  Every integral coordinate is the upper boundary of the previous pixel.

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

   ------------------------
   -- Initialize_Portion --
   ------------------------

   procedure Initialize_Portion
     (Chain    : Vertex_Array;
      X_Top    : Coordinate;
      Y_Top    : Fixed_16;
      X_Bottom : Coordinate;
      Y_Bottom : Fixed_16;
      Reversed : Boolean;
      Cursor   : in out Row_Cursor;
      State    : out Portion_State)
   is
      Segment     : constant Positive := Cursor.Segment;
      Left_Is_Top : constant Boolean  := Value (X_Top) <= Value (X_Bottom);
      --  For a vertical portion the top endpoint is used as the left one,
      --  so the sign still follows the traversal direction.

   begin
      State.Y_Lower  := Y_Top;
      State.Y_Upper  := Y_Bottom;
      State.Positive := Left_Is_Top xor Reversed;

      if Left_Is_Top then
         State.X_In  := Value (X_Top);
         State.Y_In  := Y_Top;
         State.X_End := Value (X_Bottom);
         State.Y_End := Y_Bottom;

      else
         State.X_In  := Value (X_Bottom);
         State.Y_In  := Y_Bottom;
         State.X_End := Value (X_Top);
         State.Y_End := Y_Top;
      end if;

      Set_Column_Range
        (State.X_In, State.X_End, State.Column_First, State.Column_Last);

      if State.Column_First < State.Column_Last then
         declare
            Upper : constant Fixed_16 :=
              Pixel_Lower_Bound (State.Column_First) + One;

         begin
            if not Cursor.Has_Column then
               --  Compute the intersection with the right boundary of the
               --  first cell exactly, from the left endpoint.

               Cross
                 (Self       => Cursor.At_Column,
                  Out_Origin => State.Y_In,
                  In_Origin  => (if Left_Is_Top then X_Top else X_Bottom),
                  Through    => Chain (Segment).Column_Slope,
                  In_Value   => Upper);
               Cursor.Column_Upper := Upper;
               Cursor.Has_Column   := True;

            else
               --  Move the intersection computed in the previous row to the
               --  right boundary of the first cell. It is exact, so there is
               --  no difference with direct computation. Local copies allow
               --  the compiler to keep values in registers.

               declare
                  Through : constant Slope := Chain (Segment).Column_Slope;
                  At_Edge : Coordinate     := Cursor.At_Column;
                  Steps   : GX_Integer     :=
                    To_Pixel (Upper) - To_Pixel (Cursor.Column_Upper);

               begin
                  while Steps > 0 loop
                     Step (At_Edge, Through);
                     Steps := Steps - 1;
                  end loop;

                  while Steps < 0 loop
                     Step_Back (At_Edge, Through);
                     Steps := Steps + 1;
                  end loop;

                  Cursor.At_Column    := At_Edge;
                  Cursor.Column_Upper := Upper;
               end;
            end if;

            State.Y_Next := Cursor.At_Column;
            State.Slope  := Chain (Segment).Column_Slope;
         end;
      end if;
   end Initialize_Portion;

   ----------------
   -- Contribute --
   ----------------

   procedure Contribute
     (State        : in out Portion_State;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Coverage     : in out Cell)
   is
      Y_Out   : Fixed_16;
      Width   : Fixed_6;
      Delta_Y : Fixed_6;

   begin
      --  Accumulate the height and twice the signed area of the trapezoid
      --  matching the right edge for the part of the portion inside the
      --  cell, directed along the contour. Width is the sum of distances
      --  from both ends of the part to the right edge.

      if Column = State.Column_Last then
         --  The left end of the part is the left endpoint of the portion in
         --  its first cell, and it is on the left edge of other cells.

         Y_Out := State.Y_End;
         Width :=
           Floor
             ((if Column = State.Column_First
               then Column_Upper - State.X_In
               else One)
              + (Column_Upper - State.X_End));

      elsif Column = State.Column_First then
         --  Y_Next of the first cell is computed by Initialize_Portion.

         Y_Out := Value (State.Y_Next);
         Width := Floor (Column_Upper - State.X_In);

      else
         --  The part crosses the whole cell, the left end of the part is on
         --  the left edge of the cell, and the right end is on the right
         --  edge.

         Step (State.Y_Next, State.Slope);
         Y_Out := Value (State.Y_Next);
         Width := One;
      end if;

      --  Column boundaries crossed by the portion are strictly inside of the
      --  range of its X coordinates, so exact intersections with them are
      --  strictly inside of the range of its Y coordinates, no clamping is
      --  needed.

      pragma Assert (State.Y_Lower <= Y_Out and Y_Out <= State.Y_Upper);

      Delta_Y := Floor (Y_Out) - Floor (State.Y_In);

      if not State.Positive then
         Delta_Y := Zero - Delta_Y;
      end if;

      Add (Coverage, Delta_Y, Width);

      State.Y_In := Y_Out;
   end Contribute;

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
      Cursor.Has_Column := False;

      if Row_Upper < Next.Y then
         At_Offset
           (Cursor.At_Row_Upper,
            Current.X,
            Current.Row_Slope,
            Row_Upper - Current.Y);
      end if;
   end Initialize_Row_Cursor;

   ----------------------------
   -- Can_Prepare_Simple_Row --
   ----------------------------

   function Can_Prepare_Simple_Row
     (Chain       : Vertex_Array;
      Cursor      : Row_Cursor;
      Row_Lower   : Fixed_16;
      Row_Upper   : Fixed_16) return Boolean
   is
   begin
      return Cursor.Segment /= 0
        and then Cursor.Y = Row_Lower
        and then Row_Upper < Chain (Cursor.Segment + 1).Y;
   end Can_Prepare_Simple_Row;

   ---------------------------
   -- Advance_Simple_Cursor --
   ---------------------------

   procedure Advance_Simple_Cursor
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Cursor    : in out Row_Cursor)
   is
      pragma Unreferenced (Row_Lower);
      --  Used by precondition only.

      Index : constant Positive := Cursor.Segment;
      Last  : Vertex renames Chain (Index + 1);

   begin
      Cursor.X := Cursor.At_Row_Upper;
      Cursor.Y := Row_Upper;

      if Row_Upper + One < Last.Y then
         Step (Cursor.At_Row_Upper, Chain (Index).Row_Slope);
      end if;
   end Advance_Simple_Cursor;

   ----------------------
   -- Collect_Portions --
   ----------------------

   procedure Collect_Portions
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Reversed  : Boolean;
      Cursor    : in out Row_Cursor;
      Portions  : in out Portion_Array;
      Count     : in out Natural)
   is
      procedure Prepare_Intersection (Upper_Bound : Fixed_16)
        with Pre =>
          Cursor.Segment in Chain'First .. Chain'Last - 1
          --  Both endpoints of the current segment exist.
          and then Value (Cursor.X) = Chain (Cursor.Segment).X
          --  The X displacement is measured from the first vertex.
          and then Cursor.Y = Chain (Cursor.Segment).Y
          --  Row_Slope and the Y displacement use the same origin.
          and then Is_Upper_Bound (Upper_Bound)
          --  The intersection lies on an integral row boundary.
          and then
            Ghost.Belongs_To_Pixel_Upper_Bound (Cursor.Y, Upper_Bound)
          --  The positive Y displacement is at most one pixel, as required
          --  by At_Offset.
          and then Upper_Bound < Chain (Cursor.Segment + 1).Y;
          --  The boundary is inside the segment; its final vertex is
          --  handled separately by the caller.
      --  Set Cursor.At_Row_Upper to the X coordinate where the current
      --  segment intersects the row boundary Upper_Bound. The cursor must be
      --  at the segment's first vertex, strictly above Upper_Bound and at
      --  most one pixel away; the segment must end below Upper_Bound.
      --  Cursor.X, Cursor.Y and Cursor.Segment are left unchanged.

      --------------------------
      -- Prepare_Intersection --
      --------------------------

      procedure Prepare_Intersection (Upper_Bound : Fixed_16) is
      begin
         --  The cursor is at the segment's first vertex, and Upper_Bound is
         --  inside the segment, at most one pixel below that vertex.

         At_Offset
           (Cursor.At_Row_Upper,
            Chain (Cursor.Segment).X,
            Chain (Cursor.Segment).Row_Slope,
            Upper_Bound - Cursor.Y);
      end Prepare_Intersection;

   begin
      while Cursor.Segment /= 0 loop
         declare
            Index    : constant Positive   := Cursor.Segment;
            Last     : Vertex renames Chain (Index + 1);
            X_Top    : constant Coordinate := Cursor.X;
            Y_Top    : constant Fixed_16   := Cursor.Y;
            X_Bottom : Coordinate;
            Y_Bottom : Fixed_16;

         begin
            pragma Assert (Row_Lower <= Cursor.Y);
            pragma Assert (Cursor.Y < Last.Y);

            if Cursor.Y < Row_Upper then
               if Row_Upper < Last.Y then
                  X_Bottom := Cursor.At_Row_Upper;
                  Y_Bottom := Row_Upper;

               else
                  X_Bottom := To_Coordinate (Last.X);
                  Y_Bottom := Last.Y;
               end if;

               Count := Count + 1;
               Initialize_Portion
                 (Chain,
                  X_Top,
                  Y_Top,
                  X_Bottom,
                  Y_Bottom,
                  Reversed,
                  Cursor,
                  Portions (Count));

               if Row_Upper < Last.Y then
                  Cursor.X := X_Bottom;
                  Cursor.Y := Row_Upper;

                  if Row_Upper + One < Last.Y then
                     --  Continue this segment by one DDA step.

                     Step (Cursor.At_Row_Upper, Chain (Index).Row_Slope);
                  end if;

                  return;
               end if;
            end if;

            if Row_Upper <= Cursor.Y then
               --  A newly selected segment starts in the next row.
               if Row_Upper + One < Last.Y then
                  Prepare_Intersection (Row_Upper + One);
               end if;

               return;
            end if;

            --  Consume a finished segment exactly once.
            if Index + 1 = Chain'Last then
               Cursor.Segment := 0;
            else
               Cursor.Segment    := Index + 1;
               Cursor.Has_Column := False;
               Cursor.X := To_Coordinate (Last.X);
               Cursor.Y := Last.Y;

               if Cursor.Y < Row_Upper
                 and then Row_Upper < Chain (Index + 2).Y
               then
                  Prepare_Intersection (Row_Upper);
               end if;
            end if;
         end;
      end loop;
   end Collect_Portions;

   -----------------------
   -- Rasterize_Contour --
   -----------------------

   procedure Rasterize_Contour
     (Left      : Vertex_Array;
      Right     : Vertex_Array;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
   is
      use type Grayscale;

      Left_Cursor, Right_Cursor : Row_Cursor;
      Row          : GX_Integer := To_Pixel (Left (Left'First).Y);
      Row_Lower    : Fixed_16 := Pixel_Lower_Bound (Row);
      Row_Upper    : Fixed_16 := Row_Lower + One;
      Accumulator  : Fixed_6;

      procedure Fill_Solid
        (Column : GX_Integer;
         Width  : Device_Pixel_Count)
        with Inline_Always;
      --  Fill cells not crossed by any boundary. Their occupied row height
      --  is represented by Accumulator, including partial first and last
      --  rows.

      procedure Draw_Simple_Row;
      --  Draw a complete row crossed by one segment per side.

      procedure Draw_General_Row;
      --  Draw a row crossed by any number of portions on each side.

      ----------------
      -- Fill_Solid --
      ----------------

      procedure Fill_Solid
        (Column : GX_Integer;
         Width  : Device_Pixel_Count)
      is
         Value : constant Grayscale := Solid_Alpha (Accumulator);

      begin
         if Value /= 0 then
            Fill_Span (Column, Row, Width, Value);
         end if;
      end Fill_Solid;

      ---------------------
      -- Draw_Simple_Row --
      ---------------------

      procedure Draw_Simple_Row is
         Left_State  : Portion_State;
         Right_State : Portion_State;
         Left_Right  : Boolean;
         Right_Right : Boolean;
         --  Whether portions move right.
         X_Top       : Coordinate;
         X_Bottom    : Coordinate;
         --  Endpoints of the portion of the current side.
         Current     : GX_Integer;
         Upper       : Fixed_16;
         Cell        : Coverage.Cell;
         Alpha       : Grayscale;
         Y           : constant GX_Integer := Row;

      begin
         X_Top    := Left_Cursor.X;
         X_Bottom := Left_Cursor.At_Row_Upper;
         Initialize_Portion
           (Left,
            X_Top,
            Row_Lower,
            X_Bottom,
            Row_Upper,
            True,
            Left_Cursor,
            Left_State);
         Left_Right := Value (X_Top) < Value (X_Bottom);
         Advance_Simple_Cursor (Left, Row_Lower, Row_Upper, Left_Cursor);

         X_Top    := Right_Cursor.X;
         X_Bottom := Right_Cursor.At_Row_Upper;
         Initialize_Portion
           (Right,
            X_Top,
            Row_Lower,
            X_Bottom,
            Row_Upper,
            False,
            Right_Cursor,
            Right_State);
         Right_Right := Value (X_Top) < Value (X_Bottom);
         Advance_Simple_Cursor (Right, Row_Lower, Row_Upper, Right_Cursor);
         pragma Assert
           (Left_State.Column_First <= Right_State.Column_First);

         Current := Left_State.Column_First;
         Upper   := Pixel_Lower_Bound (Current) + One;

         while Current <= Right_State.Column_Last loop
            if Current > Left_State.Column_Last
              and Current < Right_State.Column_First
            then
               Fill_Solid
                 (Current,
                  Device_Pixel_Count (Right_State.Column_First - Current));
               Current := Right_State.Column_First;
               Upper   := Pixel_Lower_Bound (Current) + One;

            else
               Cell := (Zero, Zero);

               if Current <= Left_State.Column_Last then
                  Contribute (Left_State, Current, Upper, Cell);
               end if;

               if Current >= Right_State.Column_First then
                  Contribute (Right_State, Current, Upper, Cell);
               end if;

               Integrate (Accumulator, Cell, Alpha);

               if Alpha /= 0 then
                  Fill_Span (Current, Y, 1, Alpha);
               end if;

               Current := @ + 1;
               Upper   := @ + One;
            end if;
         end loop;

         --  Intersections of a portion moving right continue in the next
         --  row from the last one of this row, which is at the left boundary
         --  of its last cell. Save it to avoid repeated DDA steps.

         if Left_Right
           and Left_State.Column_First < Left_State.Column_Last
           and Left_State.Column_Last <= Right_State.Column_Last
         then
            Left_Cursor.At_Column    := Left_State.Y_Next;
            Left_Cursor.Column_Upper :=
              Pixel_Lower_Bound (Left_State.Column_Last);
         end if;

         if Right_Right
           and Right_State.Column_First < Right_State.Column_Last
         then
            Right_Cursor.At_Column    := Right_State.Y_Next;
            Right_Cursor.Column_Upper :=
              Pixel_Lower_Bound (Right_State.Column_Last);
         end if;
      end Draw_Simple_Row;

      ----------------------
      -- Draw_General_Row --
      ----------------------

      procedure Draw_General_Row is
         Portions : Portion_Array (1 .. Left'Length + Right'Length - 2);
         Count    : Natural := 0;
         Current  : GX_Integer;
         Last     : GX_Integer := GX_Integer'First;
         Upper    : Fixed_16;
         Cell     : Coverage.Cell;
         Value    : Grayscale;
         Active   : Boolean;
         Next     : GX_Integer;

      begin
         pragma Assert (Ghost.Only_First_Segment_Can_Move_Left (Left));

         Collect_Portions
           (Left, Row_Lower, Row_Upper, True, Left_Cursor, Portions, Count);
         pragma Assert (Count /= 0);

         --  Only the first left portion can move left. Later portions are
         --  vertical or move right, so its range determines the first cell
         --  of this row.

         Current := Portions (1).Column_First;

         declare
            Right_First : constant Positive := Count + 1;

         begin
            Collect_Portions
              (Right, Row_Lower, Row_Upper, False, Right_Cursor,
               Portions, Count);
            pragma Assert (Count >= Right_First);

            for Index in Right_First .. Count loop
               Last := GX_Integer'Max (@, Portions (Index).Column_Last);
            end loop;
         end;

         Upper := Pixel_Lower_Bound (Current) + One;

         while Current <= Last loop
            Cell   := (Zero, Zero);
            Active := False;
            Next   := Last + 1;

            for Portion of Portions (1 .. Count) loop
               if Current in Portion.Column_First .. Portion.Column_Last then
                  Contribute (Portion, Current, Upper, Cell);
                  Active := True;

               elsif Current < Portion.Column_First then
                  Next := GX_Integer'Min (@, Portion.Column_First);
               end if;
            end loop;

            if Active then
               Integrate (Accumulator, Cell, Value);

               if Value /= 0 then
                  Fill_Span (Current, Row, 1, Value);
               end if;

               Current := @ + 1;
               Upper   := @ + One;

            else
               Fill_Solid (Current, Device_Pixel_Count (Next - Current));
               Current := Next;
               Upper   := Pixel_Lower_Bound (Current) + One;
            end if;
         end loop;
      end Draw_General_Row;

   begin
      Initialize_Row_Cursor (Left, Row_Upper, Left_Cursor);
      Initialize_Row_Cursor (Right, Row_Upper, Right_Cursor);

      while Left_Cursor.Segment /= 0 and Right_Cursor.Segment /= 0 loop
         Accumulator := Zero;

         if Can_Prepare_Simple_Row
              (Left, Left_Cursor, Row_Lower, Row_Upper)
           and Can_Prepare_Simple_Row
             (Right, Right_Cursor, Row_Lower, Row_Upper)
         then
            Draw_Simple_Row;
         else
            Draw_General_Row;
         end if;

         pragma Assert
           ((Left_Cursor.Segment = 0) = (Right_Cursor.Segment = 0));
         exit when Left_Cursor.Segment = 0;
         Row := @ + 1;
         Row_Lower := Row_Upper;
         Row_Upper := @ + One;
      end loop;
   end Rasterize_Contour;

   -------------------
   -- Build_Contour --
   -------------------

   function Build_Contour
     (Point_A      : GFX.Points.GF_Point;
      Point_B      : GFX.Points.GF_Point;
      Width        : GFX.GX_Real;
      Left_Vertex  : out Chain_Storage;
      Left_First   : out Positive;
      Left_Last    : out Natural;
      Right_Vertex : out Chain_Storage;
      Right_First  : out Positive;
      Right_Last   : out Natural) return Boolean
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

      Done : Boolean := False;

      procedure Append_Left_Vertex (Point : GFX.Points.GF_Point);

      procedure Append_Right_Vertex (Point : GFX.Points.GF_Point);

      procedure Adjust_Y
        (Chain    : in out Vertex_Array;
         First    : in out Positive;
         Last     : in out Natural;
         Y_Top    : Fixed_16;
         Y_Bottom : Fixed_16);

      procedure Build_Parallelogram;
      --  Build chains of the rectangle inside of the rendering area. Its
      --  bottom vertex is computed from the other ones, so the contour is an
      --  exact parallelogram on the subpixel grid and its opposite sides
      --  share slopes. Set Done unless the bottom vertex is outside of the
      --  rendering area due to rounding. Chains are left empty when the
      --  rectangle is degenerated after snapping.

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

      -------------------------
      -- Build_Parallelogram --
      -------------------------

      procedure Build_Parallelogram is
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

         Left_First  := (if Left.Y = Top.Y then 2 else 1);
         Left_Last   := (if Right.Y = Top.Y then 2 else 3);
         Right_First := (if Right.Y = Top.Y then 2 else 1);
         Right_Last  := (if Left.Y = Top.Y then 2 else 3);
      end Build_Parallelogram;

   begin
      Left_First  := 1;
      Left_Last   := 0;
      Right_First := 1;
      Right_Last  := 0;

      if Width <= 0.0 or else Point_A = Point_B
        or else X_Min (Rendering_Area) = X_Max (Rendering_Area)
        or else Y_Min (Rendering_Area) = Y_Max (Rendering_Area)
      then
         return False;
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
            Build_Parallelogram;

            if Done then
               return Left_Last /= 0;
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

            return False;
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

         return False;
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

                  return False;
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

                  return False;

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

                  return False;
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

                  return False;

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
         return False;
      end if;

      declare
         Y_Top : constant Fixed_16 :=
           Max (Left_Vertex (Left_First).Y, Right_Vertex (Right_First).Y);
         Y_Bottom : constant Fixed_16 :=
           Min (Left_Vertex (Left_Last).Y, Right_Vertex (Right_Last).Y);

      begin
         if Y_Bottom <= Y_Top then
            return False;
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

      return True;
   end Build_Contour;

   --------------------
   -- Rasterize_Line --
   --------------------

   procedure Rasterize_Line
     (Point_A   : GFX.Points.GF_Point;
      Point_B   : GFX.Points.GF_Point;
      Width     : GFX.GX_Real;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale))
   is
      Left_Vertex  : Chain_Storage;
      Left_First   : Positive;
      Left_Last    : Natural;
      Right_Vertex : Chain_Storage;
      Right_First  : Positive;
      Right_Last   : Natural;

   begin
      if Build_Contour
           (Point_A,
            Point_B,
            Width,
            Left_Vertex,
            Left_First,
            Left_Last,
            Right_Vertex,
            Right_First,
            Right_Last)
      then
         Rasterize_Contour
           (Left_Vertex (Left_First .. Left_Last),
            Right_Vertex (Right_First .. Right_Last),
            Fill_Span);
      end if;
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
   end Set_Rendering_Area;

end GFX.Rasteriser.Primitives;
