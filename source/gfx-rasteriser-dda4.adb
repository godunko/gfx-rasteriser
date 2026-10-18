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
with GFX.Implementation.Fixed_Types;
with GFX.Implementation.Subpixel_Operations;
with GFX.Vectors;

package body GFX.Rasteriser.DDA4 is

   use GFX.Implementation.Fixed_Types;
   use GFX.Implementation.Subpixel_Operations;

   type Cell_Coverage is record
      Coverage   : Fixed_6;
      --  Vertical height of the line inside the cell.
      Twice_Area : Fixed_12;
      --  Twice the signed area of the trapezoid matching the right edge.
   end record;

   type Vertex is record
      X      : Fixed_16;
      Y      : Fixed_16;
      Column : GX_Integer;
      Row    : GX_Integer;
      Step_X : Fixed_16 := Zero;
      --  Inverse slope, ΔX / ΔY, step on X while Y changes by one pixel.
      Step_Y : Fixed_16 := Zero;
      --  Slope, ΔY / ΔX, step on Y while X changes by one pixel.
   end record;

   type Vertex_Array is array (Positive range <>) of Vertex;

   type Row_Cursor is record
      Segment      : Natural := 0;
      --  Zero means that the chain is exhausted.
      X            : Fixed_16 := Zero;
      Y            : Fixed_16 := Zero;
      --  First unconsumed point, possibly inside the first row.
      At_Row_Upper : Fixed_16 := Zero;
      --  Valid only when this segment extends below the current row.
   end record;

   type Segment_DDA is record
      Segment         : Natural := 0;

      X_Top           : Fixed_16 := Zero;
      Y_Top           : Fixed_16 := Zero;
      X_Bottom        : Fixed_16 := Zero;
      Y_Bottom        : Fixed_16 := Zero;
      --  Top and bottom endpoints of the segment portion within the current
      --  row. Each may be an original segment endpoint or an intersection
      --  with a row boundary.

      Column_First    : GX_Integer := 0;
      Column_Last     : GX_Integer := -1;
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

   procedure Set_Column_Range (Part : in out Segment_DDA);
   --  Set `Column_First` and `Column_Last` to the inclusive range of pixel
   --  columns for this portion, regardless of its direction along X. If the
   --  endpoint X coordinates differ, exclude the column merely touched by
   --  the rightmost endpoint when it lies on a column boundary. If they are
   --  equal, keep the single column containing the vertical portion; on a
   --  boundary, this is the column to its right.

   type Portion_State is record
      Column_First : GX_Integer := 0;
      Column_Last  : GX_Integer := -1;
      --  Inclusive range of cells crossed by the portion.
      X_In         : Fixed_16 := Zero;
      Y_In         : Fixed_16 := Zero;
      --  Point where the portion enters the current cell: its left endpoint
      --  in the first cell, then the intersection with the cell's left
      --  boundary.
      X_End        : Fixed_16 := Zero;
      Y_End        : Fixed_16 := Zero;
      --  Right endpoint of the portion.
      Y_First_Out  : Fixed_16 := Zero;
      --  Intersection with the right boundary of the first cell; valid only
      --  when the portion spans several cells.
      Step         : Fixed_16 := Zero;
      Y_Lower      : Fixed_16 := Zero;
      Y_Upper      : Fixed_16 := Zero;
      --  DDA step and bounds of the portion along the Y axis.
      Positive     : Boolean  := False;
      --  Whether the contour traverses the portion from left to right.
   end record;
   --  All state of the portion used while walking along the row. Objects of
   --  this type are local, so the compiler can allocate them to registers.

   type Portion_Array is array (Positive range <>) of Portion_State;

   procedure Initialize_Portion
     (Chain    : Vertex_Array;
      Part     : Segment_DDA;
      Reversed : Boolean;
      State    : out Portion_State)
     with Inline_Always;
   --  Initialize State to walk Part from its first cell. Reversed traverses
   --  the segment against chain order.

   procedure Contribute
     (State        : in out Portion_State;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Coverage     : in out Cell_Coverage)
     with Inline_Always,
          Pre => Column in State.Column_First .. State.Column_Last;
   --  Add the contribution of the portion to the cell Column and advance
   --  State to the next cell. Cells must be visited in order.

   --  Initialize once before the first row.

   Rendering_Area : GFX.Implementation.Clipping.GF_Clipping_Rectangle;

   function Can_Prepare_Simple_Row
     (Chain       : Vertex_Array;
      Cursor      : Row_Cursor;
      Row_Lower   : Fixed_16;
      Row_Upper   : Fixed_16) return Boolean;
   --  Return True when the current segment crosses the complete row and no
   --  segment switch can occur in it.

   procedure Prepare_Simple_Portion
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Cursor    : in out Row_Cursor;
      Part      : out Segment_DDA)
     with Pre => Row_Upper = Row_Lower + One
       and Can_Prepare_Simple_Row (Chain, Cursor, Row_Lower, Row_Upper);
   --  Prepare the sole portion of this side crossing a complete row and
   --  advance Cursor to the next row.

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

   procedure Integrate
     (Accumulator : in out Fixed_6;
      Coverage    : Cell_Coverage;
      Alpha       : out Grayscale) with Export;

   ---------------
   -- Integrate --
   ---------------

   procedure Integrate
     (Accumulator : in out Fixed_6;
      Coverage    : Cell_Coverage;
      Alpha       : out Grayscale)
   is
      Area : constant Fixed_12 :=
        (2 * Accumulator * One + Coverage.Twice_Area) / 2;

   begin
      Alpha       := Grayscale (Integral (255 * abs Area));
      Accumulator := @ + Coverage.Coverage;
   end Integrate;

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

   --------------------
   -- Intersection_Y --
   --------------------

   function Intersection_Y
     (Chain : Vertex_Array;
      Part  : Segment_DDA;
      X     : Fixed_16) return Fixed_16
   is
      Origin : constant Fixed_16 := Chain (Part.Segment).Y;
      Offset : Fixed_16;
   begin
      if X = Part.X_Top then
         return Part.Y_Top;
      elsif X = Part.X_Bottom then
         return Part.Y_Bottom;
      end if;

      Offset := Multiply_Saturated
        (Chain (Part.Segment).Step_Y, X - Chain (Part.Segment).X);
      return Origin + Max (Part.Y_Top - Origin,
        Min (Part.Y_Bottom - Origin, Offset));
   end Intersection_Y;

   pragma Inline (Intersection_Y);

   ------------------------
   -- Initialize_Portion --
   ------------------------

   procedure Initialize_Portion
     (Chain    : Vertex_Array;
      Part     : Segment_DDA;
      Reversed : Boolean;
      State    : out Portion_State)
   is
      Left_Is_Top : constant Boolean := Part.X_Top <= Part.X_Bottom;
      --  For a vertical portion the top endpoint is used as the left one,
      --  so the sign still follows the traversal direction.

   begin
      State.Column_First := Part.Column_First;
      State.Column_Last  := Part.Column_Last;
      State.Step         := Chain (Part.Segment).Step_Y;
      State.Y_Lower      := Part.Y_Top;
      State.Y_Upper      := Part.Y_Bottom;
      State.Positive     := Left_Is_Top xor Reversed;
      State.Y_First_Out  := Zero;

      if Left_Is_Top then
         State.X_In  := Part.X_Top;
         State.Y_In  := Part.Y_Top;
         State.X_End := Part.X_Bottom;
         State.Y_End := Part.Y_Bottom;

      else
         State.X_In  := Part.X_Bottom;
         State.Y_In  := Part.Y_Bottom;
         State.X_End := Part.X_Top;
         State.Y_End := Part.Y_Top;
      end if;

      if Part.Column_First < Part.Column_Last then
         --  Initialize the intersection from the original vertex. Later
         --  cells use DDA steps.

         State.Y_First_Out :=
           Intersection_Y
             (Chain, Part, Pixel_Lower_Bound (Part.Column_First) + One);
      end if;
   end Initialize_Portion;

   ----------------
   -- Contribute --
   ----------------

   procedure Contribute
     (State        : in out Portion_State;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Coverage     : in out Cell_Coverage)
   is
      X_Out   : Fixed_16;
      Y_Out   : Fixed_16;
      Delta_Y : Fixed_6;

   begin
      if Column = State.Column_Last then
         X_Out := State.X_End;
         Y_Out := State.Y_End;

      elsif Column = State.Column_First then
         X_Out := Column_Upper;
         Y_Out := State.Y_First_Out;

      else
         X_Out := Column_Upper;
         Y_Out := State.Y_In
           + Max (State.Y_Lower - State.Y_In,
                  Min (State.Y_Upper - State.Y_In, State.Step));
      end if;

      --  Accumulate the height and twice the signed area of the trapezoid
      --  matching the right edge for the part of the portion inside the
      --  cell, directed along the contour.

      Delta_Y := Floor (Y_Out) - Floor (State.Y_In);

      if not State.Positive then
         Delta_Y := Zero - Delta_Y;
      end if;

      Coverage.Coverage   := @ + Delta_Y;
      Coverage.Twice_Area :=
        @ + Delta_Y
              * Floor ((Column_Upper - State.X_In) + (Column_Upper - X_Out));

      State.X_In := X_Out;
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
      Current : constant Vertex   := Chain (Index);
      Next    : constant Vertex   := Chain (Index + 1);

   begin
      Cursor :=
        (Segment      => Index,
         X            => Current.X,
         Y            => Current.Y,
         At_Row_Upper =>
           (if Row_Upper < Next.Y
            then
              Current.X
                + Multiply_Scaled (Current.Step_X, Row_Upper - Current.Y)
            else Zero));
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

   ----------------------------
   -- Prepare_Simple_Portion --
   ----------------------------

   procedure Prepare_Simple_Portion
     (Chain     : Vertex_Array;
      Row_Lower : Fixed_16;
      Row_Upper : Fixed_16;
      Cursor    : in out Row_Cursor;
      Part      : out Segment_DDA)
   is
      Index : constant Positive := Cursor.Segment;
      Last  : Vertex renames Chain (Index + 1);

   begin
      Part :=
        (Segment         => Index,
         X_Top           => Cursor.X,
         Y_Top           => Row_Lower,
         X_Bottom        => Cursor.At_Row_Upper,
         Y_Bottom        => Row_Upper,
         Column_First    => 0,
         Column_Last     => -1);
      Set_Column_Range (Part);

      Cursor.X := Part.X_Bottom;
      Cursor.Y := Row_Upper;
      Cursor.At_Row_Upper :=
        (if Row_Upper + One < Last.Y
         then Cursor.X + Chain (Index).Step_X
         else Zero);
   end Prepare_Simple_Portion;

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
          and then Cursor.X = Chain (Cursor.Segment).X
          --  The X displacement is measured from the first vertex.
          and then Cursor.Y = Chain (Cursor.Segment).Y
          --  Step_X and the Y displacement use the same origin.
          and then Is_Upper_Bound (Upper_Bound)
          --  The intersection lies on an integral row boundary.
          and then
            Ghost.Belongs_To_Pixel_Upper_Bound (Cursor.Y, Upper_Bound)
          --  The positive Y displacement is at most one pixel, as required
          --  by Multiply_Scaled.
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
         --  Truncation toward zero keeps the displacement within the segment
         --  without clamping.

         Cursor.At_Row_Upper := Cursor.X
           + Multiply_Scaled
               (Chain (Cursor.Segment).Step_X, Upper_Bound - Cursor.Y);
      end Prepare_Intersection;

   begin
      while Cursor.Segment /= 0 loop
         declare
            Index : constant Positive := Cursor.Segment;
            Last  : Vertex renames Chain (Index + 1);
            Part  : Segment_DDA;

         begin
            pragma Assert (Row_Lower <= Cursor.Y);
            pragma Assert (Cursor.Y < Last.Y);

            if Cursor.Y < Row_Upper then
               Part.Segment := Index;
               Part.X_Top   := Cursor.X;
               Part.Y_Top   := Cursor.Y;

               if Row_Upper < Last.Y then
                  Part.X_Bottom := Cursor.At_Row_Upper;
                  Part.Y_Bottom := Row_Upper;

               else
                  Part.X_Bottom := Last.X;
                  Part.Y_Bottom := Last.Y;
               end if;

               Set_Column_Range (Part);

               Count := Count + 1;
               Initialize_Portion (Chain, Part, Reversed, Portions (Count));

               if Row_Upper < Last.Y then
                  Cursor.X := Part.X_Bottom;
                  Cursor.Y := Row_Upper;

                  if Row_Upper + One < Last.Y then
                     --  Continue this segment by one DDA step. Truncation
                     --  toward zero keeps X within the segment.
                     Cursor.At_Row_Upper := Cursor.X + Chain (Index).Step_X;
                  else
                     Cursor.At_Row_Upper := Zero;
                  end if;

                  return;
               end if;
            end if;

            if Row_Upper <= Cursor.Y then
               --  A newly selected segment starts in the next row.
               if Row_Upper + One < Last.Y then
                  Prepare_Intersection (Row_Upper + One);
               else
                  Cursor.At_Row_Upper := Zero;
               end if;

               return;
            end if;

            --  Consume a finished segment exactly once.
            if Index + 1 = Chain'Last then
               Cursor := (others => <>);
            else
               Cursor.Segment := Index + 1;
               Cursor.X := Last.X;
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
         Acc   : Fixed_6 := Accumulator;
         Value : Grayscale;

      begin
         Integrate (Acc, (Zero, Zero), Value);

         if Value /= 0 then
            Fill_Span (Column, Row, Width, Value);
         end if;
      end Fill_Solid;

      ---------------------
      -- Draw_Simple_Row --
      ---------------------

      procedure Draw_Simple_Row is
         Part        : Segment_DDA;
         Left_State  : Portion_State;
         Right_State : Portion_State;
         Current     : GX_Integer;
         Upper       : Fixed_16;
         Cell        : Cell_Coverage;
         Value       : Grayscale;
         Y           : constant GX_Integer := Row;

      begin
         Prepare_Simple_Portion
           (Left, Row_Lower, Row_Upper, Left_Cursor, Part);
         Initialize_Portion (Left, Part, True, Left_State);
         Prepare_Simple_Portion
           (Right, Row_Lower, Row_Upper, Right_Cursor, Part);
         Initialize_Portion (Right, Part, False, Right_State);
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

               Integrate (Accumulator, Cell, Value);

               if Value /= 0 then
                  Fill_Span (Current, Y, 1, Value);
               end if;

               Current := @ + 1;
               Upper   := @ + One;
            end if;
         end loop;
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
         Cell     : Cell_Coverage;
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

      procedure Append_Left_Vertex (Point : GFX.Points.GF_Point);

      procedure Append_Right_Vertex (Point : GFX.Points.GF_Point);

      procedure Adjust_Y
        (Chain    : in out Vertex_Array;
         First    : in out Positive;
         Last     : in out Natural;
         Y_Top    : Fixed_16;
         Y_Bottom : Fixed_16);

      ------------------------
      -- Append_Left_Vertex --
      ------------------------

      procedure Append_Left_Vertex (Point : GFX.Points.GF_Point) is
         X : constant Fixed_16 := Snap_To_Subpixel (Point.X);
         Y : constant Fixed_16 := Snap_To_Subpixel (Point.Y);
         pragma Warnings (Off);
         Item : constant Vertex :=
           (X      => X,
            Y      => Y,
            Column => To_Pixel (X),
            Row    => To_Pixel (Y),
            Step_X => <>,
            Step_Y => <>);
         pragma Warnings (On);

      begin
         if Left_Last = 0 or else Left_Vertex (Left_Last) /= Item then
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
         Item : constant Vertex :=
           (X      => Snap_To_Subpixel (Point.X),
            Y      => Snap_To_Subpixel (Point.Y),
            Column => To_Pixel (X),
            Row    => To_Pixel (Y),
            Step_X => <>,
            Step_Y => <>);
         pragma Warnings (On);

      begin
         if Right_Last = 0 or else Right_Vertex (Right_Last) /= Item then
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
            Chain (First).Row := To_Pixel (Y_Top);
         end if;

         if Y_Bottom < Chain (Last).Y then
            pragma Assert
              (First < Last
                 and then Chain (Last).X = Chain (Last - 1).X
                 and then Chain (Last - 1).Y <= Y_Bottom);
            Chain (Last).Y := Y_Bottom;
            Chain (Last).Row := To_Pixel (Y_Bottom);
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

   begin
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
         declare
            DX : constant Fixed_16 :=
              Left_Vertex (J).X - Left_Vertex (J + 1).X;
            DY : constant Fixed_16 :=
              Left_Vertex (J).Y - Left_Vertex (J + 1).Y;

         begin
            Left_Vertex (J).Step_X := Divide_Saturated (DX, DY);
            Left_Vertex (J).Step_Y := Divide_Saturated (DY, DX);
         end;
      end loop;

      for J in Right_First .. Right_Last - 1 loop
         declare
            DX : constant Fixed_16 :=
              Right_Vertex (J).X - Right_Vertex (J + 1).X;
            DY : constant Fixed_16 :=
              Right_Vertex (J).Y - Right_Vertex (J + 1).Y;

         begin
            Right_Vertex (J).Step_X := Divide_Saturated (DX, DY);
            Right_Vertex (J).Step_Y := Divide_Saturated (DY, DX);
         end;
      end loop;

      --  5. Rasterize the closed contour between the two clipped chains.

      Rasterize_Contour
        (Left_Vertex (Left_First .. Left_Last),
         Right_Vertex (Right_First .. Right_Last),
         --  Left_First, Right_First,
         --  Left_Last, Right_Last,
         Fill_Span);
   end Rasterize_Line;

   ----------------------
   -- Set_Column_Range --
   ----------------------

   procedure Set_Column_Range (Part : in out Segment_DDA) is
      X_Min : constant Fixed_16 := Min (Part.X_Top, Part.X_Bottom);
      X_Max : constant Fixed_16 := Max (Part.X_Top, Part.X_Bottom);

   begin
      Part.Column_First := To_Pixel (X_Min);
      Part.Column_Last  := To_Pixel (X_Max);

      if X_Min /= X_Max and Is_Upper_Bound (X_Max) then
         --  The portion spans a nonzero X range and ends on a column
         --  boundary. Exclude the column to its right, which it only touches.

         Part.Column_Last := @ - 1;
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
      --  Empty_Area := Left = Right or else Top = Bottom;
      --  if Empty_Area then
      --     return;
      --  end if;

      GFX.Implementation.Clipping.Set_Bound
        (Self  => Rendering_Area,
         X_Min => Left,
         X_Max => Right,
         Y_Min => Top,
         Y_Max => Bottom);

      --  Clip_X_Lower_Bound := Pixel_Lower_Bound (Left);
   end Set_Rendering_Area;

end GFX.Rasteriser.DDA4;
