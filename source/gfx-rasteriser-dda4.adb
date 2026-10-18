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

      At_Column_Lower : Fixed_16 := Zero;
      At_Column_Upper : Fixed_16 := Zero;
      --  Valid only if the corresponding boundary intersects this portion.
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

   procedure Accumulate_Interior
     (Chain    : Vertex_Array;
      Part     : in out Segment_DDA;
      Reversed : Boolean;
      Coverage : in out Cell_Coverage);
   --  Advance Part's column intersections by one pixel and add its signed
   --  height and twice-area to Coverage. Both boundaries of the new cell
   --  must intersect the same segment, with no endpoint or activation event
   --  in this cell. Reversed traverses the segment against chain order.

   --  Initialize once before the first row.

   Rendering_Area : GFX.Implementation.Clipping.GF_Clipping_Rectangle;

   type Segment_Set is mod 2 ** 3;
   --  One bit per possible segment of a clipped rectangle side. This stores
   --  pending indices only, not intersections or an event queue.

   type Row_Side is record
      Top_Segment    : Segment_DDA;
      Bottom_Segment : Segment_DDA;
      --  Portions extending across the last processed cell's right boundary,
      --  in chain order. Only Top_Segment is used for a single crossing.
      --  A zero Segment marks an inactive slot; its other fields are ignored.
      First_Segment  : Natural := 0;
      Last_Segment   : Natural := 0;
      First_X        : Fixed_16 := Zero;
      First_Y        : Fixed_16 := Zero;
      Last_X         : Fixed_16 := Zero;
      Last_Y         : Fixed_16 := Zero;
      --  Full range and clipped endpoints for this row, including portions
      --  lying entirely to the right of the first cell.
      --  If two left portions cross the first cell's right boundary, only
      --  the right side can have additional, entirely deferred portions.
      Column_First   : GX_Integer := 0;
      Column_Last    : GX_Integer := -1;
      Pending        : Segment_Set := 0;
      Next_Segment   : Natural := 0;
      Next_Column    : GX_Integer := GX_Integer'Last;
      Change_Column  : GX_Integer := GX_Integer'Last;
      --  Nearest pending activation or active segment's final cell.
   end record;

   procedure Prepare_Row
     (Left           : Vertex_Array;
      Right          : Vertex_Array;
      Row_Lower      : Fixed_16;
      Row_Upper      : Fixed_16;
      Left_Cursor    : in out Row_Cursor;
      Right_Cursor   : in out Row_Cursor;
      Left_Side      : out Row_Side;
      Right_Side     : out Row_Side;
      First_Column   : out GX_Integer;
      First_Coverage : out Cell_Coverage)
     with Pre => Row_Upper = Row_Lower + One
       and Left_Cursor.Segment /= 0
       and Right_Cursor.Segment /= 0
       and Ghost.Only_First_Segment_Can_Move_Left (Left);
   --  Consume only portions belonging to this row. Initialize and accumulate
   --  First_Coverage for First_Column = Left_Side.Column_First, without
   --  integrating or emitting it. Return column states for the current row
   --  and advance both cursors to the next row (or mark them exhausted).

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
   --  advance Cursor to the next row. No Row_Side or pending set is needed.

   function Belongs_To_Same_Pixel (A : Fixed_16; B : Fixed_16) return Boolean;
   --  Return True if there is an integer N such that both A and B lie in
   --  the closed interval [N, N + 1] along one axis. Either coordinate may
   --  lie on the pixel's upper boundary, regardless of argument order.

   function Is_Upper_Bound (A : Fixed_16) return Boolean;
   --  Check whether A lies on an exclusive upper pixel boundary.
   --  Every integral coordinate is the upper boundary of the previous pixel.

   procedure Accumulate
     (Self         : in out Cell_Coverage;
      Column_Upper : Fixed_16;
      X_1          : Fixed_16;
      Y_1          : Fixed_16;
      X_2          : Fixed_16;
      Y_2          : Fixed_16)
     with Pre =>
       Is_Upper_Bound (Column_Upper)
         and Column_Upper - One <= X_1 and X_1 <= Column_Upper
         and Column_Upper - One <= X_2 and X_2 <= Column_Upper
         and Belongs_To_Same_Pixel (Y_1, Y_2),
       Post =>
         (if Y_1 = Y_2 then
            Self.Coverage = Self.Coverage'Old
              and then Self.Twice_Area = Self.Twice_Area'Old)
            and then
              (if X_1 = Column_Upper and then X_2 = Column_Upper then
                 Self.Twice_Area = Self.Twice_Area'Old);
   --  Accumulate the contribution of a directed segment contained in one
   --  cell.

   procedure Integrate
     (Accumulator : in out Fixed_6;
      Coverage    : Cell_Coverage;
      Alpha       : out Grayscale) with Export;

   procedure Step_Column (Part : in out Segment_DDA; Step : Fixed_16);

   ----------------
   -- Accumulate --
   ----------------

   procedure Accumulate
     (Self         : in out Cell_Coverage;
      Column_Upper : Fixed_16;
      X_1          : Fixed_16;
      Y_1          : Fixed_16;
      X_2          : Fixed_16;
      Y_2          : Fixed_16)
   is
      Delta_Y : constant Fixed_6  := Floor (Y_2) - Floor (Y_1);

   begin
      Self.Coverage   := @ + Delta_Y;
      Self.Twice_Area :=
        @ + Delta_Y * Floor ((Column_Upper - X_1) + (Column_Upper - X_2));
   end Accumulate;

   -------------------------
   -- Accumulate_Interior --
   -------------------------

   procedure Accumulate_Interior
     (Chain    : Vertex_Array;
      Part     : in out Segment_DDA;
      Reversed : Boolean;
      Coverage : in out Cell_Coverage)
   is
      Delta_Y : Fixed_6;

   begin
      --  Both cell boundaries intersect this segment. Membership and
      --  endpoint checks are performed only in columns where states change.

      Step_Column (Part, Chain (Part.Segment).Step_Y);

      Delta_Y := Floor (Part.At_Column_Upper) - Floor (Part.At_Column_Lower);

      --  The difference follows increasing X. Reverse its sign when the
      --  contour traverses this portion from right to left.
      if not ((Part.X_Top < Part.X_Bottom) xor Reversed) then
         Delta_Y := Zero - Delta_Y;
      end if;

      --  The endpoints lie on opposite cell boundaries, so the factor
      --  (Column_Upper - X_1) + (Column_Upper - X_2) is exactly one pixel.
      --  Multiplying by Fixed_6 One converts the signed height to Fixed_12
      --  twice-area units, preserving the rounding used by Accumulate.
      Coverage.Coverage   := @ + Delta_Y;
      Coverage.Twice_Area := @ + Delta_Y * One;
   end Accumulate_Interior;

   ---------------------------
   -- Belongs_To_Same_Pixel --
   ---------------------------

   function Belongs_To_Same_Pixel
     (A : Fixed_16; B : Fixed_16) return Boolean
   is
      Pixel_A : constant GX_Integer := To_Pixel (A);
      Pixel_B : constant GX_Integer := To_Pixel (B);

   begin
      if Pixel_A = Pixel_B then
         --  Both coordinates lie in the same half-open pixel interval.

         return True;

      elsif Pixel_B = Pixel_A + 1 then
         --  B must be exactly on the upper boundary of A's pixel.

         return B = To_Fixed_16 (Pixel_B);

      elsif Pixel_A = Pixel_B + 1 then
         --  A must be exactly on the upper boundary of B's pixel.

         return A = To_Fixed_16 (Pixel_A);
      end if;

      --  Non-adjacent pixels cannot share a closed pixel interval.

      return False;
   end Belongs_To_Same_Pixel;

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
   -- Column_Portion --
   --------------------

   function Column_Portion
     (Chain : Vertex_Array;
      Side  : Row_Side;
      Index : Positive) return Segment_DDA
   is
      Part : Segment_DDA;
   begin
      Part.Segment := Index;
      Part.X_Top := Chain (Index).X;
      Part.Y_Top := Chain (Index).Y;
      Part.X_Bottom := Chain (Index + 1).X;
      Part.Y_Bottom := Chain (Index + 1).Y;

      if Index = Side.First_Segment then
         Part.X_Top := Side.First_X;
         Part.Y_Top := Side.First_Y;
      end if;

      if Index = Side.Last_Segment then
         Part.X_Bottom := Side.Last_X;
         Part.Y_Bottom := Side.Last_Y;
      end if;

      Set_Column_Range (Part);
      return Part;
   end Column_Portion;

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

   -------------------
   -- Update_Change --
   -------------------

   procedure Update_Change (Side : in out Row_Side) is
   begin
      Side.Change_Column := Side.Next_Column;

      if Side.Top_Segment.Segment /= 0 then
         Side.Change_Column := GX_Integer'Min
           (Side.Change_Column, Side.Top_Segment.Column_Last);
      end if;

      if Side.Bottom_Segment.Segment /= 0 then
         Side.Change_Column := GX_Integer'Min
           (Side.Change_Column, Side.Bottom_Segment.Column_Last);
      end if;
   end Update_Change;

   ------------------
   -- Find_Pending --
   ------------------

   procedure Find_Pending (Chain : Vertex_Array; Side : in out Row_Side) is
   begin
      Side.Next_Segment := 0;
      Side.Next_Column := GX_Integer'Last;

      --  Called only after activation, never from the interior pixel loop.
      --  Only pending segments of this row have their geometry inspected.
      for Index in Side.First_Segment .. Side.Last_Segment loop
         if (Side.Pending and 2 ** (Index - Chain'First)) /= 0 then
            declare
               Part : constant Segment_DDA :=
                 Column_Portion (Chain, Side, Index);
            begin
               if Part.Column_First < Side.Next_Column then
                  Side.Next_Segment := Index;
                  Side.Next_Column := Part.Column_First;
               end if;
            end;
         end if;
      end loop;
   end Find_Pending;

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
         At_Column_Lower => Zero,
         At_Column_Upper => Zero,
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

   -----------------
   -- Prepare_Row --
   -----------------

   procedure Prepare_Row
     (Left           : Vertex_Array;
      Right          : Vertex_Array;
      Row_Lower      : Fixed_16;
      Row_Upper      : Fixed_16;
      Left_Cursor    : in out Row_Cursor;
      Right_Cursor   : in out Row_Cursor;
      Left_Side      : out Row_Side;
      Right_Side     : out Row_Side;
      First_Column   : out GX_Integer;
      First_Coverage : out Cell_Coverage)
   is
      Column_Lower : Fixed_16;
      Column_Upper : Fixed_16;

      procedure Collect
        (Chain    : Vertex_Array;
         Cursor   : in out Row_Cursor;
         Reversed : Boolean;
         Side     : out Row_Side);

      -------------
      -- Collect --
      -------------

      procedure Collect
        (Chain    : Vertex_Array;
         Cursor   : in out Row_Cursor;
         Reversed : Boolean;
         Side     : out Row_Side)
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
             --  The positive Y displacement is at most one pixel, as
             --  required by Multiply_Scaled.
             and then Upper_Bound < Chain (Cursor.Segment + 1).Y;
             --  The boundary is inside the segment; its final vertex is
             --  handled separately by the caller.
         --  Set Cursor.At_Row_Upper to the X coordinate where the current
         --  segment intersects the row boundary Upper_Bound. The cursor must
         --  be at the segment's first vertex, strictly above Upper_Bound and
         --  at most one pixel away; the segment must end below Upper_Bound.
         --  Cursor.X, Cursor.Y and Cursor.Segment are left unchanged.

         --------------------------
         -- Prepare_Intersection --
         --------------------------

         procedure Prepare_Intersection (Upper_Bound : Fixed_16) is
         begin
            --  The cursor is at the segment's first vertex, and Upper_Bound
            --  is inside the segment, at most one pixel below that vertex.
            --  Truncation toward zero keeps the displacement within the
            --  segment without clamping.

            Cursor.At_Row_Upper := Cursor.X
              + Multiply_Scaled
                  (Chain (Cursor.Segment).Step_X, Upper_Bound - Cursor.Y);
         end Prepare_Intersection;

      begin
         Side := (others => <>);

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

                  if Side.First_Segment = 0 then
                     Side.First_Segment := Index;
                     Side.First_X       := Part.X_Top;
                     Side.First_Y       := Part.Y_Top;
                     Side.Column_First  := Part.Column_First;
                     Side.Column_Last   := Part.Column_Last;

                     if Reversed then
                        --  Only the first left portion can move left. Later
                        --  portions are vertical or move right, so its range
                        --  determines the first cell of this row.

                        First_Column := Part.Column_First;
                        Column_Lower := Pixel_Lower_Bound (First_Column);
                        Column_Upper := Column_Lower + One;
                     end if;

                  else
                     Side.Column_First :=
                       GX_Integer'Min (@, Part.Column_First);
                     Side.Column_Last  :=
                       GX_Integer'Max (@, Part.Column_Last);
                  end if;

                  Side.Last_Segment := Index;
                  Side.Last_X := Part.X_Bottom;
                  Side.Last_Y := Part.Y_Bottom;

                  pragma Assert (First_Column <= Part.Column_First);

                  declare
                     X_Top : Fixed_16 := Part.X_Top;
                     Y_Top : Fixed_16 := Part.Y_Top;
                     X_Bottom : Fixed_16 := Part.X_Bottom;
                     Y_Bottom : Fixed_16 := Part.Y_Bottom;
                     X_Min : constant Fixed_16 := Min (X_Top, X_Bottom);
                     X_Max : constant Fixed_16 := Max (X_Top, X_Bottom);

                  begin
                     --  Keep indices for portions starting in later cells.
                     if Part.Column_First > First_Column then
                        Side.Pending := Side.Pending
                          or 2 ** (Part.Segment - Chain'First);

                        if Part.Column_First < Side.Next_Column then
                           Side.Next_Column := Part.Column_First;
                           Side.Next_Segment := Part.Segment;
                        end if;

                     else
                        if X_Min <= Column_Lower
                          and Column_Lower <= X_Max
                        then
                           Part.At_Column_Lower :=
                             Intersection_Y (Chain, Part, Column_Lower);
                        end if;

                        if X_Min <= Column_Upper
                          and Column_Upper <= X_Max
                        then
                           Part.At_Column_Upper :=
                             Intersection_Y (Chain, Part, Column_Upper);
                        end if;

                        if X_Top > Column_Upper then
                           X_Top := Column_Upper;
                           Y_Top := Part.At_Column_Upper;
                        elsif X_Top < Column_Lower then
                           X_Top := Column_Lower;
                           Y_Top := Part.At_Column_Lower;
                        end if;

                        if X_Bottom > Column_Upper then
                           X_Bottom := Column_Upper;
                           Y_Bottom := Part.At_Column_Upper;
                        elsif X_Bottom < Column_Lower then
                           X_Bottom := Column_Lower;
                           Y_Bottom := Part.At_Column_Lower;
                        end if;

                        if Reversed then
                           Accumulate (First_Coverage, Column_Upper,
                             X_Bottom, Y_Bottom, X_Top, Y_Top);
                        else
                           Accumulate (First_Coverage, Column_Upper,
                             X_Top, Y_Top, X_Bottom, Y_Bottom);
                        end if;

                        if First_Column < Part.Column_Last then
                           if Side.Top_Segment.Segment = 0 then
                              Side.Top_Segment := Part;
                           else
                              pragma Assert (Side.Bottom_Segment.Segment = 0);
                              Side.Bottom_Segment := Part;
                           end if;
                        end if;
                     end if;
                  end;

                  if Row_Upper < Last.Y then
                     Cursor.X := Part.X_Bottom;
                     Cursor.Y := Row_Upper;

                     if Row_Upper + One < Last.Y then
                        --  Continue this segment by one DDA step.
                        --  Truncation toward zero keeps X within the segment.
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
      end Collect;

   begin
      First_Coverage := (Zero, Zero);

      Collect (Left, Left_Cursor, True, Left_Side);
      Collect (Right, Right_Cursor, False, Right_Side);
      pragma Assert
        (Left_Side.First_Segment /= 0 and Right_Side.First_Segment /= 0);
      pragma Assert (Left_Side.Column_First <= Right_Side.Column_First);

      Update_Change (Left_Side);
      Update_Change (Right_Side);
   end Prepare_Row;

   -----------------------
   -- Accumulate_Portion --
   -----------------------

   procedure Accumulate_Portion
     (Part         : Segment_DDA;
      Column_Upper : Fixed_16;
      Reversed     : Boolean;
      Coverage     : in out Cell_Coverage)
   is
      Column_Lower : constant Fixed_16 := Column_Upper - One;
      X_Top    : Fixed_16 := Part.X_Top;
      Y_Top    : Fixed_16 := Part.Y_Top;
      X_Bottom : Fixed_16 := Part.X_Bottom;
      Y_Bottom : Fixed_16 := Part.Y_Bottom;
   begin
      if X_Top < Column_Lower then
         X_Top := Column_Lower;
         Y_Top := Part.At_Column_Lower;
      elsif X_Top > Column_Upper then
         X_Top := Column_Upper;
         Y_Top := Part.At_Column_Upper;
      end if;

      if X_Bottom < Column_Lower then
         X_Bottom := Column_Lower;
         Y_Bottom := Part.At_Column_Lower;
      elsif X_Bottom > Column_Upper then
         X_Bottom := Column_Upper;
         Y_Bottom := Part.At_Column_Upper;
      end if;

      if Reversed then
         Accumulate (Coverage, Column_Upper,
           X_Bottom, Y_Bottom, X_Top, Y_Top);
      else
         Accumulate (Coverage, Column_Upper,
           X_Top, Y_Top, X_Bottom, Y_Bottom);
      end if;
   end Accumulate_Portion;

   -----------------
   -- Step_Column --
   -----------------

   procedure Step_Column (Part : in out Segment_DDA; Step : Fixed_16) is
   begin
      Part.At_Column_Lower := Part.At_Column_Upper;
      Part.At_Column_Upper := Part.At_Column_Lower
        + Max (Part.Y_Top - Part.At_Column_Lower,
            Min (Part.Y_Bottom - Part.At_Column_Lower, Step));
   end Step_Column;

   -----------------------
   -- Accumulate_Change --
   -----------------------

   procedure Accumulate_Change
     (Chain        : Vertex_Array;
      Side         : in out Row_Side;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Reversed     : Boolean;
      Coverage     : in out Cell_Coverage)
   is
      procedure Keep (Part : Segment_DDA);

      procedure Continue_Portion (Part : in out Segment_DDA);

      procedure Keep (Part : Segment_DDA) is
      begin
         if Column < Part.Column_Last then
            if Side.Top_Segment.Segment = 0 then
               Side.Top_Segment := Part;
            else
               pragma Assert (Side.Bottom_Segment.Segment = 0);

               if Part.Segment < Side.Top_Segment.Segment then
                  Side.Bottom_Segment := Side.Top_Segment;
                  Side.Top_Segment := Part;
               else
                  Side.Bottom_Segment := Part;
               end if;
            end if;
         end if;
      end Keep;

      ----------------------
      -- Continue_Portion --
      ----------------------

      procedure Continue_Portion (Part : in out Segment_DDA) is
      begin
         if Part.Segment = 0 then
            return;
         end if;

         if Column_Upper < Max (Part.X_Top, Part.X_Bottom) then
            Step_Column (Part, Chain (Part.Segment).Step_Y);

         else
            --  No intersection beyond the endpoint is needed in this cell.

            Part.At_Column_Lower := Part.At_Column_Upper;
         end if;

         Accumulate_Portion (Part, Column_Upper, Reversed, Coverage);

         if Column >= Part.Column_Last then
            --  Geometry is no longer read after this portion is exhausted.

            Part.Segment := 0;
         end if;
      end Continue_Portion;

   begin
      --  Advance active portions in place, preserving their chain order.
      --  The common single-portion case needs no record copies or clearing.

      Continue_Portion (Side.Top_Segment);
      Continue_Portion (Side.Bottom_Segment);

      if Side.Top_Segment.Segment = 0
        and Side.Bottom_Segment.Segment /= 0
      then
         --  Keep a sole surviving portion in the top slot before activation
         --  of pending portions, as required by Keep and Draw_Interior.

         Side.Top_Segment := Side.Bottom_Segment;
         Side.Bottom_Segment.Segment := 0;
      end if;

      while Side.Next_Segment /= 0 and then Side.Next_Column = Column loop
         declare
            Index : constant Positive := Side.Next_Segment;
            Part  : Segment_DDA := Column_Portion (Chain, Side, Index);
            X_Min : constant Fixed_16 := Min (Part.X_Top, Part.X_Bottom);
            X_Max : constant Fixed_16 := Max (Part.X_Top, Part.X_Bottom);
         begin
            Side.Pending := Side.Pending
              and not (2 ** (Index - Chain'First));

            --  A newly activated portion starts inside this cell. Its first
            --  right-boundary intersection is initialized from the vertex;
            --  later intersections use Step_Column exclusively.
            if X_Min <= Column_Upper and Column_Upper <= X_Max
              and X_Min /= X_Max
            then
               Part.At_Column_Upper :=
                 Intersection_Y (Chain, Part, Column_Upper);
            end if;

            Accumulate_Portion (Part, Column_Upper, Reversed, Coverage);
            Keep (Part);
            Find_Pending (Chain, Side);
         end;
      end loop;

      Update_Change (Side);
   end Accumulate_Change;

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
      Left_Side, Right_Side     : Row_Side;
      Row          : GX_Integer := To_Pixel (Left (Left'First).Y);
      Row_Lower    : Fixed_16 := Pixel_Lower_Bound (Row);
      Row_Upper    : Fixed_16 := Row_Lower + One;
      Column       : GX_Integer;
      Column_Upper : Fixed_16;
      Coverage     : Cell_Coverage;
      Accumulator  : Fixed_6;
      Alpha        : Grayscale;

      procedure Emit (Width : Device_Pixel_Count := 1);
      procedure Add_None;
      procedure Add_Left_One;
      procedure Add_Left_Two;
      procedure Add_Right_One;
      procedure Add_Right_Two;

      ----------
      -- Emit --
      ----------

      procedure Emit (Width : Device_Pixel_Count := 1) is
      begin
         Integrate (Accumulator, Coverage, Alpha);

         if Alpha /= 0 then
            Fill_Span (Column, Row, Width, Alpha);
         end if;
      end Emit;

      procedure Add_None is
      begin
         null;
      end Add_None;

      procedure Add_Left_One is
      begin
         Accumulate_Interior (Left, Left_Side.Top_Segment,
           True, Coverage);
      end Add_Left_One;

      procedure Add_Left_Two is
      begin
         Add_Left_One;
         Accumulate_Interior (Left, Left_Side.Bottom_Segment,
           True, Coverage);
      end Add_Left_Two;

      procedure Add_Right_One is
      begin
         Accumulate_Interior (Right, Right_Side.Top_Segment,
           False, Coverage);
      end Add_Right_One;

      procedure Add_Right_Two is
      begin
         Add_Right_One;
         Accumulate_Interior (Right, Right_Side.Bottom_Segment,
           False, Coverage);
      end Add_Right_Two;

      generic
         with procedure Add_Left;
         with procedure Add_Right;
      procedure Draw_Run (Last : GX_Integer);

      procedure Draw_Run (Last : GX_Integer) is
      begin
         while Column <= Last loop
            Coverage := (Zero, Zero);
            Add_Left;
            Add_Right;
            Emit;
            Column := @ + 1;
            Column_Upper := @ + One;
         end loop;
      end Draw_Run;

      --  Choose a specialized loop once per range. No membership or active
      --  segment count checks occur inside these pixel loops.
      procedure Draw_01 is new Draw_Run (Add_None, Add_Right_One);
      procedure Draw_02 is new Draw_Run (Add_None, Add_Right_Two);
      procedure Draw_10 is new Draw_Run (Add_Left_One, Add_None);
      procedure Draw_11 is new Draw_Run (Add_Left_One, Add_Right_One);
      procedure Draw_12 is new Draw_Run (Add_Left_One, Add_Right_Two);
      procedure Draw_20 is new Draw_Run (Add_Left_Two, Add_None);
      procedure Draw_21 is new Draw_Run (Add_Left_Two, Add_Right_One);
      procedure Draw_22 is new Draw_Run (Add_Left_Two, Add_Right_Two);

      procedure Draw_Interior (Last : GX_Integer) is
      begin
         if Left_Side.Top_Segment.Segment = 0 then
            if Right_Side.Top_Segment.Segment = 0 then
               --  No boundary crosses this range. Its occupied row height
               --  is already represented by Accumulator, including partial
               --  first and last rows.
               Coverage := (Zero, Zero);
               Emit (Device_Pixel_Count (Last - Column + 1));
               Column := Last + 1;
               Column_Upper := Pixel_Lower_Bound (Column) + One;
            elsif Right_Side.Bottom_Segment.Segment = 0 then
               Draw_01 (Last);
            else
               Draw_02 (Last);
            end if;

         elsif Left_Side.Bottom_Segment.Segment = 0 then
            if Right_Side.Top_Segment.Segment = 0 then
               Draw_10 (Last);
            elsif Right_Side.Bottom_Segment.Segment = 0 then
               Draw_11 (Last);
            else
               Draw_12 (Last);
            end if;

         else
            if Right_Side.Top_Segment.Segment = 0 then
               Draw_20 (Last);
            elsif Right_Side.Bottom_Segment.Segment = 0 then
               Draw_21 (Last);
            else
               Draw_22 (Last);
            end if;
         end if;
      end Draw_Interior;

      procedure Draw_Simple_Row;
      --  Draw a complete row crossed by one segment per side. Only the
      --  first and last columns of each portion need endpoint handling;
      --  ranges between them use specialized loops without Row_Side events.

      procedure Draw_Simple_Row is
         Left_Part, Right_Part : Segment_DDA;

         procedure Add_Left;
         procedure Add_Right;
         procedure Add_Endpoint
           (Chain    : Vertex_Array;
            Part     : in out Segment_DDA;
            Reversed : Boolean);
         function Next_Endpoint (Part : Segment_DDA) return GX_Integer;

         procedure Add_Left is
         begin
            Accumulate_Interior (Left, Left_Part, True, Coverage);
         end Add_Left;

         procedure Add_Right is
         begin
            Accumulate_Interior (Right, Right_Part, False, Coverage);
         end Add_Right;

         procedure Draw_Left is new Draw_Run (Add_Left, Add_None);
         procedure Draw_Both is new Draw_Run (Add_Left, Add_Right);
         procedure Draw_Right is new Draw_Run (Add_None, Add_Right);

         procedure Add_Endpoint
           (Chain    : Vertex_Array;
            Part     : in out Segment_DDA;
            Reversed : Boolean)
         is
            X_Min        : constant Fixed_16 :=
              Min (Part.X_Top, Part.X_Bottom);
            X_Max        : constant Fixed_16 :=
              Max (Part.X_Top, Part.X_Bottom);
            Column_Lower : constant Fixed_16 := Column_Upper - One;

         begin
            if Column not in Part.Column_First .. Part.Column_Last then
               return;
            end if;

            if Column = Part.Column_First then
               --  Initialize intersections from the original vertex, as in
               --  the general path. Later columns retain the same DDA steps.
               if X_Min <= Column_Lower and Column_Lower <= X_Max then
                  Part.At_Column_Lower :=
                    Intersection_Y (Chain, Part, Column_Lower);
               end if;

               if X_Min <= Column_Upper and Column_Upper <= X_Max then
                  Part.At_Column_Upper :=
                    Intersection_Y (Chain, Part, Column_Upper);
               end if;

            elsif Column_Upper < X_Max then
               Step_Column (Part, Chain (Part.Segment).Step_Y);
            else
               Part.At_Column_Lower := Part.At_Column_Upper;
            end if;

            Accumulate_Portion (Part, Column_Upper, Reversed, Coverage);
         end Add_Endpoint;

         function Next_Endpoint (Part : Segment_DDA) return GX_Integer is
         begin
            if Column <= Part.Column_First then
               return Part.Column_First;
            elsif Column <= Part.Column_Last then
               return Part.Column_Last;
            else
               return Right_Part.Column_Last + 1;
            end if;
         end Next_Endpoint;

      begin
         Prepare_Simple_Portion
           (Left, Row_Lower, Row_Upper, Left_Cursor, Left_Part);
         Prepare_Simple_Portion
           (Right, Row_Lower, Row_Upper, Right_Cursor, Right_Part);
         pragma Assert (Left_Part.Column_First <= Right_Part.Column_First);

         Column := Left_Part.Column_First;
         Column_Upper := Pixel_Lower_Bound (Column) + One;

         while Column <= Right_Part.Column_Last loop
            declare
               Change : constant GX_Integer := GX_Integer'Min
                 (Next_Endpoint (Left_Part), Next_Endpoint (Right_Part));
            begin
               pragma Assert (Column <= Change);

               if Column = Change then
                  Coverage := (Zero, Zero);
                  Add_Endpoint (Left, Left_Part, True);
                  Add_Endpoint (Right, Right_Part, False);
                  Emit;
                  Column := @ + 1;
                  Column_Upper := @ + One;

               elsif Column <= Left_Part.Column_Last then
                  if Column >= Right_Part.Column_First then
                     Draw_Both (Change - 1);
                  else
                     Draw_Left (Change - 1);
                  end if;

               elsif Column >= Right_Part.Column_First then
                  Draw_Right (Change - 1);

               else
                  --  No boundary crosses this range. Preserve the general
                  --  path's single span and accumulated coverage.
                  Coverage := (Zero, Zero);
                  Emit (Device_Pixel_Count (Change - Column));
                  Column := Change;
                  Column_Upper := Pixel_Lower_Bound (Column) + One;
               end if;
            end;
         end loop;
      end Draw_Simple_Row;

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
            Prepare_Row (Left, Right, Row_Lower, Row_Upper,
              Left_Cursor, Right_Cursor, Left_Side, Right_Side,
              Column, Coverage);

            Column_Upper := Pixel_Lower_Bound (Column) + One;
            Emit;

            Column := @ + 1;
            Column_Upper := @ + One;

            while Column <= Right_Side.Column_Last loop
               declare
                  Change : constant GX_Integer := GX_Integer'Min
                    (Right_Side.Column_Last + 1,
                     GX_Integer'Min
                       (Left_Side.Change_Column, Right_Side.Change_Column));
               begin
                  pragma Assert (Column <= Change);

                  if Column < Change then
                     Draw_Interior (Change - 1);

                  else
                     Coverage := (Zero, Zero);
                     Accumulate_Change (Left, Left_Side, Column,
                       Column_Upper, True, Coverage);
                     Accumulate_Change (Right, Right_Side, Column,
                       Column_Upper, False, Coverage);
                     Emit;
                     Column := @ + 1;
                     Column_Upper := @ + One;
                  end if;
               end;
            end loop;
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
