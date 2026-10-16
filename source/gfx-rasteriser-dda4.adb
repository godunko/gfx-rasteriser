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

   Rendering_Area     : GFX.Implementation.Clipping.GF_Clipping_Rectangle;
   --  Clip_X_Lower_Bound : Fixed_16;
   --  Empty_Area : Boolean := True;

   type Vertex is record
      X      : Fixed_16;
      Y      : Fixed_16;
      Column : GX_Integer;
      Row    : GX_Integer;
      Step_X : Fixed_16;
      --  Inverse slope, ΔX / ΔY, step on X while Y changes by one pixel.
      Step_Y : Fixed_16;
      --  Slope, ΔY / ΔX, step on Y while X changes by one pixel.
   end record;

   type Vertex_Array is array (Positive range <>) of Vertex;

   type Cell_Coverage is record
      Coverage   : Fixed_6;
      --  Vertical height of the line inside the cell.
      Twice_Area : Fixed_12;
      --  Twice the signed area of the trapezoid matching the right edge.
   end record;

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

   procedure Integrate
     (Accumulator : in out Fixed_6;
      Coverage    : Cell_Coverage;
      Alpha       : out Grayscale) with Export;

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
      Self.Twice_Area := @ + Delta_Y * Floor (2 * Column_Upper - (X_1 + X_2));
   end Accumulate;

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

   --  function Magnitude (Value : Fixed_16) return Fixed_16 is
   --    (if Value < Zero then -Value else Value);
   --
   --  --  Each side advances independently through its top-to-bottom chain.
   --  --  The two permanent cursors carry DDA coordinates between rows.
   --  --  Each row caches its side segments for all boundary-cell calculations.
   --  type Side_Cursor is record
   --     Index   : Positive;
   --     A, B    : Vertex;
   --     Current : Vertex;
   --     Step    : Fixed_16;
   --     X_Major : Boolean;
   --  end record;

   --  procedure Select_Segment
   --    (Self  : in out Side_Cursor;
   --     Chain : Vertex_Array;
   --     Last  : Positive);
   --
   --  procedure Advance
   --    (Self : in out Side_Cursor;
   --     Y : Fixed_16);
   --
   --  procedure Select_Segment
   --    (Self : in out Side_Cursor;
   --     Chain : Vertex_Array;
   --     Last : Positive)
   --  is
   --     DX, DY : Fixed_16;
   --  begin
   --     --  Horizontal segments change X at the same Y, but contribute no
   --     --  signed vertical length or area. Consume all of them at a vertex.
   --     while Self.Index < Last
   --       and then Chain (Self.Index).Y = Chain (Self.Index + 1).Y
   --     loop
   --        Self.Index := @ + 1;
   --        Self.Current := Chain (Self.Index);
   --     end loop;
   --     Self.A := Chain (Self.Index);
   --     Self.B := Chain (Self.Index);
   --     Self.Step := Zero;
   --     Self.X_Major := False;
   --     if Self.Index < Last then
   --        Self.B := Chain (Self.Index + 1);
   --        DX := Self.B.X - Self.A.X;
   --        DY := Self.B.Y - Self.A.Y;
   --        Self.X_Major := Magnitude (DX) > DY;
   --        Self.Step :=
   --          (if Self.X_Major then Divide_Saturated (DY, DX)
   --           else Divide_Saturated (DX, DY));
   --     end if;
   --  end Select_Segment;

   --  procedure Advance
   --    (Self : in out Side_Cursor;
   --     Y : Fixed_16)
   --  is
   --     Target : constant Fixed_16 := Min (Y, Self.B.Y);
   --  begin
   --     if Target = Self.B.Y then
   --        Self.Current := Self.B;
   --     else
   --        if Self.X_Major then
   --           Self.Current.X := Self.A.X +
   --             Max (Min (Zero, Self.B.X - Self.A.X),
   --               Min (Max (Zero, Self.B.X - Self.A.X),
   --                 Divide_Saturated (Target - Self.A.Y, Self.Step)));
   --        else
   --           Self.Current.X := @ + Self.Step * (Target - Self.Current.Y);
   --        end if;
   --        Self.Current.X := Max (Min (Self.A.X, Self.B.X),
   --          Min (Max (Self.A.X, Self.B.X), Self.Current.X));
   --        Self.Current.Y := Target;
   --     end if;
   --  end Advance;

   procedure Rasterize_Contour
     (Left      : Vertex_Array;
      Right     : Vertex_Array;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
     with Pre =>
       Left'Length >= 2 and Right'Length >= 2,
          Export;

   procedure Rasterize_Contour
     (Left, Right : Vertex_Array;
      Fill_Span : not null access procedure
        (X        : Device_Pixel_Index;
         Y        : Device_Pixel_Index;
         Width    : Device_Pixel_Count;
         Coverage : Grayscale))
   is
      pragma Suppress (All_Checks);
      --  use type Grayscale;

      --  `Step_X`/`Step_Y` are saturated values. It is a known fact that they
      --  can overflow corresponding `*_At_*` variables in case of shallow
      --  and steep lines.
      --
      --  XXX Review this after implementation, it might be possible to avoid
      --  overlowing computations somehow. Right now it is almost expected,
      --  until doesn't break results.

      --  Top : constant Fixed_16 := Left (Left'First).Y;
      --  Bottom : constant Fixed_16 := Left (Left'Last).Y;
      --  Left_Side : Side_Cursor :=
      --    (Index => Left'First, A => Left (Left'First), B => Left (Left'First),
      --     Current => Left (Left'First), Step => Zero, X_Major => False);
      --  Right_Side : Side_Cursor :=
      --    (Index => Right'First,
      --     A => Right (Right'First), B => Right (Right'First),
      --     Current => Right (Right'First), Step => Zero, X_Major => False);
      --  Span_X : Device_Pixel_Index := 0;
      --  Span_Width : Device_Pixel_Count := 0;
      --  Span_Coverage : Grayscale := 0;
      --
      --  type Row_Segment is record
      --     Low, High : Vertex;
      --     DY, Step : Fixed_16;
      --     Reversed, X_Major : Boolean;
      --     Boundary : Vertex;
      --     --  Keep the unclamped offset for the next whole-pixel step.
      --     Boundary_Offset : Fixed_16;
      --  end record;
      --  type Row_Segment_Array is
      --    array (Positive range 1 .. 3) of Row_Segment;
      --  type Row_Side is record
      --     Segments : Row_Segment_Array;
      --     Count : Natural := 0;
      --     Minimum, Maximum : Fixed_16;
      --  end record;
      --
      --  procedure Prepare_Row
      --    (Self : in out Side_Cursor;
      --     Chain : Vertex_Array;
      --     Last : Positive;
      --     Y : Fixed_16;
      --     Side : out Row_Side);
      --  procedure Accumulate_Side
      --    (Side : in out Row_Side;
      --     Pixel_Left : Fixed_16;
      --     Negate : Boolean;
      --     Cover, Area : in out Fixed_16);
      --  procedure Flush (Row : Device_Pixel_Index);
      --  procedure Emit
      --    (X, Row : Device_Pixel_Index;
      --     Width : Device_Pixel_Count;
      --     Value : Fixed_16);
      --
      --  procedure Prepare_Row
      --    (Self : in out Side_Cursor;
      --     Chain : Vertex_Array;
      --     Last : Positive;
      --     Y : Fixed_16;
      --     Side : out Row_Side)
      --  is
      --     A, Low, High : Vertex;
      --     DX, DY : Fixed_16;
      --  begin
      --     Side.Count := 0;
      --     Side.Minimum := Self.Current.X;
      --     Side.Maximum := Side.Minimum;
      --     while Self.Current.Y < Y loop
      --        A := Self.Current;
      --        Advance (Self, Y);
      --        Low := A;
      --        High := Self.Current;
      --        if Low.X > High.X then
      --           Low := Self.Current;
      --           High := A;
      --        end if;
      --        DX := High.X - Low.X;
      --        DY := High.Y - Low.Y;
      --        Side.Count := @ + 1;
      --        declare
      --           Segment : Row_Segment renames Side.Segments (Side.Count);
      --        begin
      --           Segment :=
      --             (Low => Low, High => High, DY => DY, Step => Zero,
      --              Reversed => A.X > Self.Current.X,
      --              X_Major => DX >= Magnitude (DY),
      --              Boundary => Low, Boundary_Offset => Zero);
      --           if DX /= Zero then
      --              --  Retain the slope quantization of the row endpoints.
      --              --  Reusing the full edge's Step could change coverage.
      --              --  A Y-major segment occupying a complete row has
      --              --  DY = +/-One. These quotients are exact, and DX
      --              --  is bounded by One, so negation cannot overflow.
      --              Segment.Step :=
      --                (if Segment.X_Major then Divide_Saturated (DY, DX)
      --                 elsif DY = One then DX
      --                 elsif DY = -One then -DX
      --                 else Divide_Saturated (DX, DY));
      --           end if;
      --        end;
      --        Side.Minimum := Min (Side.Minimum, Self.Current.X);
      --        Side.Maximum := Max (Side.Maximum, Self.Current.X);
      --        exit when Self.Current.Y = Y;
      --        Self.Index := @ + 1;
      --        Select_Segment (Self, Chain, Last);
      --        Side.Minimum := Min (Side.Minimum, Self.Current.X);
      --        Side.Maximum := Max (Side.Maximum, Self.Current.X);
      --     end loop;
      --
      --     --  Select the next segment for the following row, after recording
      --     --  this row's extrema. Horizontal segments at Y belong to neither
      --     --  row's area and must not extend the current row's bounds.
      --     if Self.Current = Self.B and then Self.Index < Last then
      --        Self.Index := @ + 1;
      --        Select_Segment (Self, Chain, Last);
      --     end if;
      --  end Prepare_Row;
      --
      --  procedure Accumulate_Side
      --    (Side : in out Row_Side;
      --     Pixel_Left : Fixed_16;
      --     Negate : Boolean;
      --     Cover, Area : in out Fixed_16)
      --  is
      --     Pixel_Right : constant Fixed_16 := Pixel_Left + One;
      --
      --     procedure Accumulate (Segment : in out Row_Segment);
      --
      --     procedure Accumulate (Segment : in out Row_Segment) is
      --        Low : Vertex renames Segment.Low;
      --        High : Vertex renames Segment.High;
      --        P, Q : Vertex;
      --        Signed_Height, Signed_Area : Fixed_16;
      --
      --        function Intersection (X : Fixed_16) return Vertex is
      --        begin
      --           if X /= Segment.Boundary.X then
      --              if Segment.X_Major
      --                and then X = Segment.Boundary.X + One
      --              then
      --                 --  A whole-pixel X step adds an integral number of
      --                 --  Fixed_16 units, preserving product truncation.
      --                 Segment.Boundary_Offset := @ + Segment.Step;
      --              else
      --                 Segment.Boundary_Offset :=
      --                   (if Segment.X_Major
      --                    then Segment.Step * (X - Low.X)
      --                    else Divide_Saturated (X - Low.X, Segment.Step));
      --              end if;
      --              Segment.Boundary :=
      --                (X => X,
      --                 Y => Low.Y +
      --                   Max (Min (Zero, Segment.DY),
      --                     Min (Max (Zero, Segment.DY),
      --                       Segment.Boundary_Offset)));
      --           end if;
      --           return Segment.Boundary;
      --        end Intersection;
      --     begin
      --        if Low.X = High.X then
      --           if Low.X < Pixel_Left or else Low.X >= Pixel_Right then
      --              return;
      --           end if;
      --           P := Low;
      --           Q := High;
      --        else
      --           if Low.X >= Pixel_Right or else High.X <= Pixel_Left then
      --              return;
      --           end if;
      --           P := (if Low.X < Pixel_Left
      --                 then Intersection (Pixel_Left) else Low);
      --           Q := (if High.X > Pixel_Right
      --                 then Intersection (Pixel_Right) else High);
      --        end if;
      --        Signed_Height := Q.Y - P.Y;
      --        Signed_Area :=
      --          ((P.X - Pixel_Left) + (Q.X - Pixel_Left)) * Signed_Height;
      --        if Segment.Reversed xor Negate then
      --           Signed_Height := -Signed_Height;
      --           Signed_Area := -Signed_Area;
      --        end if;
      --        Cover := @ + Signed_Height;
      --        Area := @ + Signed_Area;
      --     end Accumulate;
      --
      --  begin
      --     --  The clipped row geometry and its quantized slopes are shared
      --     --  by all cells, preserving the original coverage arithmetic.
      --     for Index in 1 .. Side.Count loop
      --        Accumulate (Side.Segments (Index));
      --     end loop;
      --  end Accumulate_Side;
      --
      --  procedure Flush (Row : Device_Pixel_Index) is
      --  begin
      --     if Span_Width /= 0 then
      --        Fill_Span (Span_X, Row, Span_Width, Span_Coverage);
      --        Span_Width := 0;
      --     end if;
      --  end Flush;
      --
      --  procedure Emit
      --    (X, Row : Device_Pixel_Index;
      --     Width : Device_Pixel_Count;
      --     Value : Fixed_16)
      --  is
      --     Coverage : constant Grayscale :=
      --       To_Grayscale (Min (One, Max (Zero, Magnitude (Value))));
      --  begin
      --     if Width = 0 then
      --        return;
      --     end if;
      --     if Span_Width /= 0
      --       and then (Coverage /= Span_Coverage
      --                 or else X /= Span_X + Span_Width)
      --     then
      --        Flush (Row);
      --     end if;
      --     if Coverage /= 0 then
      --        if Span_Width = 0 then
      --           Span_X := X;
      --           Span_Coverage := Coverage;
      --        end if;
      --        Span_Width := @ + Width;
      --     end if;
      --  end Emit;

      --  Clip_X_Lower_Bound : constant Fixed_16 :=
      --    Pixel_Lower_Bound
      --      (GFX.Implementation.Clipping.X_Min (Rendering_Area));

      Left_Index  : Positive := Left'First;
      Right_Index : Positive := Right'First;

      Row                  : GX_Integer := To_Pixel (Left (Left_Index).Y);
      Row_Lower            : Fixed_16 := Pixel_Lower_Bound (Row);
      Row_Upper            : Fixed_16 := Pixel_Upper_Bound (Row);
      Column               : GX_Integer;
      Column_Lower         : Fixed_16;
      Column_Upper         : Fixed_16;

      Left_Vertex_Top      : Vertex := Left (Left_Index);
      Left_Vertex_Bottom   : Vertex := Left (Left_Index + 1);
      --  Left_Top             : Fixed_16 := Left_Vertex_Top.X;
      Left_At_Column_Lower : Fixed_16;
      Left_At_Column_Upper : Fixed_16;
      Left_At_Row_Lower    : Fixed_16;
      Left_At_Row_Upper    : Fixed_16;
      Left_Column_First    : GX_Integer;
      Left_Column_Last     : GX_Integer;
      --  Left_Bottom : Fixed_16;
      --  Left_Right  : Fixed_16 := Left ;
      --  Left_Left   : Fixed_16;

      Right_Vertex_Top      : Vertex := Right (Right_Index);
      Right_Vertex_Bottom   : Vertex := Right (Right_Index + 1);
      Right_Top             : Fixed_16 := Right_Vertex_Top.X;
      Right_At_Column_Lower : Fixed_16;
      Right_At_Column_Upper : Fixed_16;
      Right_At_Row_Lower    : Fixed_16;
      Right_At_Row_Upper    : Fixed_16;
      Right_Column_First    : GX_Integer;
      Right_Column_Last     : GX_Integer;

      --  Left_Vertex_Top_X  : Fixed_16 := Left (Left_Index).X;
      --  Left_Vertex_Top_Y  : Fixed_16 := Left (Left_Index).Y;
      --  Left_Vertex_Bottom_X : Fixed_16 := Left (Left_Index + 1).X;
      --  Left_Vertex_Bottom_Y : Fixed_16 := Left (Left_Index + 1).Y;

      Coverage    : Cell_Coverage := (Zero, Zero);
      Accumulator : Fixed_6 := Zero;
      Alpha       : Grayscale;

   begin
      pragma Assert (Left_Vertex_Top.Y = Right_Vertex_Top.Y);
      --  Top edge, if present, is horizontal. No processing necessary, because
      --  it doesn't change coverage and area.

      --  Process left contour.

      Left_At_Row_Lower :=
        Left_Vertex_Top.X
          + Left_Vertex_Top.Step_X * (Row_Lower - Left_Vertex_Top.Y);
      Left_At_Row_Upper :=
        Left_Vertex_Top.X
          + Left_Vertex_Top.Step_X * (Row_Upper - Left_Vertex_Top.Y);

      Left_Column_First :=
        To_Pixel
          (Min
             (Left_Vertex_Top.X,
              (Max (Left_At_Row_Upper, Left_Vertex_Bottom.X))));
      Left_Column_Last  :=
        To_Pixel
          (Max
             (Left_Vertex_Top.X,
              (Min (Left_At_Row_Upper, Left_Vertex_Bottom.X))));
      --  Column_Lower_Bound := Pixel_Lower_Bound (Column);
      --  Column_Upper_Bound := Pixel_Upper_Bound (Column);

      Right_At_Row_Lower :=
        Right_Top + Right_Vertex_Top.Step_X * (Row_Lower - Right_Vertex_Top.Y);
      Right_At_Row_Upper :=
        Right_Top + Right_Vertex_Top.Step_X * (Row_Upper - Right_Vertex_Top.Y);

      Right_Column_First :=
        To_Pixel
          (Min
             (Right_Vertex_Top.X,
              (Max (Right_At_Row_Upper, Right_Vertex_Bottom.X))));
      Right_Column_Last  :=
        To_Pixel
          (Max
             (Right_Vertex_Top.X,
              (Min (Right_At_Row_Upper, Right_Vertex_Bottom.X))));

      Column := Left_Column_First;
      Column_Lower := Pixel_Lower_Bound (Column);
      Column_Upper := Pixel_Upper_Bound (Column);

      Left_At_Column_Lower :=
        Left_Vertex_Top.Y
          + Left_Vertex_Top.Step_Y * (Column_Lower - Left_Vertex_Top.X);
      Left_At_Column_Upper :=
        Left_Vertex_Top.Y
          + Left_Vertex_Top.Step_Y * (Column_Upper - Left_Vertex_Top.X);
      --  Right_At_Column_Lower :=
      --    Right_Vertex_Top.Y
      --      + Right_Vertex_Top.Step_Y * (Column_Lower - Right_Vertex_Top.X);
      --  Right_At_Column_Upper :=
      --    Right_Vertex_Top.Y
      --      + Right_Vertex_Top.Step_Y * (Column_Upper - Right_Vertex_Top.X);

      --  if Left_Vertex_Top.Column = Column then
      --     raise Program_Error;
      --
      --  else
      --     raise Program_Error;
      --  end if;

      --  First row

      if Left_Vertex_Bottom.Row = Row then
         --  Bottom vertex of the current left edge belongs to the current
         --  (first) row, as well as top vertex.

         declare
            X_Top    : Fixed_16 := Left_Vertex_Top.X;
            Y_Top    : Fixed_16 := Left_Vertex_Top.Y;
            X_Bottom : Fixed_16 := Left_Vertex_Bottom.X;
            Y_Bottom : Fixed_16 := Left_Vertex_Bottom.Y;

         begin
            --  The first column contains the leftmost endpoint.
            pragma Assert
              (Column_Lower <= X_Top and Column_Lower <= X_Bottom);

            if X_Top > Column_Upper then
               --  Enter through the right boundary while moving down-left.

               X_Top := Column_Upper;
               Y_Top := Left_At_Column_Upper;

            elsif X_Bottom > Column_Upper then
               --  Leave through the right boundary while moving down-right.

               X_Bottom := Column_Upper;
               Y_Bottom := Left_At_Column_Upper;
            end if;

            Accumulate
              (Self         => Coverage,
               Column_Upper => Column_Upper,
               X_1          => X_Bottom,
               Y_1          => Y_Bottom,
               X_2          => X_Top,
               Y_2          => Y_Top);

            --  XXX Process remaining segments contributing to this cell.
         end;
         --  if Left_Vertex_Top.Column = Column then
         --     --  Bottom and top verticies shares same pixel.

         --     Accumulate
         --       (Self         => Coverage,
         --        Column_Upper => Column_Upper,
         --        X_1          => Left_Vertex_Bottom.X,
         --        Y_1          => Left_Vertex_Bottom.Y,
         --        X_2          => Left_Vertex_Top.X,
         --        Y_2          => Left_Vertex_Top.Y);

         --     --  XXX Здесь требуется рассмотреть:
         --     --   * следующее ребро слева, уходящее вниз, вниз-влево и,
         --     --     возможно, пересекающее нижюю или правую границу пикселя
         --     --   * первое правое ребро, уходящее вправо или вправо-вниз, и,
         --     --     возможно, пересекающее нижнюю или правую границу
         --     --   * потенциально горизонтальное верхнее ребро, если было
         --     --     выполнено отсечение.

         --  else
         --     --  Bottom vertex of the current left edge belongs to the current
         --     --  (first) row, but columns are different.

         --     --  pragma Assert ();

         --     Accumulate
         --       (Self         => Coverage,
         --        Column_Upper => Column_Upper,
         --        X_1          => Left_Vertex_Bottom.X,
         --        Y_1          => Left_Vertex_Bottom.Y,
         --        X_2          => Column_Upper,
         --        Y_2          => Left_At_Column_Upper);

         --     --  XXX Здесь требуется обработать
         --  end if;

      else
         --  Bottom vertex of the left edge below the upper row boundary.

         declare
            X_Top    : Fixed_16 := Left_Vertex_Top.X;
            Y_Top    : Fixed_16 := Left_Vertex_Top.Y;
            X_Bottom : Fixed_16;
            Y_Bottom : Fixed_16;

         begin
            pragma Assert (Column_Lower <= X_Top);
            --  The first column cannot lie to the right of the top vertex.

            if X_Top > Column_Upper then
               --  Enter through the right boundary while moving down-left.

               X_Top := Column_Upper;
               Y_Top := Left_At_Column_Upper;
            end if;

            if Left_At_Row_Upper < Column_Lower then
               --  Leave through the left boundary.

               X_Bottom := Column_Lower;
               Y_Bottom := Left_At_Column_Lower;

            elsif Left_At_Row_Upper > Column_Upper then
               --  Leave through the right boundary.

               X_Bottom := Column_Upper;
               Y_Bottom := Left_At_Column_Upper;

            else
               --  Leave through the bottom boundary, including either corner.

               X_Bottom := Left_At_Row_Upper;
               Y_Bottom := Row_Upper;
            end if;

            --  Traverse the left boundary upward, to preserve CW order.

            Accumulate
              (Self         => Coverage,
               Column_Upper => Column_Upper,
               X_1          => X_Bottom,
               Y_1          => Y_Bottom,
               X_2          => X_Top,
               Y_2          => Y_Top);
         end;
      --     if Column = Left_Vertex_Top.Column then
      --        --  Top vertex of left egde is in the first column.

      --        if Row_Upper < Left_At_Column_Upper then
      --           --  Cross row upper bound

      --           Accumulate
      --             (Self         => Coverage,
      --              Column_Upper => Column_Upper,
      --              X_1          => Left_At_Row_Upper,
      --              Y_1          => Row_Upper,
      --              X_2          => Left_Vertex_Top.X,
      --              Y_2          => Left_Vertex_Top.Y);

      --           --  XXX обработать верхнее горизонтальное и правое рёбра.

      --        else
      --           --  Cross column upper bound

      --           Accumulate
      --             (Self         => Coverage,
      --              Column_Upper => Column_Upper,
      --              X_1          => Column_Upper,
      --              Y_1          => Left_At_Column_Upper,
      --              X_2          => Left_Vertex_Top.X,
      --              Y_2          => Left_Vertex_Top.Y);

      --           --  XXX Обработать верхее горизонтальное и правое рёбра.
      --        end if;

      --     else
      --        --  Top vertex of the left edge is not in the first column.
      --        --  Edge's line cross column upper bound.

      --        if Row_Upper < Left_At_Column_Lower then
      --           --  Cross row upper bound

      --           Accumulate
      --             (Self         => Coverage,
      --              Column_Upper => Column_Upper,
      --              X_1          => Left_At_Row_Upper,
      --              Y_1          => Row_Upper,
      --              X_2          => Column_Upper,
      --              Y_2          => Left_At_Column_Upper);

      --        else
      --           --  Cross column lower bound

      --           Accumulate
      --             (Self         => Coverage,
      --              Column_Upper => Column_Upper,
      --              X_1          => Column_Lower,
      --              Y_1          => Left_At_Column_Lower,
      --              X_2          => Column_Upper,
      --              Y_2          => Left_At_Column_Upper);
      --        end if;
      --     end if;
      end if;

      Integrate (Accumulator, Coverage, Alpha);

      Fill_Span (Column, Row, 1, Alpha);

      --  Go to next column

      Column := @ + 1;
      Column_Lower := Column_Upper;
      Column_Upper := @ + One;
      Coverage := (Zero, Zero);

      Left_At_Column_Lower := Left_At_Column_Upper;
      Left_At_Column_Upper := @ + Left_Vertex_Top.Step_Y;

      loop
         --  Go through left edge.
         --
         --  XXX Проход по левому ребру здесь требует первого сегмента в
         --  списке, хотя после обработке нескольких сегментов в одном пикселе
         --  для следующей строки может потребоваться соотвествующий,
         --  выходящий через верхнюю границу строки сегмент.

         exit when Left_Column_Last < Column;

         if Column = Left_Column_Last then
            --  Process the last column touched by this segment in the current
            --  row.

            if Left_Vertex_Top.X < Left_Vertex_Bottom.X then
               --  The edge goes down-right. Select the bottom endpoint of the
               --  segment portion inside the current row.

               declare
                  X_Bottom : Fixed_16;
                  Y_Bottom : Fixed_16;

               begin
                  if Left_Vertex_Bottom.Row = Row then
                     X_Bottom := Left_Vertex_Bottom.X;
                     Y_Bottom := Left_Vertex_Bottom.Y;

                  else
                     X_Bottom := Left_At_Row_Upper;
                     Y_Bottom := Row_Upper;
                  end if;

                  --  Traverse upward from the bottom endpoint to the
                  --  intersection with the left cell boundary.

                  Accumulate
                    (Self         => Coverage,
                     Column_Upper => Column_Upper,
                     X_1          => X_Bottom,
                     Y_1          => Y_Bottom,
                     X_2          => Column_Lower,
                     Y_2          => Left_At_Column_Lower);
               end;

            else
               --  The edge goes down-left. Traverse upward from the
               --  intersection with the left cell boundary to the top vertex.

               Accumulate
                 (Self         => Coverage,
                  Column_Upper => Column_Upper,
                  X_1          => Column_Lower,
                  Y_1          => Left_At_Column_Lower,
                  X_2          => Left_Vertex_Top.X,
                  Y_2          => Left_Vertex_Top.Y);
            end if;

            --  XXX Accumulate contributions from the remaining edges.

            --  It is last column of the left edge on current row.

            --  if Left_Vertex_Top.X < Left_Vertex_Bottom.X then
            --     Accumulate
            --       (Self         => Coverage,
            --        Column_Upper => Column_Upper,
            --        X_1          => Left_Vertex_Top.X,
            --        Y_1          => Left_Vertex_Top.X,
            --        X_2          => Column_Lower,
            --        Y_2          => Left_At_Column_Lower);

            --  else
            --     Accumulate
            --       (Self         => Coverage,
            --        Column_Upper => Column_Upper,
            --        X_1          => Column_Lower,
            --        Y_1          => Left_At_Column_Lower,
            --        X_2          => Left_Vertex_Top.X,
            --        Y_2          => Left_Vertex_Top.Y);
            --  end if;

            --  XXX Accumulate top/right edge

         else
            if Left_Vertex_Top.X < Left_Vertex_Bottom.X then
               --  The edge goes down-right. Traverse it right-to-left to
               --  preserve the bottom-to-top direction of the left boundary.

               Accumulate
                 (Self         => Coverage,
                  Column_Upper => Column_Upper,
                  X_1          => Column_Upper,
                  Y_1          => Left_At_Column_Upper,
                  X_2          => Column_Lower,
                  Y_2          => Left_At_Column_Lower);

            else
               --  The edge goes down-left. Traverse it left-to-right to
               --  preserve the bottom-to-top direction of the left boundary.
               --  Vertical edges cannot reach this intermediate-column branch.

               Accumulate
                 (Self         => Coverage,
                  Column_Upper => Column_Upper,
                  X_1          => Column_Lower,
                  Y_1          => Left_At_Column_Lower,
                  X_2          => Column_Upper,
                  Y_2          => Left_At_Column_Upper);
            end if;
            --  Accumulate
            --    (Self         => Coverage,
            --     Column_Upper => Column_Upper,
            --     X_1          => Column_Lower,
            --     Y_1          => Left_At_Column_Lower,
            --     X_2          => Column_Upper,
            --     Y_2          => Left_At_Column_Upper);
         end if;

         Integrate (Accumulator, Coverage, Alpha);

         Fill_Span (Column, Row, 1, Alpha);

         Column := @ + 1;
         Column_Lower := Column_Upper;
         Column_Upper := @ + One;
         Coverage := (Zero, Zero);

         Left_At_Column_Lower := Left_At_Column_Upper;
         Left_At_Column_Upper := @ + Left_Vertex_Top.Step_Y;
      end loop;

      --  loop
      --     null;
      --  end loop;

      --  if Top >= Bottom then
      --     return;
      --  end if;
      --  Select_Segment (Left_Side, Left, Left_Last);
      --  Select_Segment (Right_Side, Right, Right_Last);
      --  for Row in
      --    Integral (Top) .. Integral (Bottom - Fixed_16_Delta_Fixed)
      --  loop
      --     declare
      --        Y : constant Fixed_16 :=
      --          Min (Bottom, Pixel_Lower_Bound (Row) + One);
      --        Height : constant Fixed_16 := Y - Left_Side.Current.Y;
      --        Left_Row, Right_Row : Row_Side;
      --        First, Last, Interior_First, Interior_End : Device_Pixel_Index;
      --        Column : Device_Pixel_Index;
      --        Cover : Fixed_16 := Zero;
      --     begin
      --        Prepare_Row (Left_Side, Left, Left_Last, Y, Left_Row);
      --        Prepare_Row (Right_Side, Right, Right_Last, Y, Right_Row);
      --        First := Integral (Min (Left_Row.Minimum, Right_Row.Minimum));
      --        Last := Integral
      --          (Max (Left_Row.Maximum, Right_Row.Maximum)
      --             - Fixed_16_Delta_Fixed);
      --        Interior_First := Integral (Left_Row.Maximum)
      --          + (if Fractional (Left_Row.Maximum) = Zero then 0 else 1);
      --        Interior_End := Integral (Right_Row.Minimum);
      --        Column := First;
      --        while Column <= Last loop
      --           if Column = Interior_First and then Column < Interior_End then
      --              --  Both boundaries stay outside these whole cells for
      --              --  the entire occupied height of this row.
      --              Cover := Height;
      --              Emit (Column, Row, Interior_End - Column, Height);
      --              Column := Interior_End;
      --           else
      --              declare
      --                 Cell_Cover, Cell_Area : Fixed_16 := Zero;
      --                 Pixel_Left : constant Fixed_16 :=
      --                   Pixel_Lower_Bound (Column);
      --              begin
      --                 Accumulate_Side
      --                   (Left_Row, Pixel_Left, False,
      --                    Cell_Cover, Cell_Area);
      --                 Accumulate_Side
      --                   (Right_Row, Pixel_Left, True,
      --                    Cell_Cover, Cell_Area);
      --                 Cover := @ + Cell_Cover;
      --                 Emit (Column, Row, 1,
      --                       Cover - Cell_Area / GX_Integer'(2));
      --              end;
      --              Column := @ + 1;
      --           end if;
      --        end loop;
      --        Flush (Row);
      --     end;
      --  end loop;
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

         --  Adjusted endpoints can coincide with their neighbours.

         if First < Last and then Chain (First) = Chain (First + 1) then
            First := @ + 1;
         end if;

         if First < Last and then Chain (Last) = Chain (Last - 1) then
            Last := @ - 1;
         end if;
      end Adjust_Y;

   begin
      --  if Empty_Area or else Width <= 0.0
      --    or else (Point_A.X = Point_B.X and then Point_A.Y = Point_B.Y)
      --  then
      --     return;
      --  end if;

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
         if Y_Bottom < Y_Top then
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
