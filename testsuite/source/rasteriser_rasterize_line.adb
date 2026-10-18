--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Text_IO;
with GFX.Implementation.Fixed_Types;
with GFX.Points;
with GFX.Rasteriser.Primitives;
with GFX.Vectors;

procedure Rasteriser_Rasterize_Line is
   use GFX;
   use GFX.Rasteriser;
   use type Grayscale;
   use type GFX.Points.GF_Point;
   use type GFX.Vectors.GF_Vector;

   type Pixel_Buffer is array (Device_Pixel_Index range 0 .. 20,
                               Device_Pixel_Index range 0 .. 20) of Grayscale;
   Pixels : Pixel_Buffer;
   Last_X, Last_Y : Device_Pixel_Index;
   Calls : Natural;
   Limit : Device_Pixel_Index := 21;
   Sample_X, Sample_Y : Device_Pixel_Index := 0;
   Sample : Grayscale := 0;
   Largest_Span : Device_Pixel_Count := 0;

   type Point is record
      X, Y : Long_Float;
   end record;
   type Polygon is array (Positive range 1 .. 16) of Point;
   Shape : Polygon;
   Shape_Count : Natural;

   procedure Fill_Span
     (X, Y : Device_Pixel_Index;
      Width : Device_Pixel_Count;
      Coverage : Grayscale);
   procedure Clip (P : in out Polygon; Count : in out Natural;
                   Axis_X : Boolean; Bound : Long_Float; Lower : Boolean);
   procedure Prepare (A, B : GFX.Points.GF_Point; Width : GX_Real);
   function Reference (X, Y : Device_Pixel_Index) return Integer;
   procedure Draw (A, B : GFX.Points.GF_Point; Width : GX_Real);
   procedure Check
     (A, B      : GFX.Points.GF_Point;
      Width     : GX_Real;
      Max_Error : Natural := 1);

   procedure Fill_Span
     (X, Y : Device_Pixel_Index;
      Width : Device_Pixel_Count;
      Coverage : Grayscale) is
   begin
      if Width = 0 or else Coverage = 0
        or else X < 0 or else X + Width > Limit
        or else Y < 0 or else Y >= Limit or else Y < Last_Y
        or else (Y = Last_Y and then X <= Last_X)
      then
         raise Program_Error with "Invalid, overlapping or unordered span";
      end if;
      Calls := @ + 1;
      Largest_Span := Device_Pixel_Count'Max (Largest_Span, Width);
      Last_Y := Y;
      Last_X := X + Width - 1;
      if Y <= 20 then
         for Column in X .. Device_Pixel_Index'Min (X + Width - 1, 20) loop
            Pixels (Column, Y) := Coverage;
         end loop;
      end if;
      if Y = Sample_Y and then Sample_X in X .. X + Width - 1 then
         Sample := Coverage;
      end if;
   end Fill_Span;

   procedure Clip (P : in out Polygon; Count : in out Natural;
                   Axis_X : Boolean; Bound : Long_Float; Lower : Boolean)
   is
      Result : Polygon;
      N : Natural := 0;
      Previous : Point;
      Previous_Distance, Distance, Ratio : Long_Float;
      function Measure (V : Point) return Long_Float is
        ((if Lower then 1.0 else -1.0)
         * ((if Axis_X then V.X else V.Y) - Bound));
   begin
      if Count = 0 then
         return;
      end if;
      Previous := P (Count);
      Previous_Distance := Measure (Previous);
      for Current of P (1 .. Count) loop
         Distance := Measure (Current);
         if (Distance >= 0.0) /= (Previous_Distance >= 0.0) then
            Ratio := Previous_Distance / (Previous_Distance - Distance);
            N := @ + 1;
            Result (N) :=
              (Previous.X + Ratio * (Current.X - Previous.X),
               Previous.Y + Ratio * (Current.Y - Previous.Y));
            if Axis_X then
               Result (N).X := Bound;
            else
               Result (N).Y := Bound;
            end if;
         end if;
         if Distance >= 0.0 then
            N := @ + 1;
            Result (N) := Current;
         end if;
         Previous := Current;
         Previous_Distance := Distance;
      end loop;
      Count := N;
      P (1 .. N) := Result (1 .. N);
   end Clip;

   procedure Prepare (A, B : GFX.Points.GF_Point; Width : GX_Real) is
      Top : constant GFX.Points.GF_Point := (if A.Y < B.Y then A else B);
      Bottom : constant GFX.Points.GF_Point := (if A.Y < B.Y then B else A);
      V : constant GFX.Vectors.GF_Vector := Bottom - Top;
      N : constant GFX.Vectors.GF_Vector :=
        GFX.Vectors.Normalize ((V.Y, -V.X)) * (Width / 2.0);
      Corners : constant array (1 .. 4) of GFX.Points.GF_Point :=
        (if V.X > 0.0
         then [Top + N, Bottom + N, Bottom - N, Top - N]
         else [Top - N, Top + N, Bottom + N, Bottom - N]);
      Inside : Boolean := True;
      Snapped : Polygon;
   begin
      Shape_Count := 4;
      for J in Corners'Range loop
         Shape (J) := (Long_Float (Corners (J).X),
                       Long_Float (Corners (J).Y));
         Inside := Inside
           and Shape (J).X >= 0.0 and Shape (J).X <= Long_Float (Limit)
           and Shape (J).Y >= 0.0 and Shape (J).Y <= Long_Float (Limit);
         Snapped (J) :=
           (Long_Float'Floor (Shape (J).X * 64.0) / 64.0,
            Long_Float'Floor (Shape (J).Y * 64.0) / 64.0);
      end loop;

      --  Unclipped rectangles preserve parallel sides on the input grid.
      --  The bottom corner is reconstructed from the other three corners.
      Snapped (3) :=
        (Snapped (2).X + Snapped (4).X - Snapped (1).X,
         Snapped (2).Y + Snapped (4).Y - Snapped (1).Y);
      if Inside and Snapped (3).Y <= Long_Float (Limit) then
         Shape (1 .. 4) := Snapped (1 .. 4);
         return;
      end if;

      --  Clipped contours snap every vertex independently.
      Clip (Shape, Shape_Count, True, 0.0, True);
      Clip (Shape, Shape_Count, True, Long_Float (Limit), False);
      Clip (Shape, Shape_Count, False, 0.0, True);
      Clip (Shape, Shape_Count, False, Long_Float (Limit), False);
      for J in 1 .. Shape_Count loop
         Shape (J).X := Long_Float'Floor (Shape (J).X * 64.0) / 64.0;
         Shape (J).Y := Long_Float'Floor (Shape (J).Y * 64.0) / 64.0;
      end loop;
   end Prepare;

   function Reference (X, Y : Device_Pixel_Index) return Integer is
      P : Polygon := Shape;
      Count : Natural := Shape_Count;
      Twice_Area : Long_Float := 0.0;

      --  Polygon clipping can place exact grid intersections just below
      --  the grid due to floating point roundoff. The bias is in subpixels.
      function Subpixel_Floor (Value : Long_Float) return Long_Float is
        (Long_Float'Floor (Value * 64.0 + 1.0E-8) / 64.0);
   begin
      Clip (P, Count, True, Long_Float (X), True);
      Clip (P, Count, True, Long_Float (X) + 1.0, False);
      Clip (P, Count, False, Long_Float (Y), True);
      Clip (P, Count, False, Long_Float (Y) + 1.0, False);
      for J in 1 .. Count loop
         declare
            Next : constant Positive := J mod Count + 1;
            Height : constant Long_Float :=
              Subpixel_Floor (P (Next).Y - Long_Float (Y))
              - Subpixel_Floor (P (J).Y - Long_Float (Y));
            Width : constant Long_Float := Subpixel_Floor
              ((Long_Float (X) + 1.0 - P (J).X)
               + (Long_Float (X) + 1.0 - P (Next).X));
         begin
            --  Integrate the independently clipped polygon with the
            --  specified 1/64 precision for heights and summed widths.
            Twice_Area := @ + Height * Width;
         end;
      end loop;
      --  Division by two truncates to Fixed_12 before grayscale conversion.
      return Integer (Long_Float'Floor
        (Long_Float'Min
           (1.0, Long_Float'Floor (abs Twice_Area * 2048.0) / 4096.0)
         * 255.0 + 1.0E-8));
   end Reference;

   procedure Draw (A, B : GFX.Points.GF_Point; Width : GX_Real) is
   begin
      Pixels := [others => [others => 0]];
      Calls := 0;
      Last_X := -1;
      Last_Y := -1;
      Sample := 0;
      Largest_Span := 0;
      GFX.Rasteriser.Primitives.Rasterize_Line (A, B, Width, Fill_Span'Access);
   end Draw;

   --  Allow one grayscale level for Fixed_16 intersection truncation.
   --  Coverage quantization is included in the polygon reference.
   procedure Check
     (A, B      : GFX.Points.GF_Point;
      Width     : GX_Real;
      Max_Error : Natural := 1) is
      Forward : Pixel_Buffer;
   begin
      Prepare (A, B, Width);
      Draw (A, B, Width);
      for X in Pixels'Range (1) loop
         for Y in Pixels'Range (2) loop
            if abs (Integer (Pixels (X, Y)) - Reference (X, Y)) > Max_Error
            then
               Ada.Text_IO.Put_Line
                 ("A=" & A.X'Image & A.Y'Image
                  & " B=" & B.X'Image & B.Y'Image & " W=" & Width'Image);
               for K in 1 .. Shape_Count loop
                  Ada.Text_IO.Put_Line ("P" & Shape (K).X'Image
                    & Shape (K).Y'Image);
               end loop;
               raise Program_Error with
                 "Coverage at" & X'Image & "," & Y'Image
                 & " actual" & Pixels (X, Y)'Image
                 & " expected" & Reference (X, Y)'Image;
            end if;
         end loop;
      end loop;
      Forward := Pixels;
      Draw (A => B, B => A, Width => Width);
      if Pixels /= Forward then
         raise Program_Error with "Coverage depends on endpoint order";
      end if;
   end Check;

begin
   GFX.Rasteriser.Primitives.Set_Rendering_Area (0, 0, 0, Limit);
   Draw ((5.0, 5.0), (15.0, 5.0), 1.0);
   if Calls /= 0 then
      raise Program_Error with "Empty rendering area produced spans";
   end if;
   GFX.Rasteriser.Primitives.Set_Rendering_Area (0, 0, Limit, Limit);
   Check ((5.0, 5.0), (10.0, 15.0), 2.0);
   Check ((15.0, 5.0), (5.0, 15.0), 2.0);
   Check ((5.0, 5.0), (15.0, 15.0), 0.25);
   Check ((5.0, 5.0), (5.25, 5.25), 2.0);
   Check ((5.0, 5.0), (5.125, 5.125), 0.25);
   --  Both sides change segments inside one row and one pixel. The two
   --  slopes also exercise opposite X directions around side vertices.
   Check ((5.5, 5.5), (5.625, 5.625), 0.25);
   Check ((5.5, 5.625), (5.625, 5.5), 0.25);
   Check ((5.25, 5.5), (5.875, 5.5), 0.375);
   Check ((5.375, 5.625), (5.625, 5.375), 2.0);
   --  A side vertex crosses a row boundary while its neighbour remains
   --  inside that row, including coincident left/right boundary cells.
   Check ((5.125, 5.875), (5.375, 6.125), 0.5);
   Check ((5.125, 6.125), (5.375, 5.875), 0.5);
   Check ((5.0, 5.0), (15.0, 5.0), 1.0);
   Check ((5.0, 5.0), (5.0, 15.0), 1.0);
   Check ((5.5, 5.5), (15.5, 5.5), 1.0);
   --  Snapping can collapse the short edges of the parallelogram.
   --  Both horizontal directions exercise the ends of both chains.
   for Direction in -1 .. 1 loop
      if Direction /= 0 then
         declare
            X_B : constant GX_Real := 10.0 + GX_Real (Direction) * 0.01;
         begin
            Check ((10.0, 10.255), (X_B, 12.265625), 1.0);
            Check ((10.0, 10.25), (X_B, 12.255), 1.0);
            Check ((10.0, 10.255), (X_B, 12.255), 1.0);
         end;
      end if;
   end loop;
   Check ((5.0, 5.0), (15.0, 5.015625), 0.25);
   Check ((5.0, 5.0), (5.015625, 15.0), 0.25);
   Check ((-3.0, 5.0), (3.0, 5.0), 2.0);
   Check ((5.0, -3.0), (5.0, 3.0), 2.0);
   Check ((18.0, 5.0), (25.0, 5.0), 2.0);
   Check ((5.0, 18.0), (5.0, 25.0), 2.0);
   Check ((-5.0, -5.0), (25.0, 25.0), 4.0);
   Check ((-5.0, 25.0), (25.0, -5.0), 4.0);
   Check ((-10.0, 10.5), (30.0, 10.5), 50.0);
   if Largest_Span < 20 then
      raise Program_Error with "Interior was not emitted as a span";
   end if;
   --  The interior span uses occupied row height, including both partial
   --  rows of a strip whose sides lie exactly on the window boundaries.
   Check ((-5.0, 5.25), (25.0, 5.25), 0.5);
   if Calls > 2 or else Largest_Span < 20
     or else Pixels (10, 5) /= 127
   then
      raise Program_Error with "Partial first row lost its interior span";
   end if;
   Check ((-5.0, 5.75), (25.0, 5.75), 0.5);
   if Calls > 2 or else Largest_Span < 20
     or else Pixels (10, 5) /= 127
   then
      raise Program_Error with "Partial last row lost its interior span";
   end if;
   Check ((-5.0, 5.25), (25.0, 5.25), 1.0);
   if Calls > 4 or else Largest_Span < 20
     or else Pixels (10, 4) /= 63 or else Pixels (10, 5) /= 191
   then
      raise Program_Error with "Partial row heights were lost";
   end if;
   Check ((-5.0, 0.0), (25.0, 0.0), 2.0);
   Check ((5.0, 22.0), (10.0, 25.0), 2.0);
   Check ((5.0, 5.0), (15.0, 5.0), 0.001);
   for W in 1 .. 3 loop
      for J in 0 .. 30 loop
         Check ((GX_Real (J) - 5.125, 3.375),
                (16.625, GX_Real (J) - 4.875), GX_Real (W) / 4.0);
      end loop;
   end loop;
   Draw ((5.0, 5.0), (15.0, 10.0), 0.0);
   if Calls /= 0 then
      raise Program_Error with "Zero width produced spans";
   end if;
   Draw ((5.0, 5.0), (15.0, 10.0), -1.0);
   if Calls /= 0 then
      raise Program_Error with "Negative width produced spans";
   end if;
   Draw ((5.0, 5.0), (5.0, 5.0), 2.0);
   if Calls /= 0 then
      raise Program_Error with "Zero length produced spans";
   end if;

   declare
      use GFX.Implementation.Fixed_Types;
   begin
      if Divide_Saturated (One, Zero) /= Fixed_16_Last
        or else Divide_Saturated (-One, Zero) /= Fixed_16_First
        or else Divide_Saturated (One, Fixed_16_Delta_Fixed) /= Fixed_16_Last
      then
         raise Program_Error with "Unexpected saturated division";
      end if;
   end;

   --  Large original coordinates exercise safe differences and clipping.
   Check ((-16_380.0, 10.5), (16_380.0, 10.5), 1.0);
   Check ((10.5, -16_380.0), (10.5, 16_380.0), 1.0);

   Limit := 16_383;
   GFX.Rasteriser.Primitives.Set_Rendering_Area (0, 0, Limit, Limit);
   for Direction in 1 .. 2 loop
      declare
         A : constant GFX.Points.GF_Point :=
           (if Direction = 1 then (5.5, 0.5) else (0.5, 5.5));
         B : constant GFX.Points.GF_Point :=
           (if Direction = 1 then (105.5, 16_380.5)
            else (16_380.5, 105.5));
         Max_Error : Integer := 0;
      begin
         Prepare (A, B, 1.0);
         for J in 0 .. 3 loop
            for Offset in -1 .. 1 loop
               if Direction = 1 then
                  Sample_Y := Device_Pixel_Index (J) * 4_000;
                  Sample_X := Device_Pixel_Index
                    (Long_Float'Floor (5.5 + 100.0
                       * (Long_Float (Sample_Y) / 16_380.0)))
                    + Device_Pixel_Index (Offset);
               else
                  Sample_X := Device_Pixel_Index (J) * 4_000;
                  Sample_Y := Device_Pixel_Index
                    (Long_Float'Floor (5.5 + 100.0
                       * (Long_Float (Sample_X) / 16_380.0)))
                    + Device_Pixel_Index (Offset);
               end if;
               Draw (A, B, 1.0);
               Max_Error := Integer'Max
                 (Max_Error, abs (Integer (Sample)
                                 - Reference (Sample_X, Sample_Y)));
            end loop;
         end loop;
         if Max_Error > 1 then
            raise Program_Error with "Long-edge coverage exceeded its bound";
         end if;
         Ada.Text_IO.Put_Line
           ("Long-edge direction" & Direction'Image
            & " sampled coverage error:" & Max_Error'Image);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Rasteriser rasterize line checks passed");
end Rasteriser_Rasterize_Line;
