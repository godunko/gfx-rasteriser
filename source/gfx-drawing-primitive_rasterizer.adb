--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

pragma Ada_2022;

with GFX.Implementation.Fixed_Types;
with GFX.Vectors;

package body GFX.Drawing.Primitive_Rasterizer is

   use Interfaces;
   use GFX.Implementation.Fixed_Types;
   use GFX.Points;
   use GFX.Vectors;

   -----------------------------
   -- Internal_Fill_Rectangle --
   -----------------------------

   procedure Internal_Fill_Rectangle
     (Top       : GFX.Drawing.Device_Pixel_Coordinate;
      Left      : GFX.Drawing.Device_Pixel_Coordinate;
      Right     : GFX.Drawing.Device_Pixel_Coordinate;
      Bottom    : GFX.Drawing.Device_Pixel_Coordinate;
      Fill_Span : not null access procedure
        (X        : GFX.Drawing.Device_Pixel_Index;
         Y        : GFX.Drawing.Device_Pixel_Index;
         Width    : GFX.Drawing.Device_Pixel_Count;
         Coverage : GFX.Drawing.Grayscale))
   is
      T  : constant Fixed_16   := To_Fixed_16 (Top);
      TI : constant GX_Integer := Integral (T);
      TC : constant Fixed_16   := Right_Coverage (T);

      L  : constant Fixed_16   := To_Fixed_16 (Left);
      LI : constant GX_Integer := Integral (L);
      LC : constant Fixed_16   := Right_Coverage (L);

      R  : constant Fixed_16   := To_Fixed_16 (Right);
      RI : constant GX_Integer := Integral (R);
      RC : constant Fixed_16   := Left_Coverage (R);

      B  : constant Fixed_16   := To_Fixed_16 (Bottom);
      BI : constant GX_Integer := Integral (B);
      BC : constant Fixed_16   := Left_Coverage (B);

      procedure Fill_1
        (HW : GFX.Drawing.Device_Pixel_Count;
         HC : Fixed_16);
      --  Fill each line by one span

      procedure Fill_2
        (LI : GFX.Drawing.Device_Pixel_Index;
         LW : GFX.Drawing.Device_Pixel_Count;
         LC : Fixed_16;
         RI : GFX.Drawing.Device_Pixel_Index;
         RW : GFX.Drawing.Device_Pixel_Count;
         RC : Fixed_16);
      --  Fill each line by two spans

      procedure Fill_3;
      --  Fill each line by three spans

      ------------
      -- Fill_1 --
      ------------

      procedure Fill_1
        (HW : GFX.Drawing.Device_Pixel_Count;
         HC : Fixed_16)
      is
         HG : constant GFX.Drawing.Grayscale := To_Grayscale (HC);
         AC : Fixed_16;
         Y  : GX_Integer := TI + 1;

      begin
         if TI = BI then
            --  Fill single rasterline

            AC := Multiply_Coverage (HC, (TC + BC) - One);
            Fill_Span (LI, TI, HW, To_Grayscale (AC));

         else
            --  Fill top rasterline

            AC := Multiply_Coverage (HC, TC);
            Fill_Span (LI, TI, HW, To_Grayscale (AC));

            --  Fill intermediate rasterlines

            while Y < BI loop
               Fill_Span (LI, Y, HW, HG);
               Y := @ + 1;
            end loop;

            --  Fill bottom rasterline

            AC := Multiply_Coverage (HC, BC);
            Fill_Span (LI, BI, HW, To_Grayscale (AC));
         end if;
      end Fill_1;

      ------------
      -- Fill_2 --
      ------------

      procedure Fill_2
        (LI : GFX.Drawing.Device_Pixel_Index;
         LW : GFX.Drawing.Device_Pixel_Count;
         LC : Fixed_16;
         RI : GFX.Drawing.Device_Pixel_Index;
         RW : GFX.Drawing.Device_Pixel_Count;
         RC : Fixed_16)
      is
         HC : constant Fixed_16 := (TC + BC) - One;
         LG : constant GFX.Drawing.Grayscale := To_Grayscale (LC);
         RG : constant GFX.Drawing.Grayscale := To_Grayscale (RC);

         Y  : GFX.Drawing.Device_Pixel_Index := TI + 1;
         SG : GFX.Drawing.Grayscale;
         EG : GFX.Drawing.Grayscale;

      begin
         if TI = BI then
            --  Fill single rasterline

            SG := To_Grayscale (Multiply_Coverage (LC, HC));
            EG := To_Grayscale (Multiply_Coverage (RC, HC));

            Fill_Span (LI, TI, LW, SG);
            Fill_Span (RI, TI, RW, EG);

         else
            --  Fill top rasterline

            SG := To_Grayscale (Multiply_Coverage (LC, TC));
            EG := To_Grayscale (Multiply_Coverage (RC, TC));

            Fill_Span (LI, TI, LW, SG);
            Fill_Span (RI, TI, RW, EG);

            --  Fill intermediate rasterlines

            while Y < BI loop
               Fill_Span (LI, Y, LW, LG);
               Fill_Span (RI, Y, RW, RG);
               Y := @ + 1;
            end loop;

            --  Fill bottom rasterline

            SG := To_Grayscale (Multiply_Coverage (LC, BC));
            EG := To_Grayscale (Multiply_Coverage (RC, BC));

            Fill_Span (LI, BI, LW, SG);
            Fill_Span (RI, BI, RW, EG);
         end if;
      end Fill_2;

      ------------
      -- Fill_3 --
      ------------

      procedure Fill_3 is
         HL : constant GFX.Drawing.Device_Pixel_Count := RI - LI + 1 - 2;
         LG : constant GFX.Drawing.Grayscale          := To_Grayscale (LC);
         CG : constant GFX.Drawing.Grayscale          := To_Grayscale (One);
         RG : constant GFX.Drawing.Grayscale          := To_Grayscale (RC);

         HC : Fixed_16;
         Y  : GFX.Drawing.Device_Pixel_Count := TI + 1;
         SG : GFX.Drawing.Grayscale;
         MG : GFX.Drawing.Grayscale;
         EG : GFX.Drawing.Grayscale;

      begin
         if TI = BI then
            --  Fill single rasterline

            HC := (TC + BC) - One;
            SG := To_Grayscale (Multiply_Coverage (LC, HC));
            MG := To_Grayscale (Multiply_Coverage (One, HC));
            EG := To_Grayscale (Multiply_Coverage (RC, HC));

            Fill_Span (LI, TI, 1, SG);
            Fill_Span (LI + 1, TI, HL, MG);
            Fill_Span (RI, TI, 1, EG);

         else
            --  Fill top rasterline

            SG := To_Grayscale (Multiply_Coverage (LC, TC));
            MG := To_Grayscale (Multiply_Coverage (One, TC));
            EG := To_Grayscale (Multiply_Coverage (RC, TC));

            Fill_Span (LI, TI, 1, SG);
            Fill_Span (LI + 1, TI, HL, MG);
            Fill_Span (RI, TI, 1, EG);

            --  Fill intermediate rasterlines

            while Y < BI loop
               Fill_Span (LI, Y, 1, LG);
               Fill_Span (LI + 1, Y, HL, CG);
               Fill_Span (RI, Y, 1, RG);
               Y := @ + 1;
            end loop;

            --  Fill bottom rasterline

            SG := To_Grayscale (Multiply_Coverage (LC, BC));
            MG := To_Grayscale (Multiply_Coverage (One, BC));
            EG := To_Grayscale (Multiply_Coverage (RC, BC));

            Fill_Span (LI, BI, 1, SG);
            Fill_Span (LI + 1, BI, HL, MG);
            Fill_Span (RI, BI, 1, EG);
         end if;
      end Fill_3;

   begin
      if LI = RI then
         --  Left and right sides inside the same pixel

         Fill_1 (1, (LC + RC) - One);

      else
         if LC = One then
            --  Full coverage of the left pixel:
            --   - one span when full coverange of the right pixel
            --   - two spans when partial coverage of the right pixel

            if RC = One then
               Fill_1 (RI - LI + 1, One);

            else
               Fill_2 (LI, RI - LI + 1 - 1, One, RI, 1, RC);
            end if;

         elsif LI + 1 /= RI then
            --  Partial coverage of the left pixel, there are medium
            --  pixels:
            --   - two spans when full coverage of the right pixel
            --   - three spans when partial coverage of the right pixel

            if RC = One then
               Fill_2 (LI, 1, LC, LI + 1, RI - LI + 1 - 1, RC);

            else
               Fill_3;
            end if;

         else
            --  Partial coverage of the left pixel, right pixel is next to
            --  left pixel:
            --   - one span when coverage of the left and right pixels are
            --     equal
            --   - two spans otherwise

            if LC = RC then
               Fill_1 (2, LC);

            else
               Fill_2 (LI, 1, LC, RI, 1, RC);
            end if;
         end if;
      end if;
   end Internal_Fill_Rectangle;

   --------------------
   -- Rasterize_Line --
   --------------------

   procedure Rasterize_Line
     (Point_A   : GFX.Points.GF_Point;
      Point_B   : GFX.Points.GF_Point;
      Width     : GFX.GX_Real;
      Fill_Span : not null access procedure
        (X        : GFX.Drawing.Device_Pixel_Index;
         Y        : GFX.Drawing.Device_Pixel_Index;
         Width    : GFX.Drawing.Device_Pixel_Count;
         Coverage : GFX.Drawing.Grayscale))
      --  Draw_Span : not null Draw_Span_Subprogram)
   is
      pragma Suppress (All_Checks);

      PA : GF_Point := Point_A;
      PB : GF_Point := Point_B;
      W  : GX_Real  := Width;

   begin
      if Is_Equal_Fixed_6 (PA.Y, PB.Y) then
         --  Line is horizontal, convert it to vertical.

         declare
            X  : constant GX_Real := (PA.X + PB.X) / 2.0;
            Y  : constant GX_Real := PA.Y;
            DY : constant GX_Real := W / 2.0;
            WX : constant GX_Real := abs (PA.X - PB.X);

         begin
            W  := WX;
            PA := (X, Y - DY);
            PB := (X, Y + DY);
         end;
      end if;

      if PA.Y > PB.Y then
         declare
            Aux : GFX.Points.GF_Point;

         begin
            Aux := PA;
            PA  := PB;
            PB  := Aux;
         end;
      end if;

      if Is_Equal_Fixed_6 (PA.X, PB.X) then
         --  Line is vertical (or horizontal converted to vertical), draw
         --  rectangle.

         declare
            HW : constant GX_Real := W / 2.0;

         begin
            Internal_Fill_Rectangle
              (Top       => PA.Y,
               Left      => PA.X - HW,
               Right     => PA.X + HW - Fixed_16_Delta,
               Bottom    => PB.Y,
               Fill_Span => Fill_Span);
            --  Fill rectangle is smaller by Fixed_16_Epsilon at right side
            --  to prevent fill of the "empty" pixel due to use of the "round
            --  to nearest" rounding mode for floating point to fixed point
            --  conversion.
         end;

         return;
      end if;

      declare
         V  : constant GF_Vector := PB - PA;
         N2 : constant GF_Vector := Normalize ((V.Y, -V.X)) * (Width / 2.0);
         --  Normal vector of the line with half width length.

         Top    : GF_Point;
         Left   : GF_Point;
         Right  : GF_Point;
         Bottom : GF_Point;

         Top_X    : Fixed_16  with Volatile;
         Top_Y    : Fixed_16 with Volatile;
         Left_X   : Fixed_16 with Volatile;
         Left_Y   : Fixed_16 with Volatile;
         Right_X  : Fixed_16 with Volatile;
         Right_Y  : Fixed_16 with Volatile;
         Bottom_X : Fixed_16 with Volatile;
         Bottom_Y : Fixed_16 with Volatile;

         Top_Left_Slope     : Fixed_16 with Volatile;
         Top_Right_Slope    : Fixed_16 with Volatile;
         Bottom_Left_Slope  : Fixed_16 with Volatile;
         Bottom_Right_Slope : Fixed_16 with Volatile;

         Left_Up    : Fixed_16 with Volatile;
         Right_Up   : Fixed_16 with Volatile;
         Left_Down  : Fixed_16 with Volatile;
         Right_Down : Fixed_16 with Volatile;

         L : Fixed_16;
         R : Fixed_16;
         DL : Fixed_16;
         DR : Fixed_16;

      begin
         --  Compute vertcies of the rectangle to be rasterized.

         if PA.X < PB.X then
            Top    := PA + N2;
            Left   := PA - N2;
            Right  := PB + N2;
            Bottom := PB - N2;

         else
            Top    := PA - N2;
            Left   := PB - N2;
            Right  := PA + N2;
            Bottom := PB + N2;
         end if;

         --  Snap verticies to the subpixel grid and convert them to Fixed_16
         --  representation.

         Top_X    := To_Fixed_16 (To_Fixed_6 (Top.X));
         Top_Y    := To_Fixed_16 (To_Fixed_6 (Top.Y));
         Left_X   := To_Fixed_16 (To_Fixed_6 (Left.X));
         Left_Y   := To_Fixed_16 (To_Fixed_6 (Left.Y));
         Right_X  := To_Fixed_16 (To_Fixed_6 (Right.X));
         Right_Y  := To_Fixed_16 (To_Fixed_6 (Right.Y));
         Bottom_X := To_Fixed_16 (To_Fixed_6 (Bottom.X));
         Bottom_Y := To_Fixed_16 (To_Fixed_6 (Bottom.Y));

         --  Compute slope of the lines of edges of the rectangle.

         Top_Left_Slope     :=
           Divide_Saturated (Left_X - Top_X, Left_Y - Top_Y);
         Top_Right_Slope    :=
           Divide_Saturated (Right_X - Top_X, Right_Y - Top_Y);
         Bottom_Left_Slope  :=
           Divide_Saturated (Left_X - Bottom_X, Left_Y - Bottom_Y);
         Bottom_Right_Slope :=
           Divide_Saturated (Right_X - Bottom_X, Right_Y - Bottom_Y);

         --  Select slopes of the line at left and line at right sides.

         DL := Top_Left_Slope;
         DR := Top_Right_Slope;

         --  Compute intersection of the current left and right lines with up
         --  and down edges of the device pixel.

         Left_Up := Top_X - (Fractional (Top_Y)) * Top_Left_Slope;
         Right_Up := Top_X - (Fractional (Top_Y)) * Top_Right_Slope;
         Left_Down := Top_X + (One - Fractional (Top_Y)) * Top_Left_Slope;
         Right_Down := Top_X + (One - Fractional (Top_Y)) * Top_Right_Slope;

         for Y in Integral (Top_Y) .. Integral (Bottom_Y + One) loop
            --  Limit horizontal span by the line's left and right edges
            --  intersection with pixel's top and bottom edges, and clip by
            --  horizontal position of the left and right vertices.

            L := Max (Left_X, Min (Left_Up, Left_Down));
            R := Min (Right_X, Max (Right_Up, Right_Down));

            Fill_Span (Integral (L), Y, Integral (R) - Integral (L) + 1, 255);

            Left_Up  := Left_Down;
            Right_Up := Right_Down;

            if Y = Integral (Left_Y) then
               --  Pixel of the left vertex has bean reached, switch direction
               --  of the left line to the bottom vertex.

               DL        := Bottom_Left_Slope;
               Left_Down :=
                 Left_X - (One - Fractional (Left_Y)) * Bottom_Left_Slope;
            end if;

            if Y = Integral (Right_Y) then
               --  Pixel of the right vertex has bean reached, switch direction
               --  of the left line to the bottom vertex.

               DR         := Bottom_Right_Slope;
               Right_Down :=
                 Right_X - (One - Fractional (Right_Y)) * Bottom_Right_Slope;
            end if;

            Left_Down  := @ + DL;
            Right_Down := @ + DR;
         end loop;
      end;
   end Rasterize_Line;

end GFX.Drawing.Primitive_Rasterizer;
