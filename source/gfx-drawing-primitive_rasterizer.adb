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

         Top_Vertex           : GF_Point;
         Left_Vertex          : GF_Point;
         Right_Vertex         : GF_Point;
         Bottom_Vertex        : GF_Point;
         --  Verticies of the drawn rectangle

         Top_Vertex_X         : Fixed_16 with Volatile;
         Top_Vertex_Y         : Fixed_16 with Volatile;
         Left_Vertex_X        : Fixed_16 with Volatile;
         Left_Vertex_Y        : Fixed_16 with Volatile;
         Right_Vertex_X       : Fixed_16 with Volatile;
         Right_Vertex_Y       : Fixed_16 with Volatile;
         Bottom_Vertex_X      : Fixed_16 with Volatile;
         Bottom_Vertex_Y      : Fixed_16 with Volatile;
         --  Coordinates of the drawn rectangle's verticies in fixed point.

         Top_Left_Slope_X     : Fixed_16 with Volatile;
         Top_Right_Slope_X    : Fixed_16 with Volatile;
         Bottom_Left_Slope_X  : Fixed_16 with Volatile;
         Bottom_Right_Slope_X : Fixed_16 with Volatile;
         --  Slopes of the edge lines of the drawn rectangle in fixed point.

         Left_Edge_Row_Up     : Fixed_16 with Volatile;
         Right_Edge_Row_Up    : Fixed_16 with Volatile;
         Left_Edge_Row_Down   : Fixed_16 with Volatile;
         Right_Edge_Row_Down  : Fixed_16 with Volatile;
         --  X coordinate of the intersection of the edge line of the draw
         --  rectangle with the current rasterline.

         LS : Fixed_16 with Volatile;
         LE : Fixed_16 with Volatile;
         RS : Fixed_16 with Volatile;
         RE : Fixed_16 with Volatile;
         DL : Fixed_16;
         DR : Fixed_16;

         Row_Top    : Fixed_16;
         Pixel_Left : Fixed_16;

      begin
         --  Compute vertcies of the rectangle to be rasterized.

         if PA.X < PB.X then
            Top_Vertex    := PA + N2;
            Left_Vertex   := PA - N2;
            Right_Vertex  := PB + N2;
            Bottom_Vertex := PB - N2;

         else
            Top_Vertex    := PA - N2;
            Left_Vertex   := PB - N2;
            Right_Vertex  := PA + N2;
            Bottom_Vertex := PB + N2;
         end if;

         --  Snap verticies to the subpixel grid and convert them to Fixed_16
         --  representation.

         Top_Vertex_X    := To_Fixed_16 (To_Fixed_6 (Top_Vertex.X));
         Top_Vertex_Y    := To_Fixed_16 (To_Fixed_6 (Top_Vertex.Y));
         Left_Vertex_X   := To_Fixed_16 (To_Fixed_6 (Left_Vertex.X));
         Left_Vertex_Y   := To_Fixed_16 (To_Fixed_6 (Left_Vertex.Y));
         Right_Vertex_X  := To_Fixed_16 (To_Fixed_6 (Right_Vertex.X));
         Right_Vertex_Y  := To_Fixed_16 (To_Fixed_6 (Right_Vertex.Y));
         Bottom_Vertex_X := To_Fixed_16 (To_Fixed_6 (Bottom_Vertex.X));
         Bottom_Vertex_Y := To_Fixed_16 (To_Fixed_6 (Bottom_Vertex.Y));

         --  Compute slope of the lines of edges of the rectangle.

         Top_Left_Slope_X     :=
           Divide_Saturated
             (Left_Vertex_X - Top_Vertex_X, Left_Vertex_Y - Top_Vertex_Y);
         Top_Right_Slope_X    :=
           Divide_Saturated
             (Right_Vertex_X - Top_Vertex_X, Right_Vertex_Y - Top_Vertex_Y);
         Bottom_Left_Slope_X  :=
           Divide_Saturated
             (Left_Vertex_X - Bottom_Vertex_X,
              Left_Vertex_Y - Bottom_Vertex_Y);
         Bottom_Right_Slope_X :=
           Divide_Saturated
             (Right_Vertex_X - Bottom_Vertex_X,
              Right_Vertex_Y - Bottom_Vertex_Y);

         --  Select slopes of the line at left and line at right sides.

         DL := Top_Left_Slope_X;
         DR := Top_Right_Slope_X;

         --  Compute intersection of the current left and right lines with up
         --  and down edges of the device pixel.

         Left_Edge_Row_Up    :=
           Top_Vertex_X - (Fractional (Top_Vertex_Y)) * Top_Left_Slope_X;
         Right_Edge_Row_Up   :=
           Top_Vertex_X - (Fractional (Top_Vertex_Y)) * Top_Right_Slope_X;
         Left_Edge_Row_Down  :=
           Top_Vertex_X + (One - Fractional (Top_Vertex_Y)) * Top_Left_Slope_X;
         Right_Edge_Row_Down :=
           Top_Vertex_X + (One - Fractional (Top_Vertex_Y)) * Top_Right_Slope_X;

         Row_Top := Floor (Top_Vertex_Y);

         loop
            --  Rasterline's span is divided into up to three segments:
            --   - intersection with the left edge line
            --   - span of the solid area
            --   - intersection with the right edge line
            --
            --  Spans of the intersection can overlap, in such case there is no
            --  solid area between them.
            --
            --  Span of the intersection with the line edge on verticies'
            --  rasterline might be wider than drawn rectangle, so clip them.

            LS := Min (Left_Edge_Row_Up, Left_Edge_Row_Down);
            LE := Max (Left_Edge_Row_Up, Left_Edge_Row_Down);
            RS := Min (Right_Edge_Row_Up, Right_Edge_Row_Down);
            RE := Max (Right_Edge_Row_Up, Right_Edge_Row_Down);

            LS := Max (Left_Vertex_X, LS);
            RE := Min (Right_Vertex_X, RE);
            LE := Min (RE, LE);
            RS := Max (LS, RS);

            --  Do rasterization

            Pixel_Left := Floor (LS);

            while Pixel_Left <= Ceiling_Minus_Delta (LE) loop
               Fill_Span (Integral (Pixel_Left), Integral (Row_Top), 1, 64);
               Pixel_Left := @ + One;
            end loop;

            if Pixel_Left < Floor (RS) then
               Fill_Span
                 (Integral (Pixel_Left),
                  Integral (Row_Top),
                  Integral (RS) - Integral (Pixel_Left),
                  255);
               Pixel_Left := Floor (RS);
            end if;

            while Pixel_Left <= Ceiling_Minus_Delta (RE) loop
               Fill_Span (Integral (Pixel_Left), Integral (Row_Top), 1, 128);
               Pixel_Left := @ + One;
            end loop;

            Left_Edge_Row_Up  := Left_Edge_Row_Down;
            Right_Edge_Row_Up := Right_Edge_Row_Down;

            if Row_Top = Floor (Left_Vertex_Y) then
               --  Pixel of the left vertex has bean reached, switch direction
               --  of the left line to the bottom vertex.

               DL                 := Bottom_Left_Slope_X;
               Left_Edge_Row_Up   :=
                 Left_Vertex_X
                   + Fractional (Left_Vertex_Y) * Bottom_Left_Slope_X;
               Left_Edge_Row_Down :=
                 Left_Vertex_X
                   - (One - Fractional (Left_Vertex_Y)) * Bottom_Left_Slope_X;
            end if;

            if Row_Top = Floor (Right_Vertex_Y) then
               --  Pixel of the right vertex has bean reached, switch direction
               --  of the left line to the bottom vertex.

               DR                  := Bottom_Right_Slope_X;
               Right_Edge_Row_Up   :=
                 Right_Vertex_X
                   + Fractional (Right_Vertex_Y) * Bottom_Right_Slope_X;
               Right_Edge_Row_Down :=
                 Right_Vertex_X
                   - (One - Fractional (Right_Vertex_Y)) * Bottom_Right_Slope_X;
            end if;

            exit when Row_Top = Floor (Bottom_Vertex_Y + One);

            Row_Top             := @ + One;
            Left_Edge_Row_Down  := @ + DL;
            Right_Edge_Row_Down := @ + DR;
         end loop;
      end;
   end Rasterize_Line;

end GFX.Drawing.Primitive_Rasterizer;
