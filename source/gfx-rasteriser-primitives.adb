--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

pragma Ada_2022;

with GFX.Implementation.Fixed_Types;
with GFX.Vectors;

package body GFX.Rasteriser.Primitives is

   use Interfaces;
   use GFX.Implementation.Fixed_Types;
   use GFX.Points;
   use GFX.Vectors;

   function Is_In
     (Lower : Fixed_16; Value : Fixed_16; Upper : Fixed_16) return Boolean;
   --  Returns True when Value is in [Lower, Upper].

   procedure Luminance_Top_Vertex
     (Top_Vertex_X               : Fixed_16;
      Top_Vertex_Y               : Fixed_16;
      Pixel_Top                  : Fixed_16;
      Pixel_Left                 : Fixed_16;
      Pixel_Right                : Fixed_16;
      Pixel_Bottom               : Fixed_16;
      Left_Edge_At_Pixel_Left    : Fixed_16;
      Left_Edge_At_Pixel_Bottom  : Fixed_16;
      Right_Edge_At_Pixel_Left   : Fixed_16;
      Right_Edge_At_Pixel_Bottom : Fixed_16;
      Right_Edge_Slope_Y         : Fixed_16;
      Luminance                  : out Fixed_16);

   procedure Luminance_Left_Vertex
     (Left_Vertex_X                    : Fixed_16;
      Left_Vertex_Y                    : Fixed_16;
      Pixel_Right                      : Fixed_16;
      Left_Top_Edge_At_Pixel_Top       : Fixed_16;
      Left_Top_Edge_At_Pixel_Left      : Fixed_16;
      Left_Top_Edge_Slope_Y            : Fixed_16;
      Left_Bottom_Edge_At_Pixel_Left   : Fixed_16;
      Left_Bottom_Edge_At_Pixel_Bottom : Fixed_16;
      Left_Bottom_Edge_Slope_Y         : Fixed_16;
      Luminance                        : in out Fixed_16);

   procedure Luminance_Bottom_Vertex
     (Bottom_Vertex_X          : Fixed_16;
      Bottom_Vertex_Y          : Fixed_16;
      Pixel_Top                : Fixed_16;
      Pixel_Left               : Fixed_16;
      Pixel_Right              : Fixed_16;
      Left_Edge_At_Pixel_Top   : Fixed_16;
      Left_Edge_At_Pixel_Left  : Fixed_16;
      Right_Edge_At_Pixel_Top  : Fixed_16;
      Right_Edge_At_Pixel_Left : Fixed_16;
      Right_Edge_Slope_Y       : Fixed_16;
      Luminance                : in out Fixed_16);

   procedure Luminance_Left_Edge
     (Pixel_Top                 : Fixed_16;
      Pixel_Right               : Fixed_16;
      Pixel_Bottom              : Fixed_16;
      Left_Edge_At_Pixel_Top    : Fixed_16;
      Left_Edge_At_Pixel_Left   : Fixed_16;
      Left_Edge_At_Pixel_Bottom : Fixed_16;
      Left_Edge_Slope_Y         : Fixed_16;
      Luminance                 : in out Fixed_16);

   procedure Luminance_Right_Edge
     (Pixel_Top                  : Fixed_16;
      Pixel_Bottom               : Fixed_16;
      Pixel_Right                : Fixed_16;
      Right_Edge_At_Pixel_Left   : Fixed_16;
      Right_Edge_At_Pixel_Top    : Fixed_16;
      Right_Edge_At_Pixel_Bottom : Fixed_16;
      Right_Edge_Slope_Y         : Fixed_16;
      Luminance                  : in out Fixed_16);

   Rendering_Area_Top    : Fixed_16;
   Rendering_Area_Left   : Fixed_16;
   Rendering_Area_Right  : Fixed_16;
   Rendering_Area_Bottom : Fixed_16;
   --  Bound of the rendering area

   -----------------------------
   -- Internal_Fill_Rectangle --
   -----------------------------

   procedure Internal_Fill_Rectangle
     (Top       : GFX.Rasteriser.Device_Pixel_Coordinate;
      Left      : GFX.Rasteriser.Device_Pixel_Coordinate;
      Right     : GFX.Rasteriser.Device_Pixel_Coordinate;
      Bottom    : GFX.Rasteriser.Device_Pixel_Coordinate;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale))
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
        (HW : GFX.Rasteriser.Device_Pixel_Count;
         HC : Fixed_16);
      --  Fill each line by one span

      procedure Fill_2
        (LI : GFX.Rasteriser.Device_Pixel_Index;
         LW : GFX.Rasteriser.Device_Pixel_Count;
         LC : Fixed_16;
         RI : GFX.Rasteriser.Device_Pixel_Index;
         RW : GFX.Rasteriser.Device_Pixel_Count;
         RC : Fixed_16);
      --  Fill each line by two spans

      procedure Fill_3;
      --  Fill each line by three spans

      ------------
      -- Fill_1 --
      ------------

      procedure Fill_1
        (HW : GFX.Rasteriser.Device_Pixel_Count;
         HC : Fixed_16)
      is
         HG : constant GFX.Rasteriser.Grayscale := To_Grayscale (HC);
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
        (LI : GFX.Rasteriser.Device_Pixel_Index;
         LW : GFX.Rasteriser.Device_Pixel_Count;
         LC : Fixed_16;
         RI : GFX.Rasteriser.Device_Pixel_Index;
         RW : GFX.Rasteriser.Device_Pixel_Count;
         RC : Fixed_16)
      is
         HC : constant Fixed_16 := (TC + BC) - One;
         LG : constant GFX.Rasteriser.Grayscale := To_Grayscale (LC);
         RG : constant GFX.Rasteriser.Grayscale := To_Grayscale (RC);

         Y  : GFX.Rasteriser.Device_Pixel_Index := TI + 1;
         SG : GFX.Rasteriser.Grayscale;
         EG : GFX.Rasteriser.Grayscale;

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
         HL : constant GFX.Rasteriser.Device_Pixel_Count := RI - LI + 1 - 2;
         LG : constant GFX.Rasteriser.Grayscale          := To_Grayscale (LC);
         CG : constant GFX.Rasteriser.Grayscale          := To_Grayscale (One);
         RG : constant GFX.Rasteriser.Grayscale          := To_Grayscale (RC);

         HC : Fixed_16;
         Y  : GFX.Rasteriser.Device_Pixel_Count := TI + 1;
         SG : GFX.Rasteriser.Grayscale;
         MG : GFX.Rasteriser.Grayscale;
         EG : GFX.Rasteriser.Grayscale;

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

   -----------
   -- Is_In --
   -----------

   function Is_In
     (Lower : Fixed_16; Value : Fixed_16; Upper : Fixed_16) return Boolean is
   begin
      return Lower <= Value and Value <= Upper;
   end Is_In;

   -----------------------------
   -- Luminance_Bottom_Vertex --
   -----------------------------

   procedure Luminance_Bottom_Vertex
     (Bottom_Vertex_X          : Fixed_16;
      Bottom_Vertex_Y          : Fixed_16;
      Pixel_Top                : Fixed_16;
      Pixel_Left               : Fixed_16;
      Pixel_Right              : Fixed_16;
      Left_Edge_At_Pixel_Top   : Fixed_16;
      Left_Edge_At_Pixel_Left  : Fixed_16;
      Right_Edge_At_Pixel_Top  : Fixed_16;
      Right_Edge_At_Pixel_Left : Fixed_16;
      Right_Edge_Slope_Y       : Fixed_16;
      Luminance                : in out Fixed_16)
   is
      Right_Edge_At_Pixel_Right : constant Fixed_16 :=
        Right_Edge_At_Pixel_Left
          + (One - Fixed_16_Delta_Fixed) * Right_Edge_Slope_Y;

   begin
      if Left_Edge_At_Pixel_Top < Pixel_Left then
         --  +-------+
         --  x -     |
         --  |   *   |
         --  | X |   |
         --  +-------+

         Luminance :=
           @
             - (Right_Coverage (Left_Edge_At_Pixel_Left)
                  + Right_Coverage (Bottom_Vertex_Y))
                / 2
                * Left_Coverage (Bottom_Vertex_X);

      else
         --  +-x-----+
         --  |  \    |
         --  |   *   |
         --  | X |   |
         --  +-------+

         Luminance :=
           @
             - Left_Coverage (Left_Edge_At_Pixel_Top)
             - (Right_Coverage (Bottom_Vertex_Y) + One)
                  / 2
                  * (Bottom_Vertex_X - Left_Edge_At_Pixel_Top
                       + Fixed_16_Delta_Fixed);
      end if;

      if Right_Edge_At_Pixel_Top < Pixel_Right then
         --  +-----x-+
         --  |    /  |
         --  |   *   |
         --  |   | X |
         --  +-------+

         Luminance :=
           @
             - Right_Coverage (Right_Edge_At_Pixel_Top)
             - (Right_Coverage (Bottom_Vertex_Y) + One)
               / 2
               * (Right_Edge_At_Pixel_Top - Bottom_Vertex_X
                    + Fixed_16_Delta_Fixed);

      else
         Luminance :=
           @
           - (Right_Coverage (Bottom_Vertex_Y)
                + Right_Coverage (Right_Edge_At_Pixel_Right))
              / 2
              * Right_Coverage (Bottom_Vertex_X);
      end if;
   end Luminance_Bottom_Vertex;

   -------------------------
   -- Luminance_Left_Edge --
   -------------------------

   procedure Luminance_Left_Edge
     (Pixel_Top                 : Fixed_16;
      Pixel_Right               : Fixed_16;
      Pixel_Bottom              : Fixed_16;
      Left_Edge_At_Pixel_Top    : Fixed_16;
      Left_Edge_At_Pixel_Left   : Fixed_16;
      Left_Edge_At_Pixel_Bottom : Fixed_16;
      Left_Edge_Slope_Y         : Fixed_16;
      Luminance                 : in out Fixed_16)
   is
      Left_Edge_At_Pixel_Right : constant Fixed_16 :=
        Left_Edge_At_Pixel_Left + Left_Edge_Slope_Y;

   begin
      if Left_Edge_At_Pixel_Left < Pixel_Top then
         if Left_Edge_At_Pixel_Bottom < Pixel_Right then
            Luminance :=
              @
                - (Fractional (Left_Edge_At_Pixel_Top)
                     + Fractional (Left_Edge_At_Pixel_Bottom)) / 2;

         else
            --  pragma Assert
            --    (Is_In (Pixel_Left, Left_Edge_Row_Up, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Left_Edge_At_Pixel_Right, Pixel_Bottom));

            Luminance :=
              @
                - (One
                     - Right_Coverage (Left_Edge_At_Pixel_Top)
                         * Left_Coverage (Left_Edge_At_Pixel_Right)
                 / 2);
         end if;

      elsif Left_Edge_At_Pixel_Left > Pixel_Bottom then
         if Left_Edge_At_Pixel_Top <= Pixel_Right then
            Luminance :=
              @
              - (Fractional (Left_Edge_At_Pixel_Top)
                 + Fractional (Left_Edge_At_Pixel_Bottom)) / 2;

         else
            --  pragma Assert
            --    (Is_In (Pixel_Left, Left_Edge_Row_Down, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Left_Edge_At_Pixel_Right, Pixel_Bottom));

            Luminance :=
              @
                - (One
                     - Right_Coverage (Left_Edge_At_Pixel_Bottom)
                     * Right_Coverage (Left_Edge_At_Pixel_Right)
                       / 2);
         end if;

      else
         if Left_Edge_At_Pixel_Left + Left_Edge_Slope_Y < Pixel_Top then
            --  pragma Assert
            --    (Is_In (Pixel_Left, Left_Edge_Row_Up, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Left_Edge_At_Pixel_Left, Pixel_Bottom));

            Luminance :=
              @
                - Left_Coverage (Left_Edge_At_Pixel_Top)
                    * Left_Coverage (Left_Edge_At_Pixel_Left)
                    / 2;

         elsif Left_Edge_At_Pixel_Left + Left_Edge_Slope_Y > Pixel_Bottom then
            --  pragma Assert
            --    (Is_In (Pixel_Left, Left_Edge_Row_Down, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Left_Edge_At_Pixel_Left, Pixel_Bottom));

            Luminance :=
              @
              - (One
                   - Left_Coverage (Left_Edge_At_Pixel_Bottom)
                   * Right_Coverage (Left_Edge_At_Pixel_Left)
                   / 2);

         else
            if Left_Edge_Slope_Y < Zero then
               --  +-------+
               --  | X   - x
               --  |   -   |
               --  x -     |
               --  +-------+

               Luminance :=
                 @
                   - (Left_Coverage (Left_Edge_At_Pixel_Left)
                        + Left_Coverage (Left_Edge_At_Pixel_Right))
                      / 2;

            else
               --  +-------+
               --  x -     |
               --  |   -   |
               --  | X   - x
               --  +-------+

               Luminance :=
                 @
                   - (Right_Coverage (Left_Edge_At_Pixel_Left)
                        + Right_Coverage (Left_Edge_At_Pixel_Right))
                      / 2;
            end if;
         end if;
      end if;
   end Luminance_Left_Edge;

   ---------------------------
   -- Luminance_Left_Vertex --
   ---------------------------

   procedure Luminance_Left_Vertex
     (Left_Vertex_X                    : Fixed_16;
      Left_Vertex_Y                    : Fixed_16;
      Pixel_Right                      : Fixed_16;
      Left_Top_Edge_At_Pixel_Top       : Fixed_16;
      Left_Top_Edge_At_Pixel_Left      : Fixed_16;
      Left_Top_Edge_Slope_Y            : Fixed_16;
      Left_Bottom_Edge_At_Pixel_Left   : Fixed_16;
      Left_Bottom_Edge_At_Pixel_Bottom : Fixed_16;
      Left_Bottom_Edge_Slope_Y         : Fixed_16;
      Luminance                        : in out Fixed_16)
   is
      Left_Top_Edge_At_Pixel_Right    : Fixed_16;
      Left_Bottom_Edge_At_Pixel_Right : Fixed_16;

   begin
      if Left_Top_Edge_At_Pixel_Top < Pixel_Right then
         --  +-----x-+
         --  | X  /  |
         --  |---*   |
         --  |       |
         --  +-------+

         Luminance :=
           @
             - (Left_Coverage (Left_Top_Edge_At_Pixel_Top)
                  + Left_Coverage (Left_Vertex_X))
                / 2
                * Left_Coverage (Left_Vertex_Y);

      else
         Left_Top_Edge_At_Pixel_Right :=
           Left_Top_Edge_At_Pixel_Left
             + (One - Fixed_16_Delta_Fixed) * Left_Top_Edge_Slope_Y;

         Luminance :=
           @
             - Left_Coverage (Left_Top_Edge_At_Pixel_Right)
             - (Left_Coverage (Left_Vertex_X) + One)
                  / 2
                  * (Left_Vertex_Y - Left_Top_Edge_At_Pixel_Right);
      end if;

      if Left_Bottom_Edge_At_Pixel_Bottom < Pixel_Right then
         --  +-------+
         --  |       |
         --  |---*   |
         --  | X  \  |
         --  +-----x-+

         Luminance :=
           @
             - (Left_Coverage (Left_Vertex_X)
                  + Left_Coverage (Left_Bottom_Edge_At_Pixel_Bottom))
                / 2
                * Right_Coverage (Left_Vertex_Y);

      else
         Left_Bottom_Edge_At_Pixel_Right :=
           Left_Bottom_Edge_At_Pixel_Left
             + (One - Fixed_16_Delta_Fixed) * Left_Bottom_Edge_Slope_Y;

         Luminance :=
           @
             - (Left_Coverage (Left_Vertex_X) + One)
                  / 2
                  * (Left_Bottom_Edge_At_Pixel_Right - Left_Vertex_Y)
             - Right_Coverage (Left_Bottom_Edge_At_Pixel_Right);
      end if;
   end Luminance_Left_Vertex;

   --------------------------
   -- Luminance_Right_Edge --
   --------------------------

   procedure Luminance_Right_Edge
     (Pixel_Top                  : Fixed_16;
      Pixel_Bottom               : Fixed_16;
      Pixel_Right                : Fixed_16;
      Right_Edge_At_Pixel_Left   : Fixed_16;
      Right_Edge_At_Pixel_Top    : Fixed_16;
      Right_Edge_At_Pixel_Bottom : Fixed_16;
      Right_Edge_Slope_Y         : Fixed_16;
      Luminance                  : in out Fixed_16)
   is
      Right_Edge_At_Pixel_Right : Fixed_16;

   begin
      if Right_Edge_At_Pixel_Left < Pixel_Top then
         if Right_Edge_At_Pixel_Bottom < Pixel_Right then
            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Top, Pixel_Right));
            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Bottom, Pixel_Right));

            Luminance :=
              @
                - (Right_Coverage (Right_Edge_At_Pixel_Top)
                     + Right_Coverage (Right_Edge_At_Pixel_Bottom)) / 2;

         else
            Right_Edge_At_Pixel_Right :=
              Right_Edge_At_Pixel_Left
                + (One - Fixed_16_Delta_Fixed) * Right_Edge_Slope_Y;

            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Top, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Right_Edge_At_Pixel_Right, Pixel_Bottom));

            Luminance :=
              @
                - Right_Coverage (Right_Edge_At_Pixel_Top)
                    * Left_Coverage (Right_Edge_At_Pixel_Right)
                    / 2;
         end if;

      elsif Right_Edge_At_Pixel_Left > Pixel_Bottom then
         if Right_Edge_At_Pixel_Top <= Pixel_Right then
            Luminance :=
              @
                - (Right_Coverage (Right_Edge_At_Pixel_Top)
                     + Right_Coverage (Right_Edge_At_Pixel_Bottom)) / 2;

         else
            Right_Edge_At_Pixel_Right :=
              Right_Edge_At_Pixel_Left
                + (One - Fixed_16_Delta_Fixed) * Right_Edge_Slope_Y;

            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Bottom, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Right_Edge_At_Pixel_Right, Pixel_Bottom));

            Luminance :=
              @
                - Right_Coverage (Right_Edge_At_Pixel_Right)
                    * Right_Coverage (Right_Edge_At_Pixel_Bottom)
                    / 2;
         end if;

      else
         Right_Edge_At_Pixel_Right :=
           Right_Edge_At_Pixel_Left
             + (One - Fixed_16_Delta_Fixed) * Right_Edge_Slope_Y;

         if Right_Edge_At_Pixel_Right < Pixel_Top then
            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Top, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Right_Edge_At_Pixel_Left, Pixel_Bottom));

            Luminance :=
              @
                - (One
                     - Left_Coverage (Right_Edge_At_Pixel_Left)
                         * Left_Coverage (Right_Edge_At_Pixel_Top)
                         / 2);

         elsif Right_Edge_At_Pixel_Right > Pixel_Bottom then
            --  pragma Assert
            --    (Is_In (Pixel_Left, Right_Edge_At_Row_Bottom, Pixel_Right));
            pragma Assert
              (Is_In (Pixel_Top, Right_Edge_At_Pixel_Left, Pixel_Bottom));

            Luminance :=
              @
                - (One
                     - Right_Coverage (Right_Edge_At_Pixel_Left)
                         * Left_Coverage (Right_Edge_At_Pixel_Bottom)
                         / 2);

         else
            if Right_Edge_Slope_Y < Zero then
               --  Bottom-right edge
               --
               --  +-------+
               --  |     - x
               --  |   -   |
               --  x -   X |
               --  +-------+


               Luminance :=
                 @
                   - (Right_Coverage (Right_Edge_At_Pixel_Left)
                        + Right_Coverage (Right_Edge_At_Pixel_Right))
                      / 2;

            else
               --  Top-right edge
               --
               --  +-------+
               --  x -   X |
               --  |   -   |
               --  |     - x
               --  +-------+

               Luminance :=
                 @
                   - (Left_Coverage (Right_Edge_At_Pixel_Left)
                        + Right_Coverage (Right_Edge_At_Pixel_Right))
                      / 2;
            end if;
         end if;
      end if;
   end Luminance_Right_Edge;

   --------------------------
   -- Luminance_Top_Vertex --
   --------------------------

   procedure Luminance_Top_Vertex
     (Top_Vertex_X               : Fixed_16;
      Top_Vertex_Y               : Fixed_16;
      Pixel_Top                  : Fixed_16;
      Pixel_Left                 : Fixed_16;
      Pixel_Right                : Fixed_16;
      Pixel_Bottom               : Fixed_16;
      Left_Edge_At_Pixel_Left    : Fixed_16;
      Left_Edge_At_Pixel_Bottom  : Fixed_16;
      Right_Edge_At_Pixel_Left   : Fixed_16;
      Right_Edge_At_Pixel_Bottom : Fixed_16;
      Right_Edge_Slope_Y         : Fixed_16;
      Luminance                  : out Fixed_16)
   is
      Right_Edge_At_Pixel_Right : constant Fixed_16 :=
        Right_Edge_At_Pixel_Left
          + (One - Fixed_16_Delta_Fixed) * Right_Edge_Slope_Y;

   begin
      pragma Assert (Left_Edge_At_Pixel_Left    >= Top_Vertex_Y);
      pragma Assert (Left_Edge_At_Pixel_Bottom  <= Top_Vertex_X);
      pragma Assert (Right_Edge_At_Pixel_Right  >= Top_Vertex_Y);
      pragma Assert (Right_Edge_At_Pixel_Bottom >= Top_Vertex_X);

      Luminance := One;

      if Left_Edge_At_Pixel_Bottom > Pixel_Left then
         --  +-------+
         --  |   |   |
         --  | X *   |
         --  |  /    |
         --  +-x-----+

         --  pragma Assert (Pixel_Left, Top_Vertex_X, Pixel_Right);
         --  pragma Assert
         --    (Is_In (Pixel_Left, Left_Edge_At_Pixel_Bottom, Pixel_Right));

         Luminance :=
           @
             - Left_Coverage (Left_Edge_At_Pixel_Bottom)
             - (Left_Coverage (Top_Vertex_Y) + One)
               / 2
               * (Top_Vertex_X - Left_Edge_At_Pixel_Bottom
                    + Fixed_16_Delta_Fixed);

      else
         --  +-------+
         --  | X |   |
         --  |   *   |
         --  x -     |
         --  +-------+

         Luminance :=
           @
             - (Left_Coverage (Left_Edge_At_Pixel_Left)
                  + Left_Coverage (Top_Vertex_Y))
                / 2
                * Left_Coverage (Top_Vertex_X);
      end if;

      if Right_Edge_At_Pixel_Bottom < Pixel_Right then
         --  pragma Assert (Is_In (Pixel_Left, Top_Vertex_X, Pixel_Right));
         --  pragma Assert
         --    (Is_In (Pixel_Left, Right_Edge_At_Pixel_Bottom, Pixel_Right));

         Luminance :=
           @
           - Right_Coverage (Right_Edge_At_Pixel_Bottom)
           - (Left_Coverage (Top_Vertex_Y) + One) / 2
              * (Right_Edge_At_Pixel_Bottom - Top_Vertex_X);

      else
         --  +-------+
         --  |   | X |
         --  |   *   |
         --  |     - x
         --  +-------+

         Luminance :=
           @
           - (Left_Coverage (Top_Vertex_Y)
                + Left_Coverage (Right_Edge_At_Pixel_Right))
              / 2
              * Right_Coverage (Top_Vertex_X);
      end if;
   end Luminance_Top_Vertex;

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
      --  Draw_Span : not null Draw_Span_Subprogram)
   is
      pragma Suppress (All_Checks);

      procedure Compute_Slope
        (From_X  : Fixed_16;
         From_Y  : Fixed_16;
         To_X    : Fixed_16;
         To_Y    : Fixed_16;
         Slope_X : out Fixed_16;
         Slope_Y : out Fixed_16);
      --  Compute slopes for X and Y directions of the given vector.

      -------------------
      -- Compute_Slope --
      -------------------

      procedure Compute_Slope
        (From_X  : Fixed_16;
         From_Y  : Fixed_16;
         To_X    : Fixed_16;
         To_Y    : Fixed_16;
         Slope_X : out Fixed_16;
         Slope_Y : out Fixed_16)
      is
         DX : constant Fixed_16 := To_X - From_X;
         DY : constant Fixed_16 := To_Y - From_Y;

      begin
         Slope_X := Divide_Saturated (DX, DY);
         Slope_Y := Divide_Saturated (DY, DX);
      end Compute_Slope;

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

         Top_Vertex_X         : Fixed_16;
         Top_Vertex_Y         : Fixed_16;
         Left_Vertex_X        : Fixed_16;
         Left_Vertex_Y        : Fixed_16;
         Right_Vertex_X       : Fixed_16;
         Right_Vertex_Y       : Fixed_16;
         Bottom_Vertex_X      : Fixed_16;
         Bottom_Vertex_Y      : Fixed_16;
         --  Coordinates of the drawn rectangle's verticies in fixed point.

         Top_Left_Slope_X     : Fixed_16;
         Top_Left_Slope_Y     : Fixed_16;
         Top_Right_Slope_X    : Fixed_16;
         Top_Right_Slope_Y    : Fixed_16;
         Bottom_Left_Slope_X  : Fixed_16;
         Bottom_Left_Slope_Y  : Fixed_16;
         Bottom_Right_Slope_X : Fixed_16;
         Bottom_Right_Slope_Y : Fixed_16;
         --  Slopes of the edge lines of the drawn rectangle in fixed point.

         Left_Edge_Row_Up         : Fixed_16;
         Right_Edge_At_Row_Top    : Fixed_16;
         Left_Edge_Row_Down       : Fixed_16;
         Right_Edge_At_Row_Bottom : Fixed_16;
         --  X coordinate of the intersection of the edge line of the draw
         --  rectangle with the current rasterline.

         Left_Edge_Row_Left     : Fixed_16;
         Left_Edge_Pixel_Left   : Fixed_16;
         Left_Edge_Pixel_Right  : Fixed_16;

         Left_Bottom_Edge_At_Row_Top    : Fixed_16;
         Left_Bottom_Edge_At_Pixel_Left : Fixed_16;
         Left_Bottom_Edge_At_Row_Bottom : Fixed_16;

         Right_Edge_At_Pixel_Left   : Fixed_16;
         Right_Edge_At_Pixel_Right  : Fixed_16;

         LS  : Fixed_16;
         LE  : Fixed_16;
         RS  : Fixed_16;
         RE  : Fixed_16;
         Left_Slope_X  : Fixed_16;
         Left_Slope_Y  : Fixed_16;
         Right_Slope_X : Fixed_16;
         Right_Slope_Y : Fixed_16;

         Row_Top        : Fixed_16;
         Row_Bottom     : Fixed_16;
         Pixel_Left     : Fixed_16;
         Pixel_Right    : Fixed_16;
         Pixel_Coverage : Fixed_16;

         Clip_Bottom : Fixed_16;

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

         Compute_Slope
           (From_X  => Top_Vertex_X,
            From_Y  => Top_Vertex_Y,
            To_X    => Left_Vertex_X,
            To_Y    => Left_Vertex_Y,
            Slope_X => Top_Left_Slope_X,
            Slope_Y => Top_Left_Slope_Y);
         Compute_Slope
           (From_X  => Top_Vertex_X,
            From_Y  => Top_Vertex_Y,
            To_X    => Right_Vertex_X,
            To_Y    => Right_Vertex_Y,
            Slope_X => Top_Right_Slope_X,
            Slope_Y => Top_Right_Slope_Y);
         Compute_Slope
           (From_X  => Bottom_Vertex_X,
            From_Y  => Bottom_Vertex_Y,
            To_X    => Left_Vertex_X,
            To_Y    => Left_Vertex_Y,
            Slope_X => Bottom_Left_Slope_X,
            Slope_Y => Bottom_Left_Slope_Y);
         Compute_Slope
           (From_X  => Bottom_Vertex_X,
            From_Y  => Bottom_Vertex_Y,
            To_X    => Right_Vertex_X,
            To_Y    => Right_Vertex_Y,
            Slope_X => Bottom_Right_Slope_X,
            Slope_Y => Bottom_Right_Slope_Y);

         --  Select slopes of the line at left and line at right sides.

         Left_Slope_X  := Top_Left_Slope_X;
         Left_Slope_Y  := Top_Left_Slope_Y;
         Right_Slope_X := Top_Right_Slope_X;
         Right_Slope_Y := Top_Right_Slope_Y;

         --  Compute intersection of the current left and right lines with up
         --  and down edges of the device pixel.

         Row_Top    := Pixel_Lower_Bound (Top_Vertex_Y);
         Row_Bottom := Pixel_Upper_Bound (Top_Vertex_Y);

         Clip_Bottom :=
           Min (Rendering_Area_Bottom, Pixel_Upper_Bound (Bottom_Vertex_Y));

         Left_Edge_Row_Up         :=
           Left_Vertex_X - (Left_Vertex_Y - Row_Top) * Left_Slope_X;
         Left_Edge_Row_Down       :=
           Left_Vertex_X - (Left_Vertex_Y - Row_Bottom) * Left_Slope_X;
         Right_Edge_At_Row_Top    :=
           Right_Vertex_X - (Right_Vertex_Y - Row_Top) * Right_Slope_X;
         Right_Edge_At_Row_Bottom :=
           Right_Vertex_X - (Right_Vertex_Y - Row_Bottom) * Right_Slope_X;

         loop
            Row_Bottom := Pixel_Upper_Bound (Row_Top);

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
            RS := Min (Right_Edge_At_Row_Top, Right_Edge_At_Row_Bottom);
            RE := Max (Right_Edge_At_Row_Top, Right_Edge_At_Row_Bottom);

            LS := Max (Left_Vertex_X, LS);
            RE := Min (Right_Vertex_X, RE);
            LE := Min (RE, LE);
            RS := Max (LS, RS);

            if Integral (Row_Top) = Integral (Bottom_Vertex_Y) then
               LE := Min (Bottom_Vertex_X, LE);
            end if;

            --  Do rasterization of the rasterline

            Pixel_Left  := Floor (LS);
            Pixel_Right := Ceiling_Minus_Delta (LS);

            Left_Edge_Row_Left :=
              Left_Vertex_Y - (Left_Vertex_X - Floor (LS)) * Left_Slope_Y;

            Left_Edge_Pixel_Left := Left_Edge_Row_Left;
            Right_Edge_At_Pixel_Left :=
              Right_Vertex_Y
                - Multiply_Saturated
                    (Right_Vertex_X - Floor (LS), Right_Slope_Y);

            while Pixel_Left <= Ceiling_Minus_Delta (LE) loop
               Pixel_Coverage := One;

               if Integral (Row_Bottom) = Integral (Top_Vertex_Y)
                 and Integral (Pixel_Left) = Integral (Top_Vertex_X)
               then
                  Luminance_Top_Vertex
                    (Top_Vertex_X               => Top_Vertex_X,
                     Top_Vertex_Y               => Top_Vertex_Y,
                     Pixel_Top                  => Row_Top,
                     Pixel_Left                 => Pixel_Left,
                     Pixel_Right                => Pixel_Right,
                     Pixel_Bottom               => Row_Bottom,
                     Left_Edge_At_Pixel_Left    => Left_Edge_Pixel_Left,
                     Left_Edge_At_Pixel_Bottom  => Left_Edge_Row_Down,
                     Right_Edge_At_Pixel_Left   => Right_Edge_At_Pixel_Left,
                     Right_Edge_At_Pixel_Bottom => Right_Edge_At_Row_Bottom,
                     Right_Edge_Slope_Y         => Right_Slope_Y,
                     Luminance                  => Pixel_Coverage);

               elsif Integral (Row_Bottom) = Integral (Left_Vertex_Y)
                 and Integral (Pixel_Left) = Integral (Left_Vertex_X)
               then
                  Left_Bottom_Edge_At_Pixel_Left :=
                    Left_Vertex_Y
                      - (Left_Vertex_X - Pixel_Left) * Bottom_Left_Slope_Y;
                  Left_Bottom_Edge_At_Row_Bottom :=
                    Left_Vertex_X
                      - (Left_Vertex_Y - Row_Bottom) * Bottom_Left_Slope_X;

                  Luminance_Left_Vertex
                    (Left_Vertex_X                    => Left_Vertex_X,
                     Left_Vertex_Y                    => Left_Vertex_Y,
                     Pixel_Right                      => Pixel_Right,
                     Left_Top_Edge_At_Pixel_Top       => Left_Edge_Row_Up,
                     Left_Top_Edge_At_Pixel_Left      => Left_Edge_Pixel_Left,
                     Left_Top_Edge_Slope_Y            => Top_Left_Slope_Y,
                     Left_Bottom_Edge_At_Pixel_Left   =>
                       Left_Bottom_Edge_At_Pixel_Left,
                     Left_Bottom_Edge_At_Pixel_Bottom =>
                       Left_Bottom_Edge_At_Row_Bottom,
                     Left_Bottom_Edge_Slope_Y         => Bottom_Left_Slope_Y,
                     Luminance                        => Pixel_Coverage);

               elsif Integral (Row_Bottom) = Integral (Bottom_Vertex_Y)
                 and Integral (Pixel_Left) = Integral (Bottom_Vertex_X)
               then
                  Luminance_Bottom_Vertex
                    (Bottom_Vertex_X          => Bottom_Vertex_X,
                     Bottom_Vertex_Y          => Bottom_Vertex_Y,
                     Pixel_Top                => Row_Top,
                     Pixel_Left               => Pixel_Left,
                     Pixel_Right              => Pixel_Right,
                     Left_Edge_At_Pixel_Top   => Left_Edge_Row_Up,
                     Left_Edge_At_Pixel_Left  => Left_Edge_Pixel_Left,
                     Right_Edge_At_Pixel_Top  => Right_Edge_At_Row_Top,
                     Right_Edge_At_Pixel_Left => Right_Edge_At_Pixel_Left,
                     Right_Edge_Slope_Y       => Bottom_Right_Slope_Y,
                     Luminance                => Pixel_Coverage);

               else
                  Luminance_Left_Edge
                    (Pixel_Top                 => Row_Top,
                     Pixel_Right               => Pixel_Right,
                     Pixel_Bottom              => Row_Bottom,
                     Left_Edge_At_Pixel_Top    => Left_Edge_Row_Up,
                     Left_Edge_At_Pixel_Left   => Left_Edge_Pixel_Left,
                     Left_Edge_At_Pixel_Bottom => Left_Edge_Row_Down,
                     Left_Edge_Slope_Y         => Left_Slope_Y,
                     Luminance                 => Pixel_Coverage);

                  if Integral (Row_Bottom) = Integral (Left_Vertex_Y) then
                     Left_Bottom_Edge_At_Pixel_Left :=
                       Left_Vertex_Y
                         - (Left_Vertex_X - Pixel_Left) * Bottom_Left_Slope_Y;
                     Left_Bottom_Edge_At_Row_Top    :=
                       Left_Vertex_X
                         - (Left_Vertex_Y - Row_Top) * Bottom_Left_Slope_X;
                     Left_Bottom_Edge_At_Row_Bottom :=
                       Left_Vertex_X
                         - (Left_Vertex_Y - Row_Bottom) * Bottom_Left_Slope_X;

                     if Integral (Pixel_Left)
                       <= Integral (Left_Bottom_Edge_At_Row_Bottom)
                     then
                        Luminance_Left_Edge
                          (Pixel_Top                 => Row_Top,
                           Pixel_Right               => Pixel_Right,
                           Pixel_Bottom              => Row_Bottom,
                           Left_Edge_At_Pixel_Top    =>
                             Left_Bottom_Edge_At_Row_Top,
                           Left_Edge_At_Pixel_Left   =>
                             Left_Bottom_Edge_At_Pixel_Left,
                           Left_Edge_At_Pixel_Bottom =>
                             Left_Bottom_Edge_At_Row_Bottom,
                           Left_Edge_Slope_Y         => Bottom_Left_Slope_Y,
                           Luminance                 => Pixel_Coverage);
                     end if;
                  end if;

                  if RS <= Pixel_Right then
                     Luminance_Right_Edge
                       (Pixel_Top                  => Row_Top,
                        Pixel_Bottom               => Row_Bottom,
                        Pixel_Right                => Pixel_Right,
                        Right_Edge_At_Pixel_Left   => Right_Edge_At_Pixel_Left,
                        Right_Edge_At_Pixel_Top    => Right_Edge_At_Row_Top,
                        Right_Edge_At_Pixel_Bottom => Right_Edge_At_Row_Bottom,
                        Right_Edge_Slope_Y         => Right_Slope_Y,
                        Luminance                  => Pixel_Coverage);
                  end if;
               end if;

               Fill_Span
                 (Integral (Pixel_Left),
                  Integral (Row_Top),
                  1,
                  Grayscale (Integral (255 * Pixel_Coverage)));

               Left_Edge_Pixel_Left     := @ + Left_Slope_Y;
               Right_Edge_At_Pixel_Left := @ + Right_Slope_Y;
               Pixel_Left               := @ + One;
               Pixel_Right              := @ + One;
            end loop;

            if Pixel_Left < Floor (RS) then
               Fill_Span
                 (Integral (Pixel_Left),
                  Integral (Row_Top),
                  Integral (RS) - Integral (Pixel_Left),
                  255);
               Pixel_Left  := Floor (RS);
               Pixel_Right := Ceiling_Minus_Delta (RS);
            end if;

            Right_Edge_At_Pixel_Left :=
              Right_Vertex_Y
                - Multiply_Saturated
                    (Right_Vertex_X - Floor (RS), Right_Slope_Y);

            while Pixel_Left <= Ceiling_Minus_Delta (RE) loop
               Pixel_Coverage := One;

               Luminance_Right_Edge
                 (Pixel_Top                  => Row_Top,
                  Pixel_Bottom               => Row_Bottom,
                  Pixel_Right                => Pixel_Right,
                  Right_Edge_At_Pixel_Left   => Right_Edge_At_Pixel_Left,
                  Right_Edge_At_Pixel_Top    => Right_Edge_At_Row_Top,
                  Right_Edge_At_Pixel_Bottom => Right_Edge_At_Row_Bottom,
                  Right_Edge_Slope_Y         => Right_Slope_Y,
                  Luminance                  => Pixel_Coverage);

               Fill_Span
                 (Integral (Pixel_Left),
                  Integral (Row_Top),
                  1,
                  Grayscale (Integral (255 * Pixel_Coverage)));

               Right_Edge_At_Pixel_Left := @ + Right_Slope_Y;
               Pixel_Left               := @ + One;
               Pixel_Right              := @ + One;
            end loop;

            exit when Row_Bottom = Clip_Bottom;

            --  Compute data for the next iteration

            if Row_Top = Floor (Left_Vertex_Y) then
               --  Pixel of the left vertex has been reached, switch direction
               --  of the left line to the bottom vertex.

               Left_Slope_X        := Bottom_Left_Slope_X;
               Left_Slope_Y        := Bottom_Left_Slope_Y;
               Left_Edge_Row_Up    :=
                 Left_Vertex_X - (Left_Vertex_Y - Row_Top) * Left_Slope_X;
               Left_Edge_Row_Down  :=
                 Left_Vertex_X - (Left_Vertex_Y - Row_Bottom) * Left_Slope_X;
            end if;

            Left_Edge_Row_Up   := @ + Left_Slope_X;
            Left_Edge_Row_Down := @ + Left_Slope_X;

            if Row_Top = Floor (Right_Vertex_Y) then
               --  Pixel of the right vertex has bean reached, switch direction
               --  of the left line to the bottom vertex.

               Right_Slope_X            := Bottom_Right_Slope_X;
               Right_Slope_Y            := Bottom_Right_Slope_Y;
               Right_Edge_At_Row_Top    :=
                 Right_Vertex_X - (Right_Vertex_Y - Row_Top) * Right_Slope_X;
               Right_Edge_At_Row_Bottom :=
                 Right_Vertex_X
                   - (Right_Vertex_Y - Row_Bottom) * Right_Slope_X;
            end if;

            Right_Edge_At_Row_Top    := @ + Right_Slope_X;
            Right_Edge_At_Row_Bottom := @ + Right_Slope_X;

            Row_Top    := @ + One;
            Row_Bottom := @ + One;
         end loop;
      end;
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
      Rendering_Area_Left   := Pixel_Lower_Bound (Left);
      Rendering_Area_Right  := Pixel_Upper_Bound (Right);
      Rendering_Area_Top    := Pixel_Lower_Bound (Top);
      Rendering_Area_Bottom := Pixel_Upper_Bound (Bottom);
   end Set_Rendering_Area;

end GFX.Rasteriser.Primitives;
