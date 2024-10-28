--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This version of the package is optimized for use on Cortex-M4 (ARMv7-M)
--  processor with single pecision FPU.
--
--  Input coordinates are limited to 16 bits signed integer range,
--  [-32_768 .. 32_767].
--
--  XXX Critical point to enchance range is computation of the ellipse
--  coordinates. Right now it assumes grid coordinates to be fixed point number
--  of Q18.6 format (24 bits total). Square of this numbers prodice Q36.12 (48
--  bits) result, 12 bits is necessary to don't lost precision of the division
--  and 4 bits is used to improve precision of this operation. In total
--  intermediate value of the Y^2 divident can occupy up to 64 bits.
--
--  So actual limits for coordinates is 18 bit signed integer right now, but it
--  might be lowered to improve precision.

pragma Restrictions (No_Elaboration_Code);
pragma Ada_2022;

package body GFX.Rasteriser.CSS_Borders is

   --  type F_18_6 is delta 1.0 / 2 ** 6 range -2.0**17 .. 2.0**17-1.0/2**6
   --    with Size => 24, Object_Size => 32;
   --
   --  type F_16_8 is delta 1.0 / 2 ** 8 range -2.0**15 .. 2.0**15-1.0/2**8
   --    with Size => 24, Object_Size => 32;
   --
   --  F_18_6_1 : constant F_18_6 := 1.0 with Export;
   --  F_18_6_2 : constant F_18_6 := 1.0 / 2 with Export;
   --  F_16_8_2 : constant F_16_8 := 1.0 / 2 with Export;
   --
   --  type Subpixel is new F_18_6;
   --
   --  type Grid is new F_18_6;
   --
   --  --  type Q_18_6 is
   --
   --  function Device_Pixel
   --    (Item : Grid) return GFX.Rasteriser.Device_Pixel_Index is
   --    --  (Item : F_18_6) return GFX.Rasteriser.Device_Pixel_Index is
   --  begin
   --     return GFX.Rasteriser.Device_Pixel_Index (Item - 0.5);
   --     --  return GFX.Rasteriser.Device_Pixel_Index (Item / 2 ** 6);
   --  end Device_Pixel;
   --
   --  function To_Grid (Item : GFX.GX_Real) return Grid is
   --     S : constant Subpixel := Subpixel (Item);
   --
   --  begin
   --     return Grid (Item) - 0.5;
   --  end To_Grid;

   type Grid_Coordinate is new GFX.GX_Integer;

   Grid_One   : constant := 2 ** 6;
   Grid_Delta : constant := 1.0 / Grid_One;

   function Snap_To_Grid (Item : GX_Real) return Grid_Coordinate;

   function Length_To_Grid (Item : GX_Real) return Grid_Coordinate;

   function Snap_To_Grid (Item : GX_Real) return Grid_Coordinate is
   begin
      return Grid_Coordinate ((Item + 0.5) / Grid_Delta);
   end Snap_To_Grid;

   function Length_To_Grid (Item : GX_Real) return Grid_Coordinate is
   begin
      return Grid_Coordinate (Item / Grid_Delta);
   end Length_To_Grid;

   function Device_Pixel
     (Item : Grid_Coordinate) return GFX.Rasteriser.Device_Pixel_Index is
   begin
   --     return GFX.Rasteriser.Device_Pixel_Index (Item - 0.5);
   --     --  return GFX.Rasteriser.Device_Pixel_Index (Item / 2 ** 6);
      return GFX.Rasteriser.Device_Pixel_Index (Item / Grid_One);
   end Device_Pixel;

   -----------
   -- Lower --
   -----------

   function Lower (Item : Device_Pixel_Index) return Grid_Coordinate is
   begin
      return Grid_Coordinate (Item) * Grid_One;
   end Lower;

   function Upper (Item : Device_Pixel_Index) return Grid_Coordinate is
   begin
      return Grid_Coordinate (Item) * Grid_One + Grid_One - 1;
   end Upper;

   function Ellipse_X
     (A : Grid_Coordinate;
      B : Grid_Coordinate;
      Y : Grid_Coordinate) return Grid_Coordinate
   is
      Grid_One_2 : constant := Grid_One * Grid_One;
      Over       : constant := 4;
      Over_2     : constant := Over * Over;

      Y2  : constant GX_Integer_2 := GX_Integer_2 (Y) * GX_Integer_2 (Y) with Export;
      B2  : constant GX_Integer_2 := GX_Integer_2 (B) * GX_Integer_2 (B) with Export;
      Y2D : constant GX_Integer_2 := Y2 * Grid_One_2 * Over_2 with Export;
      YB  : constant GX_Integer   := GX_Integer (Y2D / B2) with Export;
      MYB : constant GX_Integer   := Grid_One_2 * Over_2 - YB with Export;
      S   : constant GX_Real      :=  (Sqrt (GX_Real (MYB))) with Export;
      AS  : constant GX_Integer   := GX_Integer (A) * GX_Integer (S) / Over with Export;

   begin
      return Grid_Coordinate (AS / Grid_One);
   end Ellipse_X;

   function Ellipse_Y
     (A : Grid_Coordinate;
      B : Grid_Coordinate;
      X : Grid_Coordinate) return Grid_Coordinate is
   begin
      return Ellipse_X (B, A, X);
   end Ellipse_Y;

   type Ellipse is record
      Center_X          : Grid_Coordinate;
      Center_Y          : Grid_Coordinate;
      Horizontal_Radius : Grid_Coordinate;
      Vertical_Radius   : Grid_Coordinate;
      Inverse_X         : Boolean;
      Inverse_Y         : Boolean;
   end record;

   function Coordinate_X
     (Self         : Ellipse;
      Coordinate_Y : Grid_Coordinate) return Grid_Coordinate;

   function Coordinate_Y
     (Self         : Ellipse;
      Coordinate_X : Grid_Coordinate) return Grid_Coordinate;

   ------------------
   -- Coordinate_X --
   ------------------

   function Coordinate_X
     (Self         : Ellipse;
      Coordinate_Y : Grid_Coordinate) return Grid_Coordinate
   is
      Aux : constant Grid_Coordinate :=
        Ellipse_X
          (Self.Horizontal_Radius,
           Self.Vertical_Radius,
           Coordinate_Y - Self.Center_Y);

   begin
      return
        (if Self.Inverse_X then Self.Center_X - Aux else Self.Center_X + Aux);
   end Coordinate_X;

   ------------------
   -- Coordinate_Y --
   ------------------

   function Coordinate_Y
     (Self         : Ellipse;
      Coordinate_X : Grid_Coordinate) return Grid_Coordinate
   is
      Aux : constant Grid_Coordinate :=
        Ellipse_X
          (Self.Vertical_Radius,
           Self.Horizontal_Radius,
           Coordinate_X - Self.Center_X);

   begin
      return
        (if Self.Inverse_Y then Self.Center_Y - Aux else Self.Center_Y + Aux);
   end Coordinate_Y;

   --------------------------
   -- Rasterize_CSS_Border --
   --------------------------

   procedure Rasterize_CSS_Border
     (Border    : CSS_Border;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale))
   is

      procedure TL
        (ST          : Grid_Coordinate;  --  Scanline cell top
         SR          : Grid_Coordinate;  --                right
         SB          : Grid_Coordinate;  --                bottom
         SL          : Grid_Coordinate;  --                left
         ET          : Grid_Coordinate;  --  Edge at top
         ER          : Grid_Coordinate;  --          right
         EB          : Grid_Coordinate;  --          bottom
         EL          : Grid_Coordinate;  --          left
         Area        : out GX_Integer;
         Coverage : in out GX_Integer);

      --------
      -- TL --
      --------

      procedure TL
        (ST          : Grid_Coordinate;  --  Scanline cell top
         SR          : Grid_Coordinate;  --                right
         SB          : Grid_Coordinate;  --                bottom
         SL          : Grid_Coordinate;  --                left
         ET          : Grid_Coordinate;  --  Edge at top
         ER          : Grid_Coordinate;  --          right
         EB          : Grid_Coordinate;  --          bottom
         EL          : Grid_Coordinate;  --          left
         Area        : out GX_Integer;
         Coverage : in out GX_Integer)
      is
         Height : GX_Integer;

      begin
         if ER < ST then
            --  Crossing top side of the scanline cell

            if EL < ST then
               --  Crossing top side of the scanline cell

               raise Program_Error;

            elsif EL <= SB then
               --  Crossing left side of the scanline cell

               Height := GX_Integer (EL - ST);
               Area   := GX_Integer (ET - SL) * Height;

            else
               --  Crossing bottom side of the scanline cell

               Height := Grid_One;
               Area   := GX_Integer ((ET - SL) + (EB - SL)) * Height;
            end if;

         elsif ER <= SB then
            --  Crossing right side of the scanline cell

            if EL < ST then
               --  Crossing top side of the scanline cell

               raise Program_Error;

            elsif EL <= SB then
               --  Crossing left side of the scanline cell

               Height := GX_Integer (EL - ER);
               Area   := Grid_One * Height;

            else
               --  Crossing bottom side of the scanline cell

               Height := GX_Integer (SB - ER);
               Area   := (GX_Integer (EB - SL) + Grid_One) * Height;
            end if;

         else
            --  Crossing bottom side of the scanline cell

            raise Program_Error;
         end if;

         Coverage := @ + Height;
      end TL;

      --------
      -- TR --
      --------

      procedure TR
        (ST          : Grid_Coordinate;  --  Scanline cell top
         SR          : Grid_Coordinate;  --                right
         SB          : Grid_Coordinate;  --                bottom
         SL          : Grid_Coordinate;  --                left
         ET          : Grid_Coordinate;  --  Edge at top
         ER          : Grid_Coordinate;  --          right
         EB          : Grid_Coordinate;  --          bottom
         EL          : Grid_Coordinate;  --          left
         Area        : out GX_Integer;
         Coverage : in out GX_Integer)
      is
         Height : GX_Integer;

      begin
         if ER < ST then
            --  Crossing top side of the scanline cell

            if EL < ST then
               --  Crossing top side of the scanline cell

               raise Program_Error;

            elsif EL <= SB then
               --  Crossing left side of the scanline cell

               --  Height := GX_Integer (EL - ST);
               --  Area   := GX_Integer (ET - SL) * Height;
               raise Program_Error;

            else
               --  Crossing bottom side of the scanline cell

               --  Height := Grid_One;
               --  Area   := GX_Integer ((ET - SL) + (EB - SL)) * Height;
               raise Program_Error;
            end if;

         elsif ER <= SB then
            --  Crossing right side of the scanline cell

            if EL < ST then
               --  Crossing top side of the scanline cell

               Height := -GX_Integer (ER - ST);
               Area   := GX_Integer (ET - SL + Grid_One) * Height;

            elsif EL <= SB then
               --  Crossing left side of the scanline cell

               Height := GX_Integer (EL - ER);
               Area   := Grid_One * Height;

            else
               --  Crossing bottom side of the scanline cell

               --  Height := GX_Integer (SB - ER);
               --  Area   := (GX_Integer (EB - SL) + Grid_One) * Height;
               raise Program_Error;
            end if;

         else
            --  Crossing bottom side of the scanline cell

            if EL < ST then
               --  Crossing top side of the scanline cell

               Height := -Grid_One;
               Area   := GX_Integer ((ET - SL) + (EB - SL)) * Height;

            elsif EL <= SB then
               --  Crossing left side of the scanline cell

               Height := -GX_Integer (SB - EL);
               Area   := GX_Integer (EB - SL) * Height;

            else
               --  Crossing bottom side of the scanline cell

               raise Program_Error;
            end if;
         end if;

         Coverage := @ + Height;
      end TR;

      --  BT : F_18_6 := F_18_6 (Border.Edge.Top);
      --  BR : F_18_6 := F_18_6 (Border.Edge.Right);
      --  BB : F_18_6 := F_18_6 (Border.Edge.Bottom);
      --  BL : F_18_6 := F_18_6 (Border.Edge.Left);
      BT : Grid_Coordinate := Snap_To_Grid (Border.Edge.Top) with Export;
      BR : Grid_Coordinate := Snap_To_Grid (Border.Edge.Right) with Export;
      BB : Grid_Coordinate := Snap_To_Grid (Border.Edge.Bottom) with Export;
      BL : Grid_Coordinate := Snap_To_Grid (Border.Edge.Left) with Export;

      --  RT : Grid := Grid'Floor (BT);
      --  CL : Grid := Grid'Floor (BR);

      A : Grid_Coordinate := Length_To_Grid (Border.Top_Left_Radius.Horizontal);
      B : Grid_Coordinate := Length_To_Grid (Border.Top_Left_Radius.Vertical);
      X : Grid_Coordinate;
      Y : Grid_Coordinate;

      ETL : Ellipse;
      ETR : Ellipse;
      EBR : Ellipse;
      EBL : Ellipse;

      SR  : Device_Pixel_Index;
      SRT : Grid_Coordinate;
      SRB : Grid_Coordinate;
      SC  : Device_Pixel_Index;
      SCL : Grid_Coordinate with Export;
      SCR : Grid_Coordinate with Export;

      ELL : Grid_Coordinate with Export;
      ELR : Grid_Coordinate with Export;
      ERL : Grid_Coordinate with Export;
      ERR : Grid_Coordinate with Export;
      CAR : Grid_Coordinate with Export;
      CAL : Grid_Coordinate with Export;

      Coverage    : GX_Integer;
      Accumulated : GX_Integer;
      Area        : GX_Integer;
      Gray        : GX_Integer;

   begin
      --  Fill_Span (1, 1, 1, 255);
      --  Fill_Span (2, 1, 1, 127);
      --  Fill_Span (3, 1, 1, 63);
      --  Fill_Span (4, 1, 1, 31);
      --  Fill_Span (5, 1, 1, 15);
      --  Fill_Span (6, 1, 1, 7);
      --  Fill_Span (7, 1, 1, 3);
      --  Fill_Span (8, 1, 1, 1);
      --  Fill_Span (9, 1, 1, 0);
      --  null;
      --
      --  Fill_Span (Device_Pixel (BL), Device_Pixel (BT), 1, 255);
      --  Fill_Span (Device_Pixel (BR), Device_Pixel (BT), 1, 255);
      --  Fill_Span (Device_Pixel (BR), Device_Pixel (BB), 1, 255);
      --  Fill_Span (Device_Pixel (BL), Device_Pixel (BB), 1, 255);

      ETL :=
        (Center_X          =>
           Snap_To_Grid (Border.Edge.Left)
             + Length_To_Grid (Border.Top_Left_Radius.Horizontal),
         Center_Y          =>
           Snap_To_Grid (Border.Edge.Top)
             + Length_To_Grid (Border.Top_Left_Radius.Vertical),
         Horizontal_Radius =>
           Length_To_Grid (Border.Top_Left_Radius.Horizontal),
         Vertical_Radius   =>
           Length_To_Grid (Border.Top_Left_Radius.Vertical),
         Inverse_X         => True,
         Inverse_Y         => True);

      ETR :=
        (Center_X          =>
           Snap_To_Grid (Border.Edge.Right)
             - Length_To_Grid (Border.Top_Right_Radius.Horizontal),
         Center_Y          =>
           Snap_To_Grid (Border.Edge.Top)
             + Length_To_Grid (Border.Top_Right_Radius.Vertical),
         Horizontal_Radius =>
           Length_To_Grid (Border.Top_Right_Radius.Horizontal),
         Vertical_Radius   =>
           Length_To_Grid (Border.Top_Right_Radius.Vertical),
         Inverse_X         => False,
         Inverse_Y         => True);

      X := Snap_To_Grid (Border.Edge.Left) + Grid_One;
      Y := Snap_To_Grid (Border.Edge.Top) + Grid_One;

      loop
         Fill_Span
           (Device_Pixel (Coordinate_X (ETL, Y)),
            Device_Pixel (Y),
            1,
            129);

         Y := @ + Grid_One;

         exit when Y > ETL.Center_Y;
      end loop;

      --  First scanline

      Accumulated := 0;

      SR  := Device_Pixel (BT);
      SRT := Lower (SR);
      SRB := SRT + Grid_One;

      ELL := Coordinate_X (ETL, SRB);
      ELR := Coordinate_X (ETL, BT);
      ERL := Coordinate_X (ETR, BT);
      ERR := Coordinate_X (ETR, SRB);

      SC  := Device_Pixel (ELL);
      SCL := Lower (SC);
      SCR := SCL + Grid_One;

      --  First pixel, ellipse

      CAR := Coordinate_Y (ETL, SCR);

      TL
        (ST       => SRT,
         SR       => SCR,
         SB       => SRB,
         SL       => SCL,
         ET       => Grid_Coordinate'Last,
         ER       => CAR,
         EB       => ELL,
         EL       => Grid_Coordinate'Last,
         Area     => Area,
         Coverage => Accumulated);

      Gray := Grid_One * Accumulated - Area / 2;
      Gray := @ * 255 / (Grid_One * Grid_One);

      Fill_Span (SC, SR, 1, Grayscale (Gray));

      SC  := @ + 1;
      SCL := @ + Grid_One;
      SCR := @ + Grid_One;

      --  Continuation of the ellipse

      loop
         exit when SCR > ELR;

         CAL := CAR;
         CAR := Coordinate_Y (ETL, SCR);

         TL
           (ST => SRT,
            SR => SCR,
            SB => SRB,
            SL => SCL,
            ET => Grid_Coordinate'Last,
            ER => CAR,
            EB => ELL,
            EL => CAL,
            Area      => Area,
            Coverage => Accumulated);

         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));

         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;
      end loop;

      --  Ellipse and top line intersection pixel

      CAL := CAR;

      Coverage    := GX_Integer (CAL - BT);
      Area        := GX_Integer (ELR + 1 - SCL) * Coverage;
      Accumulated := @ + Coverage;

      Gray := Grid_One * Accumulated - Area / 2;
      Gray := @ * 255 / (Grid_One * Grid_One);

      Fill_Span (SC, SR, 1, Grayscale (Gray));

      SC  := @ + 1;
      SCL := @ + Grid_One;
      SCR := @ + Grid_One;

      --  Top line

      loop
         exit when SCR > ERL;

         Area := 0;
         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));

         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;
      end loop;

      --  Top line and top-right ellipse intersection

      CAR := Coordinate_Y (ETR, SCR);

      Coverage    := GX_Integer (BT - CAR);
      Area        := (GX_Integer (ERL + 1 - SCL) + Grid_One) * Coverage;
      Accumulated := @ + Coverage;

      Gray := Grid_One * Accumulated - Area / 2;
      Gray := @ * 255 / (Grid_One * Grid_One);

      Fill_Span (SC, SR, 1, Grayscale (Gray));

      SC  := @ + 1;
      SCL := @ + Grid_One;
      SCR := @ + Grid_One;

      loop
         exit when SCR > ERR;

         CAL := CAR;
         CAR := Coordinate_Y (ETR, SCR);

         TR
           (ST       => SRT,
            SR       => SCR,
            SB       => SRB,
            SL       => SCL,
            ET       => ERL,
            ER       => CAR,
            EB       => ERR,
            EL       => CAL,
            Area     => Area,
            Coverage => Accumulated);

         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));

         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;
      end loop;

      CAL := CAR;
      Coverage    := GX_Integer (CAL - (SRB));
      Area        := GX_Integer (ERR - SCL) * Coverage;
      Accumulated := @ + Coverage;

      Gray := Grid_One * Accumulated - Area / 2;
      Gray := @ * 255 / (Grid_One * Grid_One);

      Fill_Span (SC, SR, 1, Grayscale (Gray));

      --  Vertical part of the top corner's ellipse

      SR  := @ + 1;
      SRT := @ + Grid_One;
      SRB := @ + Grid_One;
      Accumulated := 0;

      Outer: loop
         ELR := ELL;
         ELL := Coordinate_X (ETL, SRB);
         ERL := ERR;
         ERR := Coordinate_X (ETR, SRB);

         exit when SRB > ETL.Center_Y;
         --  XXX just to prevent exception and failure

         SC  := Device_Pixel (ELL);
         SCL := Lower (SC);
         SCR := SCL + Grid_One;

         --  First pixel, ellipse

         CAL := Grid_Coordinate'Last;
         CAR := Coordinate_Y (ETL, SCR);

         TL
           (ST       => SRT,
            SR       => SCR,
            SB       => SRB,
            SL       => SCL,
            ET       => ELR,
            ER       => CAR,
            EB       => ELL,
            EL       => CAL,
            Area     => Area,
            Coverage => Accumulated);

         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));

         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;

         --  Continuation of the ellipse

         loop
            exit when ELR <= SCL;

            CAL := CAR;
            CAR := Coordinate_Y (ETL, SCR);

            TL
              (ST       => SRT,
               SR       => SCR,
               SB       => SRB,
               SL       => SCL,
               ET       => ELR,
               ER       => CAR,
               EB       => ELL,
               EL       => CAL,
               Area     => Area,
               Coverage => Accumulated);

            Gray := Grid_One * Accumulated - Area / 2;
            Gray := @ * 255 / (Grid_One * Grid_One);

            Fill_Span (SC, SR, 1, Grayscale (Gray));

            SC  := @ + 1;
            SCL := @ + Grid_One;
            SCR := @ + Grid_One;
         end loop;

         --  Inner area

         loop
            exit when SCR > ERL;

            Area := 0;
            Gray := Grid_One * Accumulated - Area / 2;
            Gray := @ * 255 / (Grid_One * Grid_One);

            --  XXX Grayscale should be 255 always!

            Fill_Span (SC, SR, 1, Grayscale (Gray));

            SC  := @ + 1;
            SCL := @ + Grid_One;
            SCR := @ + Grid_One;
         end loop;

         --  First pixel of the top right ellipse at current scanline

         CAL := Coordinate_Y (ETR, SCL);
         --  CAL := Grid_Coordinate'First;
         CAR :=
           (if SCR < BR
              then Coordinate_Y (ETR, SCR)
              else Grid_Coordinate'Last);

         TR
           (ST       => SRT,
            SR       => SCR,
            SB       => SRB,
            SL       => SCL,
            ET       => ERL,
            ER       => CAR,
            EB       => ERR,
            EL       => CAL,
            Area     => Area,
            Coverage => Accumulated);

         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));

         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;

         loop
            exit when ERR <= SCL;

            CAL := CAR;
            CAR :=
              (if SCR < BR
                 then Coordinate_Y (ETR, SCR)
                 else Grid_Coordinate'Last);

            TR
              (ST       => SRT,
               SR       => SCR,
               SB       => SRB,
               SL       => SCL,
               ET       => ERL,
               ER       => CAR,
               EB       => ERR,
               EL       => CAL,
               Area     => Area,
               Coverage => Accumulated);

            Gray := Grid_One * Accumulated - Area / 2;
            Gray := @ * 255 / (Grid_One * Grid_One);

            Fill_Span (SC, SR, 1, Grayscale (Gray));

            SC  := @ + 1;
            SCL := @ + Grid_One;
            SCR := @ + Grid_One;
         end loop;

         SR  := @ + 1;
         SRT := @ + Grid_One;
         SRB := @ + Grid_One;
         Accumulated := 0;
      end loop Outer;

      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y - 3*Grid_One)),
      --     Device_Pixel (Y - 3*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y - 2*Grid_One)),
      --     Device_Pixel (Y - 2*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y - Grid_One)),
      --     Device_Pixel (Y - Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y)),
      --     Device_Pixel (Y),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + Grid_One)),
      --     Device_Pixel (Y + Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 2*Grid_One)),
      --     Device_Pixel (Y + 2*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 3*Grid_One)),
      --     Device_Pixel (Y + 3*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 4*Grid_One)),
      --     Device_Pixel (Y + 4*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 5*Grid_One)),
      --     Device_Pixel (Y + 5*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 6*Grid_One)),
      --     Device_Pixel (Y + 6*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (Ellipse_X (A, B, Y + 7*Grid_One)),
      --     Device_Pixel (Y + 7*Grid_One),
      --     1, 128);
      --  Fill_Span
      --    (Device_Pixel (X),
      --     Device_Pixel (Ellipse_Y (A, B, X)),
      --     1, 128);
   end Rasterize_CSS_Border;

   ------------------------
   -- Set_Rendering_Area --
   ------------------------

   procedure Set_Rendering_Area
     (Top    : GFX.Rasteriser.Device_Pixel_Index;
      Left   : GFX.Rasteriser.Device_Pixel_Index;
      Right  : GFX.Rasteriser.Device_Pixel_Index;
      Bottom : GFX.Rasteriser.Device_Pixel_Index) is
   begin
      null;
   end Set_Rendering_Area;

end GFX.Rasteriser.CSS_Borders;
