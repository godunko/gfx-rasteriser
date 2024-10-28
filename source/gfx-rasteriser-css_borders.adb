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
      --  BT : F_18_6 := F_18_6 (Border.Edge.Top);
      --  BR : F_18_6 := F_18_6 (Border.Edge.Right);
      --  BB : F_18_6 := F_18_6 (Border.Edge.Bottom);
      --  BL : F_18_6 := F_18_6 (Border.Edge.Left);
      BT : Grid_Coordinate := Snap_To_Grid (Border.Edge.Top);
      BR : Grid_Coordinate := Snap_To_Grid (Border.Edge.Right);
      BB : Grid_Coordinate := Snap_To_Grid (Border.Edge.Bottom);
      BL : Grid_Coordinate := Snap_To_Grid (Border.Edge.Left);

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
      SCL : Grid_Coordinate;
      SCR : Grid_Coordinate;

      EIL : Grid_Coordinate with Export;
      EIR : Grid_Coordinate with Export;
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

      --  S  := Device_Pixel (Snap_To_Grid (Border.Edge.Top));
      SR  := Device_Pixel (BT);
      SRT := Lower (SR);
      SRB := Upper (SR);

      EIL := Coordinate_X (ETL, SRB + 1);
      EIR := Coordinate_X (ETL, BT);

      SC  := Device_Pixel (EIL);
      SCL := Lower (SC);
      SCR := SCL + Grid_One;

      CAR := Coordinate_Y (ETL, SCR);

      Coverage    := GX_Integer (SRB + 1 - CAR);
      Area        := GX_Integer ((EIL + 1 - SCL) + Grid_One) * Coverage;
      Accumulated := @ + Coverage;

      Gray := Grid_One / Accumulated + Area / 2;
      Gray := @ / Grid_One;

      Fill_Span (SC, SR, 1, Grayscale (Gray));

      loop
         SC  := @ + 1;
         SCL := @ + Grid_One;
         SCR := @ + Grid_One;

         exit when SCR > EIR;

         CAL := CAR;
         CAR := Coordinate_Y (ETL, SCR);

         Coverage    := GX_Integer (CAL - CAR);
         Area        := Grid_One * Coverage;
         Accumulated := @ + Coverage;

         Gray := Grid_One * Accumulated - Area / 2;
         Gray := @ * 255 / (Grid_One * Grid_One);

         Fill_Span (SC, SR, 1, Grayscale (Gray));
      end loop;

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
