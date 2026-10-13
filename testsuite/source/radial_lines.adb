--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Numerics.Generic_Elementary_Functions;

with GFX.Points;
with GFX.Rasteriser.Primitives;

with PPM;
with Test_Support;

procedure Radial_Lines is
   use GFX;
   use GFX.Rasteriser;

   package Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (GX_Real);

   Cell_Size    : constant Device_Pixel_Count := 128;
   Inner_Radius : constant GX_Real := 24.0;
   Outer_Radius : constant GX_Real := 60.0;
   Ray_Count    : constant := 32;

   --  Columns run from thin to thick; rows shift X and Y independently.
   Widths : constant array (Positive range <>) of GX_Real :=
     (0.25, 0.5, 1.0, 1.5, 2.0, 3.0);
   Offsets : constant array (Positive range <>) of GFX.Points.GF_Point :=
     ((0.0, 0.0), (0.5, 0.0), (0.0, 0.5), (0.5, 0.5));

   Image_Width : constant Device_Pixel_Count :=
     Cell_Size * Device_Pixel_Count (Widths'Length);
   Image_Height : constant Device_Pixel_Count :=
     Cell_Size * Device_Pixel_Count (Offsets'Length);
   Base_Center : constant GX_Real := GX_Real (Cell_Size) / 2.0 - 0.5;

   procedure Draw_Rays (Center : GFX.Points.GF_Point; Width : GX_Real);

   ---------------
   -- Draw_Rays --
   ---------------

   procedure Draw_Rays (Center : GFX.Points.GF_Point; Width : GX_Real) is
      Angle   : GX_Real;
      DX      : GX_Real;
      DY      : GX_Real;
      Point_A : GFX.Points.GF_Point;
      Point_B : GFX.Points.GF_Point;

   begin
      --  Include both axes, diagonals, and shallow and steep slopes in each
      --  quadrant. The rows exercise different subpixel positioning.
      --  Opposite rays naturally exercise opposite drawing directions.

      for Ray in 0 .. Ray_Count - 1 loop
         Angle := GX_Real (Ray) * (360.0 / GX_Real (Ray_Count));
         DX := Elementary_Functions.Cos (Angle, Cycle => 360.0);
         DY := Elementary_Functions.Sin (Angle, Cycle => 360.0);
         Point_A :=
           (Center.X + Inner_Radius * DX, Center.Y + Inner_Radius * DY);
         Point_B :=
           (Center.X + Outer_Radius * DX, Center.Y + Outer_Radius * DY);

         GFX.Rasteriser.Primitives.Rasterize_Line
           (Point_A, Point_B, Width, Test_Support.Set_PPM_Span'Access);
      end loop;
   end Draw_Rays;

begin
   GFX.Rasteriser.Primitives.Set_Rendering_Area
     (0, 0, Image_Width - 1, Image_Height - 1);

   PPM.Initialize (Image_Width, Image_Height, Test_Support.Green);

   for Row in Offsets'Range loop
      for Column in Widths'Range loop
         Draw_Rays
           ((GX_Real (Column - Widths'First) * GX_Real (Cell_Size)
               + Base_Center + Offsets (Row).X,
             GX_Real (Row - Offsets'First) * GX_Real (Cell_Size)
               + Base_Center + Offsets (Row).Y),
            Widths (Column));
      end loop;
   end loop;

   PPM.Save ("radial_lines.ppm");
   PPM.Finalize;
end Radial_Lines;
