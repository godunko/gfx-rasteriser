--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with PPM;

package body Test_Support is

   use type GFX.Drawing.Device_Pixel_Index;

   Current_Y : GFX.Drawing.Device_Pixel_Index :=
     GFX.Drawing.Device_Pixel_Index'First;

   ------------------
   -- Set_PPM_Span --
   ------------------

   procedure Set_PPM_Span
     (X         : GFX.Drawing.Device_Pixel_Index;
      Y         : GFX.Drawing.Device_Pixel_Index;
      Width     : GFX.Drawing.Device_Pixel_Count;
      Luminance : GFX.Drawing.Grayscale)
   is
      Color : constant GFX.RGBA8888 :=
        GFX.To_RGBA (R => Luminance, G => Luminance, B => Luminance, A => 255);

   begin
      for C in X .. X + Width - 1 loop
         PPM.Set_Pixel (C, Y, Color);
      end loop;
   end Set_PPM_Span;

   ----------------
   -- Print_Span --
   ----------------

   procedure Print_Span
     (X         : GFX.Drawing.Device_Pixel_Index;
      Y         : GFX.Drawing.Device_Pixel_Index;
      Width     : GFX.Drawing.Device_Pixel_Count;
      Luminance : GFX.Drawing.Grayscale) is
   begin
      if Y /= Current_Y then
         Current_Y := Y;
         New_Line;
         Put (Trim (GFX.Drawing.Device_Pixel_Index'Image (Y), Both));
         Put (":");
      end if;

      Put ("  ");
      Put (Trim (GFX.Drawing.Device_Pixel_Index'Image (X), Both));
      Put ('/');
      Put (Trim (GFX.Drawing.Device_Pixel_Index'Image (Width), Both));
      Put (" [");
      Put (Trim (GFX.Drawing.Grayscale'Image (Luminance), Both));
      Put (']');
   end Print_Span;

end Test_Support;
