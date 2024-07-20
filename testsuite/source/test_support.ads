--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Drawing;

package Test_Support is

   Green : constant GFX.RGBA8888;

   procedure Print_Span
     (X         : GFX.Drawing.Device_Pixel_Index;
      Y         : GFX.Drawing.Device_Pixel_Index;
      Width     : GFX.Drawing.Device_Pixel_Count;
      Luminance : GFX.Drawing.Grayscale);
   --  Output span's information to standard output stream.

   procedure Set_PPM_Span
     (X         : GFX.Drawing.Device_Pixel_Index;
      Y         : GFX.Drawing.Device_Pixel_Index;
      Width     : GFX.Drawing.Device_Pixel_Count;
      Luminance : GFX.Drawing.Grayscale);
   --  Fill PPM according to given information, without blending.

private

   Green : constant GFX.RGBA8888 :=
     GFX.To_RGBA (R => 0, G => 255, B => 0, A => 0);

end Test_Support;
