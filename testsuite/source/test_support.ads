--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Rasteriser;

package Test_Support is

   Green : constant GFX.RGBA8888;

   procedure Print_Span
     (X         : GFX.Rasteriser.Device_Pixel_Index;
      Y         : GFX.Rasteriser.Device_Pixel_Index;
      Width     : GFX.Rasteriser.Device_Pixel_Count;
      Luminance : GFX.Rasteriser.Grayscale);
   --  Output span's information to standard output stream.

   procedure Set_PPM_Span
     (X         : GFX.Rasteriser.Device_Pixel_Index;
      Y         : GFX.Rasteriser.Device_Pixel_Index;
      Width     : GFX.Rasteriser.Device_Pixel_Count;
      Luminance : GFX.Rasteriser.Grayscale);
   --  Fill PPM according to given information, without blending.

private

   Green : constant GFX.RGBA8888 :=
     GFX.To_RGBA (R => 0, G => 255, B => 0, A => 0);

end Test_Support;
