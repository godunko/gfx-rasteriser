--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Drawing;

package Test_Support is

   procedure Print_Span
     (X         : GFX.Drawing.Device_Pixel_Index;
      Y         : GFX.Drawing.Device_Pixel_Index;
      Width     : GFX.Drawing.Device_Pixel_Count;
      Luminance : GFX.Drawing.Grayscale);
   --  Output span's information to standard output stream.

end Test_Support;
