--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Draw into PPM (portable pixmap format) image and save it in file.

with GFX.Drawing;

package PPM is

   procedure Initialize
     (Width  : GFX.Drawing.Device_Pixel_Count;
      Height : GFX.Drawing.Device_Pixel_Count;
      Color  : GFX.RGBA8888 := GFX.To_RGBA (0, 0, 0, 0));

   procedure Finalize;

   procedure Set_Pixel
     (X     : GFX.Drawing.Device_Pixel_Index;
      Y     : GFX.Drawing.Device_Pixel_Index;
      Color : GFX.RGBA8888);

   function Get_Pixel
     (X : GFX.Drawing.Device_Pixel_Index;
      Y : GFX.Drawing.Device_Pixel_Index) return GFX.RGBA8888;

   procedure Save (File_Name : String);

end PPM;
