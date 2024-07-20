--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Drawing.Primitive_Rasterizer;

with PPM;
with Test_Support;

procedure Thick_Line_1 is
begin
   GFX.Drawing.Primitive_Rasterizer.Rasterize_Line
     ((5.0, 5.0), (15.0, 15.0), 2.0, Test_Support.Print_Span'Access);

   PPM.Initialize (20, 20, Test_Support.Green);
   GFX.Drawing.Primitive_Rasterizer.Rasterize_Line
     ((5.0, 5.0), (15.0, 15.0), 2.0, Test_Support.Set_PPM_Span'Access);
   PPM.Save ("thick_line_1.ppm");
   PPM.Finalize;
end Thick_Line_1;
