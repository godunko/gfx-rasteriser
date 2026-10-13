--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Rasteriser.Primitives;

with PPM;
with Test_Support;

procedure Thick_Line_1 is
begin
   GFX.Rasteriser.Primitives.Set_Rendering_Area (0, 0, 31, 19);

   GFX.Rasteriser.Primitives.Rasterize_Line
     ((5.0, 5.0), (15.0, 15.0), 2.0, Test_Support.Print_Span'Access);
   GFX.Rasteriser.Primitives.Rasterize_Line
     ((18.0, 5.0), (28.0, 10.0), 2.0, Test_Support.Print_Span'Access);

   PPM.Initialize (32, 20, Test_Support.Green);
   GFX.Rasteriser.Primitives.Rasterize_Line
     ((5.0, 5.0), (15.0, 15.0), 2.0, Test_Support.Set_PPM_Span'Access);
   GFX.Rasteriser.Primitives.Rasterize_Line
     ((18.0, 5.0), (28.0, 10.0), 2.0, Test_Support.Set_PPM_Span'Access);
   PPM.Save ("thick_line_1.ppm");
   PPM.Finalize;
end Thick_Line_1;
