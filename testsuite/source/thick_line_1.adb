--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Drawing.Primitive_Rasterizer;

with Test_Support;

procedure Thick_Line_1 is
begin
   GFX.Drawing.Primitive_Rasterizer.Rasterize_Line
     ((5.0, 5.0), (15.0, 15.0), 2.0, Test_Support.Print_Span'Access);
end Thick_Line_1;
