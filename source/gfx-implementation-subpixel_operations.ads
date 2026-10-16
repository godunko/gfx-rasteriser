--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides various operations with subpixel precision.

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Fixed_Types;
with GFX.Rasteriser;

package GFX.Implementation.Subpixel_Operations
  with Pure
is

   function Pixel_Upper_Bound
     (Item : GFX.Rasteriser.Device_Pixel_Index)
      return GFX.Implementation.Fixed_Types.Fixed_16
     with Pre => Item in -32_768 .. 32_766;
   --  Return the exclusive upper boundary of the given pixel (equal to
   --  inclusive lower bound of the next pixel).

   function To_Pixel
     (Item : GFX.Implementation.Fixed_Types.Fixed_16)
      return GFX.Rasteriser.Device_Pixel_Index;
   --  Return the index of the pixel containing the given coordinate,
   --  rounding toward negative infinity. Pixel N contains coordinates in
   --  [N, N + 1); for example, -0.25 belongs to pixel -1.

private

   use GFX.Implementation.Fixed_Types;

   function To_Pixel
     (Item : GFX.Implementation.Fixed_Types.Fixed_16)
      return GFX.Rasteriser.Device_Pixel_Index is (Floor (Item));

end GFX.Implementation.Subpixel_Operations;
