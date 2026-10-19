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

   subtype Subpixel_Index is GFX.GX_Integer;
   --  Index of the 1/64 pixel cell. Subpixel N contains coordinates in
   --  [N / 64, (N + 1) / 64).

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

   function To_Subpixel
     (Item : GFX.Implementation.Fixed_Types.Fixed_16)
      return Subpixel_Index with Inline;
   --  Return the index of the subpixel containing the given coordinate,
   --  rounding toward negative infinity; for example, -0.25 belongs to
   --  subpixel -16 and -1/128 belongs to subpixel -1. For a coordinate on
   --  the 1/64 grid, and for a difference of such coordinates, the result
   --  is exact: it is the value in 1/64 units.

   package Ghost with Ghost is

      function Is_On_Subpixel_Grid
        (Item : GFX.Implementation.Fixed_Types.Fixed_16) return Boolean;
      --  Check that given value is a multiple of 1/64 pixel.

   end Ghost;

private

   use GFX.Implementation.Fixed_Types;

   function To_Pixel
     (Item : GFX.Implementation.Fixed_Types.Fixed_16)
      return GFX.Rasteriser.Device_Pixel_Index is (Floor (Item));

   function To_Subpixel
     (Item : GFX.Implementation.Fixed_Types.Fixed_16)
      return Subpixel_Index is (Floor_6 (Item));

end GFX.Implementation.Subpixel_Operations;
