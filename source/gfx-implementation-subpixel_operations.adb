--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

package body GFX.Implementation.Subpixel_Operations is

   -----------------------
   -- Pixel_Upper_Bound --
   -----------------------

   function Pixel_Upper_Bound
     (Item : GFX.Rasteriser.Device_Pixel_Index) return Fixed_16 is
   begin
      return To_Fixed_16 (Item + 1);
   end Pixel_Upper_Bound;

   package body Ghost is

      -------------------------
      -- Is_On_Subpixel_Grid --
      -------------------------

      function Is_On_Subpixel_Grid (Item : Fixed_16) return Boolean is
        (To_Subpixel (Item) * (One / 64) = Item);
      --  To_Subpixel rounds toward negative infinity, so converting its
      --  result back gives Item only when Item is on the grid.

   end Ghost;

end GFX.Implementation.Subpixel_Operations;
