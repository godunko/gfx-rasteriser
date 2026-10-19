--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;
pragma Restrictions (No_Elaboration_Code);

package body GFX.Rasteriser.Coverage is

   use GFX.Implementation.Fixed_Types;

   ---------
   -- Add --
   ---------

   procedure Add
     (Item   : in out Cell;
      Height : Fixed_6;
      Width  : Fixed_6) is
   begin
      Item.Coverage   := @ + Height;
      Item.Twice_Area := @ + Height * Width;
   end Add;

   ---------------
   -- Integrate --
   ---------------

   procedure Integrate
     (Accumulator : in out Fixed_6;
      Item        : Cell;
      Alpha       : out Grayscale)
   is
      Area : constant Fixed_12 :=
        (2 * Accumulator * One + Item.Twice_Area) / 2;

   begin
      Alpha       := Grayscale (Integral (255 * abs Area));
      Accumulator := @ + Item.Coverage;
   end Integrate;

end GFX.Rasteriser.Coverage;
