--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with Ada.Long_Float_Text_IO; use Ada.Long_Float_Text_IO;

package body GFX.Drawing.Debug is

   -----------
   -- Image --
   -----------

   function Image
     (Item : GFX.Implementation.Fixed_Types.Fixed_16) return String
   is
      Integer_Value  : GFX.GX_Integer  with Address => Item'Address;
      Unsigned_Value : GFX.GX_Unsigned with Address => Item'Address;
      Fraction       : GFX.GX_Unsigned := Unsigned_Value and 16#FFFF#;
      Buffer         : String (1 .. 16);

   begin
      Put (Buffer, Long_Float (Integer_Value) / 2.0 ** 16, 5, 0);

      return
        Trim (Buffer, Both)
        & '('
        --  & Trim (GFX.GX_Integer'Image (Integer_Value), Both)
        --  & '/'
        & '.'
        & Trim (GFX.GX_Unsigned'Image (Fraction), Both)
        & ')';
   end Image;

end GFX.Drawing.Debug;
