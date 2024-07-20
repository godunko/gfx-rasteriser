--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with Ada.Unchecked_Conversion;

package body GFX.Implementation.Fixed_Types is

   use Interfaces;

   Fixed_6_Scale    : constant  := 2.0 ** 6;
   Fixed_16_Scale   : constant  := 2.0 ** 16;
   Fixed_16_Offset  : constant  := -(2.0 ** 15);
   Fixed_6_16_Scale : constant GX_Integer := 2 ** 10;
   --  Scale to convert Fixed_6 to Fixed_16 values.
   Fixed_6_16_Offset : constant := -(2 ** 15);

   function To_Integer is
     new Ada.Unchecked_Conversion (GX_Unsigned, GX_Integer);
   function To_Unsigned is
     new Ada.Unchecked_Conversion (Fixed_16, GX_Unsigned);
   function To_Fixed_16 is
     new Ada.Unchecked_Conversion (GX_Unsigned, Fixed_16);

   ---------
   -- "<" --
   ---------

   overriding function "<"
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
   begin
      return GX_Integer (Left) < GX_Integer (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   overriding function "<="
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
   begin
      return GX_Integer (Left) <= GX_Integer (Right);
   end "<=";

   ---------
   -- ">" --
   ---------

   overriding function ">"
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
   begin
      return GX_Integer (Left) > GX_Integer (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   overriding function ">="
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
   begin
      return GX_Integer (Left) >= GX_Integer (Right);
   end ">=";

   ---------
   -- "+" --
   ---------

   overriding function "+"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (Integer (Left) + Integer (Right));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (-GX_Integer (Right));
   end "-";

   ---------
   -- "-" --
   ---------

   overriding function "-"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (Integer (Left) - Integer (Right));
   end "-";

   ---------
   -- "*" --
   ---------

   overriding function "*"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
   begin
      return
        Fixed_16
          ((GX_Integer_2 (Left) * GX_Integer_2 (Right)) / (2 ** 16));
   end "*";

   ---------
   -- "*" --
   ---------

   function "*" (Left : GX_Integer; Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (GX_Integer_2 (Left) * GX_Integer_2 (Right));
   end "*";

   ---------
   -- "/" --
   ---------

   overriding function "/"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
   is
      L : constant GX_Integer_2 := GX_Integer_2 (Left) * (2 ** 16);
      R : constant GX_Integer_2 := GX_Integer_2 (Right);

   begin
      return Fixed_16 (L / R);
   end "/";

   ---------
   -- "/" --
   ---------

   function "/" (Left : Fixed_16; Right : GX_Integer) return Fixed_16 is
   begin
      return Fixed_16 (GX_Integer (Left) / Right);
   end "/";

   -------------------------
   -- Ceiling_Minus_Delta --
   -------------------------

   function Ceiling_Minus_Delta (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Unsigned (Item) or 16#FFFF#);
   end Ceiling_Minus_Delta;

   ----------------------------
   -- Distance_From_Previous --
   ----------------------------

   --  function Distance_From_Previous (Item : Fixed_16) return Fixed_16 is
   --     use type Unsigned;
   --
   --     function To_Unsigned is
   --       new Ada.Unchecked_Conversion (Fixed_16, Unsigned);
   --     function To_Fixed_16 is
   --       new Ada.Unchecked_Conversion (Unsigned, Fixed_16);
   --
   --  begin
   --     return To_Fixed_16 (To_Unsigned (Item) and 16#FFFF#);
   --  end Distance_From_Previous;

   ----------------------
   -- Distance_To_Next --
   ----------------------

   --  function Distance_To_Next (Item : Fixed_16) return Fixed_16 is
   --     use type Unsigned;
   --
   --     function To_Unsigned is
   --       new Ada.Unchecked_Conversion (Fixed_16, Unsigned);
   --     function To_Fixed_16 is
   --       new Ada.Unchecked_Conversion (Unsigned, Fixed_16);
   --
   --  begin
   --     return
   --       To_Fixed_16
   --         ((2**16 - (To_Unsigned (Item) and 16#FFFF#)) and 16#FFFF#);
   --  end Distance_To_Next;

   ----------------------
   -- Divide_Saturated --
   ----------------------

   function Divide_Saturated
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
   is
      L : constant GX_Integer_2 := GX_Integer_2 (Left) * (2 ** 16);
      R : constant GX_Integer_2 := GX_Integer_2 (Right);
      D : GX_Integer_2;

   begin
      if Right = 0 then
         if Left < 0 then
            return Fixed_16 (GX_Integer'First);

         else
            return Fixed_16 (GX_Integer'Last);
         end if;
      end if;

      D  := L / R;

      if D < GX_Integer_2 (GX_Integer'First) then
         return Fixed_16 (GX_Integer'First);

      elsif D > GX_Integer_2 (GX_Integer'Last) then
         return Fixed_16 (GX_Integer'Last);

      else
         return Fixed_16 (D);
      end if;
   end Divide_Saturated;

   -----------
   -- Floor --
   -----------

   function Floor (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Unsigned (Item) and not 16#FFFF#);
   end Floor;

   ----------------
   -- Fractional --
   ----------------

   function Fractional (Item : Fixed_16) return Fixed_16 is
   begin
      --  if GX_Integer (Item) >= 0 then
         return To_Fixed_16 (To_Unsigned (Item) and 16#FFFF#);
      --
      --  else
      --     return
      --       To_Fixed_16 ((To_Unsigned (Item) and 16#FFFF#) or 16#FFFF_0000#);
      --  end if;
   end Fractional;

   --------------
   -- Integral --
   --------------

   function Integral (Item : Fixed_16) return GX_Integer is
   begin
      return To_Integer (Shift_Right_Arithmetic (To_Unsigned (Item), 16));
   end Integral;

   ----------------------
   -- Is_Equal_Fixed_6 --
   ----------------------

   function Is_Equal_Fixed_6
     (Left  : GFX.Drawing.Device_Pixel_Coordinate;
      Right : GFX.Drawing.Device_Pixel_Coordinate) return Boolean is
   begin
      return To_Fixed_6 (Left) = To_Fixed_6 (Right);
      --  ??? Can it be optimized like:
      --  return Fixed_6 ((Left - Right) * Fixed_6_Multiplier) = 0;
      --  return (Left - Right) * Fixed_6_Multiplier < 1.0;
   end Is_Equal_Fixed_6;

   -------------------
   -- Left_Coverage --
   -------------------

   function Left_Coverage (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Unsigned (Item) and 16#FFFF#) + 1;
   end Left_Coverage;

   ---------
   -- Max --
   ---------

   function Max (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (GX_Integer'Max (GX_Integer (Left), GX_Integer (Right)));
   end Max;

   ---------
   -- Min --
   ---------

   function Min (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
   begin
      return Fixed_16 (GX_Integer'Min (GX_Integer (Left), GX_Integer (Right)));
   end Min;

   -----------------------
   -- Multiply_Coverage --
   -----------------------

   function Multiply_Coverage
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
   is
      L  : constant GX_Unsigned := To_Unsigned (Left);
      LH : constant GX_Unsigned := Shift_Right (L, 16);
      LL : constant GX_Unsigned := L and 16#FFFF#;

      R  : constant GX_Unsigned := To_Unsigned (Right);
      RH : constant GX_Unsigned := Shift_Right (R, 16);
      RL : constant GX_Unsigned := R and 16#FFFF#;

   begin
      return
        To_Fixed_16
          (LH * RH * 2**16 + LH * RL + LL * RH + (LL * RL) / 2**16);
   end Multiply_Coverage;

   --------------------
   -- Right_Coverage --
   --------------------

   function Right_Coverage (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (2**16 - (To_Unsigned (Item) and 16#FFFF#));
   end Right_Coverage;

   ----------------
   -- To_Fixed_6 --
   ----------------

   function To_Fixed_6
     (Item : GFX.Drawing.Device_Pixel_Coordinate) return Fixed_6 is
   begin
      return Fixed_6 (Item * Fixed_6_Scale);
   end To_Fixed_6;

   -----------------
   -- To_Fixed_16 --
   -----------------

   function To_Fixed_16
     (Item : GFX.Drawing.Device_Pixel_Coordinate) return Fixed_16 is
   begin
      return Fixed_16 (Item * Fixed_16_Scale - Fixed_16_Offset);
   end To_Fixed_16;

   -----------------
   -- To_Fixed_16 --
   -----------------

   function To_Fixed_16 (Item : Fixed_6) return Fixed_16 is
   begin
      return
        Fixed_16 (GX_Integer (Item) * Fixed_6_16_Scale) - Fixed_6_16_Offset;
   end To_Fixed_16;

   ------------------
   -- To_Grayscale --
   ------------------

   function To_Grayscale
     (Item : Fixed_16) return GFX.Drawing.Grayscale is
   begin
      return
        GFX.Drawing.Grayscale
          (Shift_Right (To_Unsigned (Item) * 255, 16));
   end To_Grayscale;

end GFX.Implementation.Fixed_Types;
