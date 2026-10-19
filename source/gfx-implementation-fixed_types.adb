--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

package body GFX.Implementation.Fixed_Types is

   use Interfaces;

   Fixed_6_Scale       : constant := 2.0 ** 6;
   Fixed_16_Scale_R    : constant := 2.0 ** 16;
   Fixed_16_Scale_Mask : constant := 2 ** 16 - 1;
   Fixed_6_16_Scale    : constant GX_Integer := 2 ** 10;
   --  Scale to convert Fixed_6 to Fixed_16 values.
   Fixed_6_16_Offset   : constant := -(2 ** 15);

   function To_Integer is
     new Ada.Unchecked_Conversion (GX_Unsigned, GX_Integer);
   function To_Unsigned is
     new Ada.Unchecked_Conversion (Fixed_16, GX_Unsigned);
   function To_Fixed_16 is
     new Ada.Unchecked_Conversion (GX_Unsigned, Fixed_16);

   -----------
   -- Ghost --
   -----------

   package body Ghost is

      -----------------
      -- Is_In_Range --
      -----------------

      function Is_In_Range
        (Value : Fixed_16;
         Lower : Fixed_16;
         Upper : Fixed_16) return Boolean is (Value in Lower .. Upper);

      ------------------
      -- Is_Saturated --
      ------------------

      function Is_Saturated
        (Value : GX_Integer_2; Result : Fixed_16) return Boolean is
        (GX_Integer_2 (Result) = GX_Integer_2'Max
           (GX_Integer_2 (Fixed_16_First), GX_Integer_2'Min
              (GX_Integer_2 (Fixed_16_Last), Value)));

      ------------------------------------
      -- Is_Truncated_Saturated_Product --
      ------------------------------------

      function Is_Truncated_Saturated_Product
        (Left, Right, Result : Fixed_16) return Boolean is
        (Is_Saturated
           ((GX_Integer_2 (Left) * GX_Integer_2 (Right)) / 2 ** 16,
            Result));

      -------------------------------------
      -- Is_Truncated_Saturated_Quotient --
      -------------------------------------

      function Is_Truncated_Saturated_Quotient
        (Left, Right, Result : Fixed_16) return Boolean is
        (if Right = Zero then
           (if Left < Zero then Result = Fixed_16_First
            elsif Left > Zero then Result = Fixed_16_Last
            else False)
         else
           Is_Saturated
             ((GX_Integer_2 (Left) * 2 ** 16) / GX_Integer_2 (Right),
              Result));
      --  Integer division truncates toward zero. Widening before scaling
      --  or multiplication keeps all intermediate values representable.

   end Ghost;

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

         elsif Left = 0 then
            raise Constraint_Error with "saturated divide 0/0";

         else
            return Fixed_16 (GX_Integer'Last);
         end if;
      end if;

      if ((To_Unsigned (Left) or To_Unsigned (Right)) and 16#3FF#) = 0
        and GX_Integer (Left) in -(2 ** 25 - 1) .. 2 ** 25 - 1
      then
         --  Both operands are on the 1/64 subpixel grid (it is the case of
         --  the snapped coordinates) and the dividend is less than 512
         --  pixels. Scaling both operands down by 2 ** 10 gives the same
         --  quotient, and the scaled dividend fits into GX_Integer, so the
         --  native division can be used instead of the much slower 64-bit
         --  one. The result is always in range.

         declare
            Dividend : constant GX_Integer := GX_Integer (Left) / 2 ** 10;
            Divisor  : constant GX_Integer := GX_Integer (Right) / 2 ** 10;

         begin
            return Fixed_16 (GX_Integer'(Dividend * 2 ** 16 / Divisor));
         end;
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
      return To_Fixed_16 (To_Unsigned (Item) and not Fixed_16_Scale_Mask);
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
     (Left  : GFX.Rasteriser.Device_Pixel_Coordinate;
      Right : GFX.Rasteriser.Device_Pixel_Coordinate) return Boolean is
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

   ---------------------------
   -- Multiply_Divide_Floor --
   ---------------------------

   procedure Multiply_Divide_Floor
     (Left        : Fixed_16;
      Numerator   : GX_Integer;
      Correction  : GX_Integer;
      Denominator : GX_Integer;
      Quotient    : out Fixed_16;
      Remainder   : out GX_Integer)
   is
      Small_Numerator : constant := 2 ** 15;
      --  With abs Left <= 1.0 and abs Correction <= abs Numerator, numerators
      --  less than this value allow to use native 32-bit arithmetic.

      Q : GX_Integer;

   begin
      if abs Numerator < Small_Numerator then
         --  The dividend fits into GX_Integer. Native arithmetic is much
         --  faster than 64-bit one on 32-bit CPUs.

         declare
            Dividend : constant GX_Integer :=
              GX_Integer (Left) * Numerator + Correction;

         begin
            Q         := Dividend / Denominator;
            Remainder := Dividend rem Denominator;
         end;

      else
         declare
            Dividend : constant GX_Integer_2 :=
              GX_Integer_2 (Left) * GX_Integer_2 (Numerator)
                + GX_Integer_2 (Correction);
            Q_2      : constant GX_Integer_2 :=
              Dividend / GX_Integer_2 (Denominator);

         begin
            if Q_2 not in GX_Integer_2 (GX_Integer'First + 1)
                            .. GX_Integer_2 (GX_Integer'Last)
            then
               Quotient  :=
                 (if Q_2 < 0
                  then Fixed_16 (GX_Integer'First + 1)
                  else Fixed_16 (GX_Integer'Last));
               Remainder := 0;

               return;
            end if;

            Q         := GX_Integer (Q_2);
            Remainder :=
              GX_Integer (Dividend - Q_2 * GX_Integer_2 (Denominator));
         end;
      end if;

      if Remainder < 0 then
         Q         := Q - 1;
         Remainder := Remainder + Denominator;
      end if;

      Quotient := Fixed_16 (Q);
   end Multiply_Divide_Floor;

   ------------------------
   -- Multiply_Saturated --
   ------------------------

   function Multiply_Saturated
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
   is
      Aux : constant GX_Integer_2 :=
        (GX_Integer_2 (Left) * GX_Integer_2 (Right)) / (2 ** 16);

   begin
      if Aux < GX_Integer_2 (GX_Integer'First) then
         return Fixed_16 (GX_Integer'First);

      elsif Aux > GX_Integer_2 (GX_Integer'Last) then
         return Fixed_16 (GX_Integer'Last);

      else
         return Fixed_16 (Aux);
      end if;
   end Multiply_Saturated;

   ---------------------
   -- Multiply_Scaled --
   ---------------------

   function Multiply_Scaled
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
   is
      pragma Suppress (Range_Check);
      pragma Suppress (Overflow_Check);
      --  Range check is impossible due to precondition on `Right` parameter.

   begin
      return
        Fixed_16 ((GX_Integer_2 (Left) * GX_Integer_2 (Right)) / (2 ** 16));
   end Multiply_Scaled;

   ------------------
   -- Pixel_Bounds --
   ------------------

   procedure Pixel_Bounds
     (Item  : Fixed_16;
      Lower : out Fixed_16;
      Upper : out Fixed_16) is
   begin
      Lower := Pixel_Lower_Bound (Item);
      Upper := Pixel_Upper_Bound (Item);
   end Pixel_Bounds;

   -----------------------
   -- Pixel_Lower_Bound --
   -----------------------

   function Pixel_Lower_Bound
     (Item : GFX.Rasteriser.Device_Pixel_Index) return Fixed_16 is
   begin
      return Fixed_16 (Item * GX_Integer (Fixed_16_Scale_I));
   end Pixel_Lower_Bound;

   -----------------------
   -- Pixel_Lower_Bound --
   -----------------------

   function Pixel_Lower_Bound (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Unsigned (Item) and not Fixed_16_Scale_Mask);
   end Pixel_Lower_Bound;

   -----------------------
   -- Pixel_Upper_Bound --
   -----------------------

   function Pixel_Upper_Bound (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Unsigned (Item) or Fixed_16_Scale_Mask);
   end Pixel_Upper_Bound;

   --------------------
   -- Right_Coverage --
   --------------------

   function Right_Coverage (Item : Fixed_16) return Fixed_16 is
   begin
      return To_Fixed_16 (2**16 - (To_Unsigned (Item) and 16#FFFF#));
   end Right_Coverage;

   ----------------------
   -- Snap_To_Subpixel --
   ----------------------

   function Snap_To_Subpixel
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_16 is
      Scaled : constant GX_Real := Item * 64.0;
      Value  : GX_Integer := GX_Integer (GX_Real'Truncation (Scaled));
      --  Conversion of the truncated value is done by the single hardware
      --  instruction, while GX_Real'Floor is implemented by the run-time
      --  library.

   begin
      if GX_Real (Value) > Scaled then
         --  Truncation rounds negative values toward zero; adjust to round
         --  toward negative infinity.

         Value := Value - 1;
      end if;

      return Fixed_16 (Value * GX_Integer'(16#400#));
   end Snap_To_Subpixel;

   ---------------------------
   -- Snap_To_Subpixel_Grid --
   ---------------------------

   function Snap_To_Subpixel_Grid
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_16 is
   begin
      return To_Fixed_16 (To_Fixed_6 (Item));
   end Snap_To_Subpixel_Grid;

   ----------------
   -- To_Fixed_6 --
   ----------------

   function To_Fixed_6
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_6 is
   begin
      return Fixed_6 (Item * Fixed_6_Scale);
   end To_Fixed_6;

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
     (Item : Fixed_16) return GFX.Rasteriser.Grayscale is
   begin
      return
        GFX.Rasteriser.Grayscale
          (Shift_Right (To_Unsigned (Item) * 255, 16));
   end To_Grayscale;

end GFX.Implementation.Fixed_Types;
