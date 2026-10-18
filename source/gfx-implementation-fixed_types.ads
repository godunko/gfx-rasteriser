--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains type declarations and operations for binary fixed
--  point types with 6, 12 and 16 bits of the precision used for rasterizing.
--
--  They use GX_Integer as base type, which is 32bit on 32bit CPUs, thus enough
--  to support displays with reasonable size for modern 32-bit MCUs. On 64bit
--  systems use of 64bit integer as base type enhance range of supported
--  display resolutions, and/or might improve performance.

pragma Restrictions (No_Elaboration_Code);

private with Ada.Unchecked_Conversion;

with GFX.Rasteriser;

package GFX.Implementation.Fixed_Types
  with Pure
is

   type Fixed_6 is private;

   type Fixed_12 is private
     with Preelaborable_Initialization;

   type Fixed_16 is private
     with Preelaborable_Initialization;

   function Zero return Fixed_6;

   function One return Fixed_6;

   function Zero return Fixed_12;

   function Zero return Fixed_16;

   function One return Fixed_16;

   Fixed_16_Delta_Fixed : constant Fixed_16;
   Fixed_16_First       : constant Fixed_16;
   Fixed_16_Last        : constant Fixed_16;

   Fixed_16_Delta : constant := 1.0 / (2.0 ** 16);

   function To_Fixed_6
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_6;
   --  Convert given floating point value to binary fixed point value with 6
   --  bits of precision.

   function "+" (Left : Fixed_6; Right : Fixed_6) return Fixed_6;

   function "-" (Left : Fixed_6; Right : Fixed_6) return Fixed_6;

   function "*" (Left : GX_Integer; Right : Fixed_6) return Fixed_6;

   function "*" (Left : Fixed_6; Right : Fixed_6) return Fixed_12;

   function "+" (Left : Fixed_12; Right : Fixed_12) return Fixed_12;

   function "-" (Left : Fixed_12; Right : Fixed_12) return Fixed_12;

   function "*" (Left : GX_Integer; Right : Fixed_12) return Fixed_12;

   function "abs" (Right : Fixed_12) return Fixed_12;

   function "<=" (Left : Fixed_12; Right : Fixed_12) return Boolean;

   function "-" (Right : Fixed_16) return Fixed_16;

   function "<" (Left : Fixed_16; Right : Fixed_16) return Boolean;

   function "<=" (Left : Fixed_16; Right : Fixed_16) return Boolean;

   function ">" (Left : Fixed_16; Right : Fixed_16) return Boolean;

   function ">=" (Left : Fixed_16; Right : Fixed_16) return Boolean;

   function "+" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function "-" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function "*" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function "*" (Left : GX_Integer; Right : Fixed_16) return Fixed_16;

   function "/" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  ??? Should it be better to use floating point? Implementation requires
   --  integer type of GX_Integer'Size * 2 bits.

   function "/" (Left : Fixed_16; Right : GX_Integer) return Fixed_16;

   function Is_Equal_Fixed_6
     (Left  : GFX.Rasteriser.Device_Pixel_Coordinate;
      Right : GFX.Rasteriser.Device_Pixel_Coordinate) return Boolean;
   --  Return True when two given values is equal when converted to Fixed_6
   --  type.
   --
   --  ??? Float->Integer conversion is expensive on ARMv7M, can it be
   --  optimized ???

   function Snap_To_Subpixel
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_16;
   --  Convert given floating point value to binary fixed point value with 16
   --  bits of precision, rounded toward negative infinity to a 1/64 pixel
   --  boundary.

   function Snap_To_Subpixel_Grid
     (Item : GFX.Rasteriser.Device_Pixel_Coordinate) return Fixed_16;
   --  Convert given floating point value to binary fixed point value with 16
   --  bits of precision with the coordinate system offset.
   --
   --  ??? Should it snap to subpixel grid first?

   function Pixel_Lower_Bound
     (Item : GFX.Rasteriser.Device_Pixel_Index) return Fixed_16;
   --  Returns lower bound of the given pixel

   function To_Fixed_16 (Item : Fixed_6) return Fixed_16;
   --  Convert given subpixel coordinate to binary fixed point value with
   --  coordinate system offset.

   function To_Fixed_16
     (Item : GFX.Rasteriser.Device_Pixel_Index) return Fixed_16
     with Pre => Item in -32_768 .. 32_767;
   --  Convert index of the given device pixel into `Fixed_16` value.

   --  function Distance_From_Previous (Item : Fixed_16) return Fixed_16;
   --  --  Return distance from the previous integral value to the given value. When
   --  --  the given value is integral value it returns 0.
   --
   --  function Distance_To_Next (Item : Fixed_16) return Fixed_16;
   --  --  Return distance to the next integral value from the given value. When the
   --  --  given value is integral value it returns 0.

   function Left_Coverage (Item : Fixed_16) return Fixed_16;
   --  Returns coverage of the pixel at the left side from the given value.

   function Right_Coverage (Item : Fixed_16) return Fixed_16;
   --  Returns coverage of the pixel at the right side from the given value.

   function Integral (Item : Fixed_12) return GX_Integer;
   --  Return integral part, truncating toward zero.

   function Integral (Item : Fixed_16) return GX_Integer;
   --  Returns integral part of the given value (round toward zero,
   --  truncation).

   function Fractional (Item : Fixed_16) return Fixed_16;
   --  Returns fractional part of the fixed point number.
   --
   --  ??? Not a fractional part, it is distance from the left pixel boundary.

   function Floor (Item : Fixed_16) return Fixed_16;

   function Pixel_Lower_Bound (Item : Fixed_16) return Fixed_16;

   function Pixel_Upper_Bound (Item : Fixed_16) return Fixed_16;

   procedure Pixel_Bounds
     (Item  : Fixed_16;
      Lower : out Fixed_16;
      Upper : out Fixed_16);

   function Multiply_Coverage
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Multiply two given fixed point values and return result rounded toward
   --  zero.
   --
   --  This subprogram is intended to be used for multiplication of the
   --  coverage values, thus arguments must be in 0.0 .. 1.0 range.

   function To_Grayscale (Item : Fixed_16) return GFX.Rasteriser.Grayscale;
   --  Converts given fixed point value in range 0.0 .. 1.0 into grayscale
   --  value in range 0 .. 255.

   function Truncate (Item : Fixed_16) return Fixed_6;
   --  Reduce precision to six fractional bits, truncating toward zero.

   --  XXX Subprograms below are known used for "new" code with new coordinate
   --  system conventions. Other subprograms might be used too, need to be
   --  checked.
   --
   --  They are listed in alphabetical order intentionally.

   function "/" (Left : Fixed_12; Right : GX_Integer) return Fixed_12;

   function Divide_Saturated
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
     with
       Pre => Left /= Zero or Right /= Zero,
       --  The result of 0 / 0 is undefined; Constraint_Error is raised if
       --  precondition checks are disabled.
       Post =>
         Ghost.Is_Truncated_Saturated_Quotient
           (Left, Right, Divide_Saturated'Result);
   --  Return the quotient, truncated toward zero and saturated on overflow.
   --  Division by zero returns `Fixed_16'First` for a negative numerator and
   --  `Fixed_16'Last` for a positive numerator. Zero divided by zero violates
   --  the precondition; the body raises `Constraint_Error` if precondition
   --  checks are disabled.
   --
   --  The implementation uses 64-bit integer arithmetic.

   function Floor (Item : Fixed_16) return Fixed_6;

   function Floor (Item : Fixed_16) return GX_Integer;
   --  Return the greatest integer less than or equal to `Item`, rounding
   --  toward negative infinity. For example, 1.25 yields 1 and -1.25 yields
   --  -2; integral values are unchanged.

   function Max (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Return the larger of Left and Right.

   function Min (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Return the smaller of Left and Right.

   function Multiply_Saturated
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
     with Post =>
       Ghost.Is_Truncated_Saturated_Product
         (Left, Right, Multiply_Saturated'Result);
   --  Return the product, truncated toward zero and saturated on overflow.
   --
   --  The implementation uses 64-bit integer arithmetic.

   function Multiply_Scaled
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16
     with
       Pre => Ghost.Is_In_Range (Right, Zero, One),
       Post =>
         Ghost.Is_Truncated_Saturated_Product
           (Left, Right, Multiply_Scaled'Result);
   --  Return the product, truncated toward zero.
   --
   --  The implementation uses 64-bit integer arithmetic. With `Right` in the
   --  range `[0.0, 1.0]`, the intermediate product fits in 64 bits, and the
   --  scaled result fits in `Fixed_16` without saturation.

   pragma Inline (Floor);
   pragma Inline (Integral);
   pragma Inline (Multiply_Saturated);
   pragma Inline (Multiply_Scaled);
   pragma Inline (Pixel_Lower_Bound);

   package Ghost with Ghost is

      --  Predicates use widened integer arithmetic to specify truncation
      --  toward zero followed by saturation to the `Fixed_16` range.

      function Is_Truncated_Saturated_Product
        (Left, Right, Result : Fixed_16) return Boolean;

      function Is_Truncated_Saturated_Quotient
        (Left, Right, Result : Fixed_16) return Boolean;
      --  Return `False` for zero divided by zero: no normal result exists.

      function Is_In_Range
        (Value : Fixed_16;
         Lower : Fixed_16;
         Upper : Fixed_16) return Boolean;
      --  Check that given value is in given range, both bounds inclusive.

   end Ghost;

private

   type Fixed_6 is new GFX.GX_Integer;

   type Fixed_12 is new GFX.GX_Integer;

   type Fixed_16 is new GFX.GX_Integer;

   function Zero return Fixed_6 is (0);

   function Zero return Fixed_12 is (0);

   function Zero return Fixed_16 is (0);

   function One return Fixed_6 is (2 ** 6);

   function One return Fixed_16 is (2 ** 16);

   Fixed_16_Delta_Fixed : constant Fixed_16 := 1;
   Fixed_16_First       : constant Fixed_16 := Fixed_16'First;
   Fixed_16_Last        : constant Fixed_16 := Fixed_16'Last;
   Fixed_16_Scale_I     : constant := 2 ** 16;

   function As_GX_Integer is
     new Ada.Unchecked_Conversion (GX_Unsigned, GX_Integer);
   function As_GX_Unsigned is
     new Ada.Unchecked_Conversion (Fixed_16, GX_Unsigned);
   function As_GX_Unsigned is
     new Ada.Unchecked_Conversion (GX_Integer, GX_Unsigned);
   function As_Fixed_16 is
     new Ada.Unchecked_Conversion (GX_Unsigned, Fixed_16);

   function Truncate (Item : Fixed_16) return Fixed_6 is
     (Fixed_6 (GX_Integer (Item) / 2**10));

   overriding function "+"
     (Left : Fixed_6; Right : Fixed_6) return Fixed_6 is
       (Fixed_6 (GX_Integer (Left) + GX_Integer (Right)));

   overriding function "-"
     (Left : Fixed_6; Right : Fixed_6) return Fixed_6 is
       (Fixed_6 (GX_Integer (Left) - GX_Integer (Right)));

   function "/" (Left : Fixed_12; Right : GX_Integer) return Fixed_12 is
      (Fixed_12 (GX_Integer (Left) / Right));

   function "*" (Left : GX_Integer; Right : Fixed_6) return Fixed_6 is
     (Fixed_6 (Left * GX_Integer (Right)));

   function "*" (Left : Fixed_6; Right : Fixed_6) return Fixed_12 is
     (Fixed_12 (GX_Integer (Left) * GX_Integer (Right)));

   overriding function "+"
     (Left : Fixed_12; Right : Fixed_12) return Fixed_12 is
       (Fixed_12 (GX_Integer (Left) + GX_Integer (Right)));

   overriding function "-"
     (Left : Fixed_12; Right : Fixed_12) return Fixed_12 is
       (Fixed_12 (GX_Integer (Left) - GX_Integer (Right)));

   function "*" (Left : GX_Integer; Right : Fixed_12) return Fixed_12 is
     (Fixed_12 (Left * GX_Integer (Right)));

   overriding function "abs" (Right : Fixed_12) return Fixed_12 is
     (Fixed_12 (abs GX_Integer (Right)));

   overriding function "<="
     (Left : Fixed_12; Right : Fixed_12) return Boolean is
       (GX_Integer (Left) <= GX_Integer (Right));

   function Integral (Item : Fixed_12) return GX_Integer is
     (GX_Integer (Item) / 2**12);

   overriding function "<"
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
       (GX_Integer (Left) < GX_Integer (Right));

   overriding function "<="
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
       (GX_Integer (Left) <= GX_Integer (Right));

   overriding function ">"
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
       (GX_Integer (Left) > GX_Integer (Right));

   overriding function ">="
     (Left : Fixed_16; Right : Fixed_16) return Boolean is
       (GX_Integer (Left) >= GX_Integer (Right));

   overriding function "+"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
       (Fixed_16 (Integer (Left) + Integer (Right)));

   overriding function "-" (Right : Fixed_16) return Fixed_16 is
     (Fixed_16 (-GX_Integer (Right)));

   overriding function "-"
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
       (Fixed_16 (Integer (Left) - Integer (Right)));

   function Floor (Item : Fixed_16) return Fixed_6 is
     (Fixed_6
        (As_GX_Integer (Shift_Right_Arithmetic (As_GX_Unsigned (Item), 10))));

   function Floor (Item : Fixed_16) return GX_Integer is
     (As_GX_Integer (Shift_Right_Arithmetic (As_GX_Unsigned (Item), 16)));

   function Max (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
     (Fixed_16 (GX_Integer'Max (GX_Integer (Left), GX_Integer (Right))));

   function Min (Left : Fixed_16; Right : Fixed_16) return Fixed_16 is
     (Fixed_16 (GX_Integer'Min (GX_Integer (Left), GX_Integer (Right))));

   function To_Fixed_16
     (Item : GFX.Rasteriser.Device_Pixel_Index) return Fixed_16 is
       (As_Fixed_16
         (As_GX_Unsigned (Item) * GX_Unsigned (Fixed_16_Scale_I)));

end GFX.Implementation.Fixed_Types;
