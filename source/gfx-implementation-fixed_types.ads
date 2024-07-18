--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains type declarations and operations for binary fixed
--  point types with 6 and 16 bits of the precision used for rasterizing.
--
--  They use GX_Integer as base type, which is 32bit on 32bit CPUs, thus enough
--  to support displays with reasonable size for modern 32-bit MCUs. On 64bit
--  systems use of 64bit integer as base type enhance range of supported
--  display resolutions, and/or might improve performance.

pragma Restrictions (No_Elaboration_Code);

with GFX.Drawing;

package GFX.Implementation.Fixed_Types
  with Pure
is

   type Fixed_6 is private;

   type Fixed_16 is private;

   One : constant Fixed_16;

   Fixed_16_Delta : constant := 1.0 / (2.0 ** 16);

   function To_Fixed_6
     (Item : GFX.Drawing.Device_Pixel_Coordinate) return Fixed_6;
   --  Convert given floating point value to binary fixed point value with 6
   --  bits of precision.

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

   function Divide_Saturated
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Divide Left by Right, return GX_Integer'First/GX_Integer'Last on
   --  overflow.

   function Min (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function Max (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function Is_Equal_Fixed_6
     (Left  : GFX.Drawing.Device_Pixel_Coordinate;
      Right : GFX.Drawing.Device_Pixel_Coordinate) return Boolean;
   --  Return True when two given values is equal when converted to Fixed_6
   --  type.
   --
   --  ??? Float->Integer conversion is expensive on ARMv7M, can it be
   --  optimized ???

   function To_Fixed_16
     (Item : GFX.Drawing.Device_Pixel_Coordinate) return Fixed_16;
   --  Convert given floating point value to binary fixed point value with 16
   --  bits of precision with the coordinate system offset.
   --
   --  ??? Should it snap to subpixel grid first?

   function To_Fixed_16 (Item : Fixed_6) return Fixed_16;
   --  Convert given subpixel coordinate to binary fixed point value with
   --  coordinate system offset.

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

   function Integral (Item : Fixed_16) return GX_Integer;
   --  Returns integral part of the given value (round toward zero,
   --  truncation).

   function Fractional (Item : Fixed_16) return Fixed_16;
   --  Returns fractional part of the fixed point number.
   --
   --  ??? Not a fractional part, it is distance from the left pixel boundary.

   function Floor (Item : Fixed_16) return Fixed_16;

   function Ceiling_Minus_Delta (Item : Fixed_16) return Fixed_16;

   function Multiply_Coverage
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Multiply two given fixed point values and return result rounded toward
   --  zero.
   --
   --  This subprogram is intended to be used for multiplication of the
   --  coverage values, thus arguments must be in 0.0 .. 1.0 range.

   function To_Grayscale (Item : Fixed_16) return GFX.Drawing.Grayscale;
   --  Converts given fixed point value in range 0.0 .. 1.0 into grayscale
   --  value in range 0 .. 255.

private

   type Fixed_6 is new GFX.GX_Integer;

   type Fixed_16 is new GFX.GX_Integer;

   One : constant Fixed_16 := 2**16;

end GFX.Implementation.Fixed_Types;
