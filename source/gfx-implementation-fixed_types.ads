--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package contains type declarations and operations for binary fixed
--  point types with 6 and 16 bits of the precision used for rasterizing.
--
--  They use Integer_32 as base type, which is enough to support displays with
--  reasonable size for modern 32-bit MCUs. On 64bit systems Integer_64 can be
--  used as base type to enhance range of supported display resolutions, and/or
--  improve performance.

pragma Restrictions (No_Elaboration_Code);

with Interfaces;

with GFX.Drawing;

package GFX.Implementation.Fixed_Types
  with Pure
is

   subtype Integer is Interfaces.Integer_32;

   type Fixed_6 is private;

   type Fixed_16 is private;

   One : constant Fixed_16;

   function To_Fixed_6
     (Item : GFX.Drawing.Device_Pixel_Coordinate) return Fixed_6;
   --  Convert given floating point value to binary fixed point value with 6
   --  bits of precision.

   function "+" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

   function "-" (Left : Fixed_16; Right : Fixed_16) return Fixed_16;

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

   function Integral (Item : Fixed_16) return Integer;
   --  Returns integral part of the given value (round toward zero, truncation).

   function Multiply_Coverage
     (Left : Fixed_16; Right : Fixed_16) return Fixed_16;
   --  Multiply two given fixed point values and return result rounded toward
   --  zero.
   --
   --  This subprogram is intended to be used for multiplication of the coverage
   --  values, thus arguments must be in 0.0 .. 1.0 range.

   function To_Grayscale (Item : Fixed_16) return GFX.Drawing.Grayscale;
   --  Converts given fixed point value in range 0.0 .. 1.0 into grayscale value
   --  in range 0 .. 255.

private

   type Fixed_6 is new Interfaces.Integer_32;

   type Fixed_16 is new Interfaces.Integer_32;

   One : constant Fixed_16 := 2**16;

   subtype Unsigned is Interfaces.Unsigned_32;
   pragma Assert (Unsigned'Size = Fixed_6'Size);
   pragma Assert (Unsigned'Size = Fixed_16'Size);
   --  Unsigned subtype to use for bit-wise operation.

   pragma Assert (Integer'Size = Fixed_6'Size);
   pragma Assert (Integer'Size = Fixed_16'Size);
   --  Assert size of the Integer type.

end GFX.Implementation.Fixed_Types;
