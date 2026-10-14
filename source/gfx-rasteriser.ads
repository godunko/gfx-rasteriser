--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  GFX use few types to represent device pixel coordinate/index/subindex.
--
--  1. Applications use the floating point type `Device_Pixel_Coordinate` for
--  display coordinates. The origin is the upper-left corner of pixel (0, 0).
--  X increases to the right and Y increases downward. Integer coordinates
--  lie on pixel boundaries; the center of pixel (X, Y) is (X + 0.5, Y + 0.5).
--
--  2. Pixel buffers use the integer type `Device_Pixel_Index` to index
--  individual pixels. Pixel (X, Y) occupies [X, X + 1) by [Y, Y + 1).
--
--  3. Each pixel is divided into 64x64 subpixels. `Fixed_6` is used to
--  represent coordinates on this subpixel grid. Subpixel indices within
--  a pixel range from 0 to 63 along each axis.
--
--  Relationship between these types is presented below.
--
--  16bit binary precision      |0                             65535|
--  6bit binary precision       |0                                63|
--  device pixel index          |                 0                 |
--  64x64 subpixel index        | 0 | 1 |...| 30| 31| 32|...| 62| 63|
--  device pixel coordinate   [0.0                                1.0)
--                 --- ---  0.0 +---+---+...+---+---+---+...+---+---+
--                      0       |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      1       |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                     ...      ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      30      |   |   |   |   |   |   |   |   |   |
--                  0  ---      +---+---+...+---+---+---+...+---+---+
--                      31      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      32      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                     ...      ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      62      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      63      |   |   |   |   |   |   |   |   |   |
--                 --- ---  1.0 +---+---+...+---+---+---+...+---+---+
--
--  To obtain the index of the pixel containing a `Device_Pixel_Coordinate`,
--  take its floor, including for negative coordinates: -0.25 belongs to
--  pixel -1. Ada's default floating point to integer conversion rounds to
--  the nearest integer and therefore cannot be used directly for this.
--
--  4. `Fixed_16`, with 16 fractional bits, is used internally to avoid
--  floating point operations and floating point to integer conversions on
--  code paths critical for performance. For DDA with a truncated increment
--  and no remainder correction, accumulated error is less than N / 65_536
--  pixels after N one-pixel steps. This bound is at most one 1/64 subpixel
--  for up to 1_024 steps; beyond that, error can exceed one subpixel.
--  The limit concerns the number of DDA steps, not the display resolution.
--  Even smaller errors can change coverage near subpixel grid boundaries.
--
--  5. Device pixel coordinates and indices supplied to the rasterizer must
--  be representable by `Fixed_16`. With a signed 32-bit underlying integer,
--  its range is -32_768.0 .. 32_768.0 - 2**(-16); integral pixel indices
--  therefore range from -32_768 to 32_767. `Device_Pixel_Coordinate` is
--  constrained to the GX_Real values within this range.
--
--  The application is responsible for clipping geometry before passing it
--  to the rasterizer. Input range constraints alone do not prevent overflow:
--  intermediate values stored as `Fixed_16`, including coordinate
--  differences and DDA increments, must also fit within its range.

package GFX.Rasteriser
  with Pure
is

   subtype Device_Pixel_Coordinate is GX_Real;
   --  range -32_768.0 .. GX_Real'Pred (32_768.0);
   --
   --  Coordinate in device pixel units. Integral values correspond to pixel
   --  boundaries; pixel centers have a fractional part of 0.5.
   --
   --  The upper bound is the largest GX_Real value below 32_768.0, since
   --  the exact Fixed_16 upper bound is not representable by GX_Real.

   subtype Device_Pixel_Index is GX_Integer;
   --  range -32_768 .. 32_768 - 1;
   --
   --  Index of the device's hardware pixel.

   subtype Device_Pixel_Count is Device_Pixel_Index
     range 0 .. Device_Pixel_Index'Last;

   subtype Grayscale is Interfaces.Unsigned_8;
   --  Grayscale level.

end GFX.Rasteriser;
