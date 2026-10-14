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
--  5. With a signed 32-bit underlying integer, `Fixed_16` has the range
--  -32_768.0 .. 32_768.0 - 2**(-16). The intended input pixel index range
--  is -16_384 .. 16_382 (-2**14 .. 2**14 - 2). Coordinates within these
--  pixels lie in [-16_384.0, 16_383.0).
--
--  This range ensures that differences of any two input coordinates fit
--  in `Fixed_16`. Rendering areas use half-open intervals [Left, Right)
--  and [Top, Bottom). Each boundary denotes the left/top edge of the pixel
--  at that coordinate. The value 16_383 is allowed only as the exclusive
--  right/bottom boundary; it does not designate a pixel included in the
--  rendering area. Subpixel coordinates beyond this boundary are not used.
--  Differences involving this boundary also fit in `Fixed_16`. No previous
--  pixel is needed at the lower boundary, so no additional lower-end margin
--  is reserved.
--
--  The application is responsible for clipping geometry before passing it
--  to the rasterizer. This input range makes coordinate subtraction safe,
--  but other intermediate values, including DDA increments and products,
--  must still fit in their respective types. These input limits are checked
--  by `Is_Valid_Device_Pixel`, not enforced by subtype constraints.
--  The exclusive boundary value 16_383 is not a valid input pixel and
--  is accepted by `Is_Valid_Upper_Bound` for right/bottom boundaries.

package GFX.Rasteriser
  with Pure
is

   subtype Device_Pixel_Coordinate is GX_Real;
   --  range -16_384.0 .. GX_Real'Pred (16_383.0);
   --
   --  Coordinate in device pixel units. Integral values correspond to pixel
   --  boundaries; pixel centers have a fractional part of 0.5.
   --
   --  The intended upper bound is the largest GX_Real value below
   --  16_383.0. The coordinate 16_383.0 is allowed only as the exclusive
   --  right/bottom boundary of a rendering area.

   subtype Device_Pixel_Index is GX_Integer;
   --  range -16_384 .. 16_384 - 2;
   --
   --  Index of the device's hardware pixel.
   --  The value 16_383 is allowed only as the exclusive right/bottom
   --  boundary of a rendering area, not as an included pixel index.

   subtype Device_Pixel_Count is Device_Pixel_Index
     range 0 .. Device_Pixel_Index'Last;

   subtype Grayscale is Interfaces.Unsigned_8;
   --  Grayscale level.

   function Is_Valid_Device_Pixel
     (Item : Device_Pixel_Coordinate) return Boolean is
        (Item in -16_384.0 .. GX_Real'Pred (16_383.0));

   function Is_Valid_Device_Pixel
     (Item : Device_Pixel_Index) return Boolean is
       (Item in -16_384 .. 16_384 - 2);

   function Is_Valid_Lower_Bound
     (Item : Device_Pixel_Coordinate) return Boolean is
       (Is_Valid_Device_Pixel (Item));
   --  Check a left/top boundary of a half-open rendering area.

   function Is_Valid_Lower_Bound
     (Item : Device_Pixel_Index) return Boolean is
       (Is_Valid_Device_Pixel (Item));
   --  Check a left/top boundary expressed as a pixel index.

   function Is_Valid_Upper_Bound
     (Item : Device_Pixel_Coordinate) return Boolean is
       (Item in -16_384.0 .. 16_383.0);
   --  Check an exclusive right/bottom boundary of a half-open rendering
   --  area, including the terminal coordinate 16_383.0.

   function Is_Valid_Upper_Bound
     (Item : Device_Pixel_Index) return Boolean is
       (Item in -16_384 .. 16_383);
   --  Check an exclusive right/bottom boundary expressed as a pixel index,
   --  including the terminal index 16_383.

   function Pixel_Lower_Bound
     (Item : Device_Pixel_Index) return Device_Pixel_Coordinate is
       (Device_Pixel_Coordinate (Item))
     with Pre => Is_Valid_Device_Pixel (Item);
   --  Return the initial coordinate of the given pixel.

   function Pixel_Upper_Bound
     (Item : Device_Pixel_Index) return Device_Pixel_Coordinate is
       (Device_Pixel_Coordinate'Pred (Device_Pixel_Coordinate (Item) + 1.0))
     with Pre => Is_Valid_Device_Pixel (Item);
   --  Return the largest representable coordinate below the initial
   --  coordinate of the next pixel.

   function Pixel_Lower_Subpixel
     (Item : Device_Pixel_Index) return Device_Pixel_Coordinate is
       (Device_Pixel_Coordinate (Item))
     with Pre => Is_Valid_Device_Pixel (Item);
   --  Return the coordinate of subpixel 0 of the given pixel.

   function Pixel_Upper_Subpixel
     (Item : Device_Pixel_Index) return Device_Pixel_Coordinate is
       (Device_Pixel_Coordinate (Item) + 63.0 / 64.0)
     with Pre => Is_Valid_Device_Pixel (Item);
   --  Return the coordinate of subpixel 63 of the given pixel.

end GFX.Rasteriser;
