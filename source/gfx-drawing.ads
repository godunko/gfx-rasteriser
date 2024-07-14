--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  GFX use few types to represent device pixel coordinate/index/subindex.
--
--  1. Application sees display coordinates as value of the floating point
--  type Device_Pixel_Coordinate. Center of the pixel at (0, 0) coorinate is
--  an origin of the coordinate system.
--
--  2. Pixel buffers use integer type Device_Pixel_Index as coordinate of the
--  individual pixels.
--
--  3. Each pixel is divided on 64x64 subpixels. Fixed_Types.Fixed_6 type is
--  used to index subpixels.
--
--  4. 16bit binary precision fixed point type is used by rasterizer internally
--  to avoid any floating point operations and floating point to integer
--  conversions on code paths critical for performance. Such precision allows to
--  minimize accumulated error for displays with about 32K resolution.
--
--  Relationship between these types is presented below.
--
--  16bit binary precision      |0                             65535|
--  device pixel index          |                 0                 |
--  64x64 subpixel index        |-32|-31|...| -1| 0 | 1 |...| 30| 31|
--  device pixel coordinate  [-0.5               0.0               0.5)
--                 --- ---      +---+---+...+---+---+---+...+---+---+
--                     -32      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                     -31      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                     ...      ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      -1      |   |   |   |   |   |   |   |   |   |
--                  0  ---      +---+---+...+---+---+---+...+---+---+
--                      0   0.0 |   |   |   |   | * |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      1       |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                     ...      ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮   ⋮
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      30      |   |   |   |   |   |   |   |   |   |
--                     ---      +---+---+...+---+---+---+...+---+---+
--                      31      |   |   |   |   |   |   |   |   |   |
--                 --- ---      +---+---+...+---+---+---+...+---+---+
--
--  Conversion between Device_Pixel_Coordinate and Device_Pixel_Index is
--  straight forward, thanks to Ada's default floating to integer values
--  conversion rules (rounding to nearest integer). Note, negative and positive
--  number are rounded in different directions. It is ignored by the algoriphms.

package GFX.Drawing
  with Pure
is

   subtype Device_Pixel_Coordinate is GFX.Real;
   --  Coordinate of the device's hardware pixel. Integral value corresponds to
   --  the center of the hardware pixel.

   subtype Device_Pixel_Index is GX_Integer;
   --  Index of the device's hardware pixel.

   subtype Device_Pixel_Count is Device_Pixel_Index
     range 0 .. Device_Pixel_Index'Last;

   subtype Grayscale is Interfaces.Unsigned_8;
   --  Grayscale level.

end GFX.Drawing;
