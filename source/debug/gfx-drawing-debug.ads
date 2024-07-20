--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with GFX.Implementation.Fixed_Types;

package GFX.Drawing.Debug is

   function Image
     (Item : GFX.Implementation.Fixed_Types.Fixed_16) return String;

end GFX.Drawing.Debug;
