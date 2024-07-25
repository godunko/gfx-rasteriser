--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

package body GFX.Implementation.Clipping is

   use GFX.Implementation.Fixed_Types;

   ---------------------
   -- Compute_Outcode --
   ---------------------

   function Compute_Outcode
     (Self : Rendering_Area;
      X    : GFX.Implementation.Fixed_Types.Fixed_16;
      Y    : GFX.Implementation.Fixed_Types.Fixed_16)
      return Outcode is
   begin
      return
        (X_Decrease => X          < Self.X_Min,
         X_Increase => Self.X_Max < Y,
         Y_Decrease => Y          < Self.Y_Min,
         Y_Increase => Self.Y_Max < Y);
   end Compute_Outcode;

   ------------------------
   -- Set_Rendering_Area --
   ------------------------

   procedure Set_Rendering_Area
     (Self  : out Rendering_Area;
      X_Min : GFX.Rasteriser.Device_Pixel_Index;
      X_Max : GFX.Rasteriser.Device_Pixel_Index;
      Y_Min : GFX.Rasteriser.Device_Pixel_Index;
      Y_Max : GFX.Rasteriser.Device_Pixel_Index) is
   begin
      Self :=
        (X_Min => Pixel_Lower_Bound (X_Min),
         X_Max => Pixel_Upper_Bound (X_Max),
         Y_Min => Pixel_Lower_Bound (Y_Min),
         Y_Max => Pixel_Upper_Bound (Y_Max));
   end Set_Rendering_Area;

end GFX.Implementation.Clipping;
