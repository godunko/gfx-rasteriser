--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Clipping utilities

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Fixed_Types;
with GFX.Rasteriser;

package GFX.Implementation.Clipping
  with Pure
is

   type Outside_Direction is (X_Decrease, X_Increase, Y_Decrease, Y_Increase);

   type Outcode is array (Outside_Direction) of Boolean
     with Pack, Size => 4;

   type Rendering_Area is private
     with Preelaborable_Initialization;

   procedure Set_Rendering_Area
     (Self  : out Rendering_Area;
      X_Min : GFX.Rasteriser.Device_Pixel_Index;
      X_Max : GFX.Rasteriser.Device_Pixel_Index;
      Y_Min : GFX.Rasteriser.Device_Pixel_Index;
      Y_Max : GFX.Rasteriser.Device_Pixel_Index)
     with Pre => (X_Min <= X_Max) and (Y_Min <= Y_Max);
   --  Sets clipping are to the given rendering area.

   function X_Min
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16;
   function X_Max
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16;
   function Y_Min
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16;
   function Y_Max
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16;

   function Compute_Outcode
     (Self : Rendering_Area;
      X    : GFX.Implementation.Fixed_Types.Fixed_16;
      Y    : GFX.Implementation.Fixed_Types.Fixed_16) return Outcode;
   --  Compute position of the given point relative to the clipping area.

private

   type Rendering_Area is record
      X_Min : GFX.Implementation.Fixed_Types.Fixed_16; -- :=
        --  GFX.Implementation.Fixed_Types.Fixed_16_First;
      X_Max : GFX.Implementation.Fixed_Types.Fixed_16; -- :=
        --  GFX.Implementation.Fixed_Types.Fixed_16_Last;
      Y_Min : GFX.Implementation.Fixed_Types.Fixed_16; -- :=
        --  GFX.Implementation.Fixed_Types.Fixed_16_First;
      Y_Max : GFX.Implementation.Fixed_Types.Fixed_16; --  :=
        --  GFX.Implementation.Fixed_Types.Fixed_16_Last;
   end record;

   function X_Min
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16 is
        (Self.X_Min);
   function X_Max
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16 is
        (Self.X_Max);
   function Y_Min
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16 is
        (Self.Y_Min);
   function Y_Max
     (Self : Rendering_Area) return GFX.Implementation.Fixed_Types.Fixed_16 is
        (Self.Y_Max);

end GFX.Implementation.Clipping;
