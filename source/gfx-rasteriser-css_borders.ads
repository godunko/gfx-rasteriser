--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  This package provides subprograms to rasterize CSS border.
--
--  These subprograms need to be combined with color scaling and color blending
--  subprograms to draw real image.
--
--  Access to span draw subprogram versus generic with formal span draw
--  subprogram is selected to have minimal code footprint.

pragma Restrictions (No_Elaboration_Code);

--  with GFX.Points;

package GFX.Rasteriser.CSS_Borders is

   type Radius is record
      Horizontal : GFX.GX_Real;
      Vertical   : GFX.GX_Real;
   end record;

   type CSS_Box is record
      Top    : GFX.GX_Real;
      Right  : GFX.GX_Real;
      Bottom : GFX.GX_Real;
      Left   : GFX.GX_Real;
   end record;

   type CSS_Border is record
      Edge                : CSS_Box;

      Top_Width           : GFX.GX_Real;
      Right_Width         : GFX.GX_Real;
      Bottom_Width        : GFX.GX_Real;
      Left_Width          : GFX.GX_Real;

      Top_Left_Radius     : Radius;
      Top_Right_Radius    : Radius;
      Bottom_Right_Radius : Radius;
      Botton_Left_Radius  : Radius;
   end record;

   type Fill_Span_Subprogram is
     access procedure (X        : GFX.Rasteriser.Device_Pixel_Index;
                       Y        : GFX.Rasteriser.Device_Pixel_Index;
                       Width    : GFX.Rasteriser.Device_Pixel_Count;
                       Coverage : GFX.Rasteriser.Grayscale);

   procedure Rasterize_CSS_Border
     (Border    : CSS_Border;
      Fill_Span : not null access procedure
        (X        : GFX.Rasteriser.Device_Pixel_Index;
         Y        : GFX.Rasteriser.Device_Pixel_Index;
         Width    : GFX.Rasteriser.Device_Pixel_Count;
         Coverage : GFX.Rasteriser.Grayscale));
   --  Draw CSS border

   procedure Set_Rendering_Area
     (Top    : GFX.Rasteriser.Device_Pixel_Index;
      Left   : GFX.Rasteriser.Device_Pixel_Index;
      Right  : GFX.Rasteriser.Device_Pixel_Index;
      Bottom : GFX.Rasteriser.Device_Pixel_Index);

end GFX.Rasteriser.CSS_Borders;
