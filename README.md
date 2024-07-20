# Modular Portable Graphics Library: Primitive's Rasteriser

Primitive's Rasteriser provides implementation of the rasterization stage of the drawing by software. Rasterization algoriphms calls subprogram with profile
```ada
     procedure Fill_Span (X         : GFX.Rasteriser.Device_Pixel_Index;
                          Y         : GFX.Rasteriser.Device_Pixel_Index;
                          Width     : GFX.Rasteriser.Device_Pixel_Count;
                          Luminance : GFX.Rasteriser.Grayscale);
```
to do next stage of the processing, like color scaling and blending.

Right now it supports rasterization of the thick lines with anitialiasing.

Use case of this module can be found in [GFX sandbox](https://github.com/godunko/gfx-sandbox).
