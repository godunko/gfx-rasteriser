# Platform Independed Graphics Library. Primitive Drawing Module

Drawing Module of the library provides implementation of the rasterization stage of the drawing. Rasterization algoriphms calls subprogram with profile
```ada
     procedure Fill_Span (X        : GFX.Drawing.Device_Pixel_Index;
                          Y        : GFX.Drawing.Device_Pixel_Index;
                          Width    : GFX.Drawing.Device_Pixel_Count;
                          Coverage : GFX.Drawing.Grayscale);
```
to do next stage of the processing, like color scaling and blending.

Use case of this module can be found in [GFX sandbox](https://github.com/godunko/gfx-sandbox).
