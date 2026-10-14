--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

pragma Restrictions (No_Elaboration_Code);

package body GFX.Implementation.Clipping is

   use GFX.Implementation.Fixed_Types;

   ----------
   -- Clip --
   ----------

   function Clip
     (Self     : GF_Clipping_Rectangle;
      Vertex_A : GFX.Points.GF_Point;
      Vertex_B : GFX.Points.GF_Point;
      Clip_A   : out GFX.Points.GF_Point;
      Clip_B   : out GFX.Points.GF_Point) return Boolean
   is
      use type GFX.Implementation.Clipping.Outcode;

      Code_A : GFX.Implementation.Clipping.Outcode :=
        Compute_Outcode (Self, Vertex_A);
      Code_B : GFX.Implementation.Clipping.Outcode :=
        Compute_Outcode (Self, Vertex_B);

   begin
      Clip_A := Vertex_A;
      Clip_B := Vertex_B;

      loop
         if Code_A = GFX.Implementation.Clipping.Inside
           and Code_B = GFX.Implementation.Clipping.Inside
         then
            --  Both endpoints lie inside the rectangle

            return True;

         elsif (Code_A and Code_B) /= GFX.Implementation.Clipping.Inside then
            --  Both endpoints share an outside region (trivial reject)

            return False;

         elsif Code_A /= GFX.Implementation.Clipping.Inside then
            if Code_A (GFX.Implementation.Clipping.X_Decrease) then
               Clip_A :=
                 (X => Self.X_Min,
                  Y =>
                    Clip_A.Y
                      + (Clip_B.Y - Clip_A.Y)
                        * (Self.X_Min - Clip_A.X)
                          / (Clip_B.X - Clip_A.X));

            elsif Code_A (GFX.Implementation.Clipping.X_Increase) then
               Clip_A :=
                 (X => Self.X_Max,
                  Y =>
                    Clip_A.Y
                      + (Clip_B.Y - Clip_A.Y)
                        * (Self.X_Max - Clip_A.X)
                          / (Clip_B.X - Clip_A.X));

            elsif Code_A (GFX.Implementation.Clipping.Y_Decrease) then
               Clip_A :=
                 (X =>
                    Clip_A.X
                      + (Clip_B.X - Clip_A.X)
                        * (Self.Y_Min - Clip_A.Y)
                          / (Clip_B.Y - Clip_A.Y),
                  Y => Self.Y_Min);

            else
               --  The remaining outside direction is Y_Increase.
               Clip_A :=
                 (X =>
                    Clip_A.X
                      + (Clip_B.X - Clip_A.X)
                        * (Self.Y_Max - Clip_A.Y)
                          / (Clip_B.Y - Clip_A.Y),
                  Y => Self.Y_Max);

            end if;

            Code_A := Compute_Outcode (Self, Clip_A);

         else
            if Code_B (GFX.Implementation.Clipping.X_Decrease) then
               Clip_B :=
                 (X => Self.X_Min,
                  Y =>
                    Clip_A.Y
                      + (Clip_B.Y - Clip_A.Y)
                        * (Self.X_Min - Clip_A.X)
                          / (Clip_B.X - Clip_A.X));

            elsif Code_B (GFX.Implementation.Clipping.X_Increase) then
               Clip_B :=
                 (X => Self.X_Max,
                  Y =>
                    Clip_A.Y
                      + (Clip_B.Y - Clip_A.Y)
                        * (Self.X_Max - Clip_A.X)
                          / (Clip_B.X - Clip_A.X));

            elsif Code_B (GFX.Implementation.Clipping.Y_Decrease) then
               Clip_B :=
                 (X =>
                    Clip_A.X
                      + (Clip_B.X - Clip_A.X)
                        * (Self.Y_Min - Clip_A.Y)
                          / (Clip_B.Y - Clip_A.Y),
                  Y => Self.Y_Min);

            else
               --  The remaining outside direction is Y_Increase.
               Clip_B :=
                 (X =>
                    Clip_A.X
                      + (Clip_B.X - Clip_A.X)
                        * (Self.Y_Max - Clip_A.Y)
                          / (Clip_B.Y - Clip_A.Y),
                  Y => Self.Y_Max);
            end if;

            Code_B := Compute_Outcode (Self, Clip_B);
         end if;
      end loop;
   end Clip;

   ---------------------
   -- Compute_Outcode --
   ---------------------

   function Compute_Outcode
     (Self  : GF_Clipping_Rectangle;
      Point : GFX.Points.GF_Point) return GFX.Implementation.Clipping.Outcode
   is
   begin
      return
        [X_Decrease => Point.X    < Self.X_Min,
         X_Increase => Self.X_Max < Point.X,
         Y_Decrease => Point.Y    < Self.Y_Min,
         Y_Increase => Self.Y_Max < Point.Y];
   end Compute_Outcode;

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

   ---------------
   -- Set_Bound --
   ---------------

   procedure Set_Bound
     (Self  : out GF_Clipping_Rectangle;
      X_Min : GFX.Rasteriser.Device_Pixel_Index;
      X_Max : GFX.Rasteriser.Device_Pixel_Index;
      Y_Min : GFX.Rasteriser.Device_Pixel_Index;
      Y_Max : GFX.Rasteriser.Device_Pixel_Index) is
   begin
      Self.X_Min := GFX.Rasteriser.Pixel_Lower_Bound (X_Min);
      Self.X_Max := GFX.Rasteriser.Pixel_Lower_Bound (X_Max);
      Self.Y_Min := GFX.Rasteriser.Pixel_Lower_Bound (Y_Min);
      Self.Y_Max := GFX.Rasteriser.Pixel_Lower_Bound (Y_Max);
   end Set_Bound;

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
