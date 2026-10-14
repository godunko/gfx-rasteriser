--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Text_IO;

with GFX.Implementation.Clipping;
with GFX.Points;

procedure GF_Clip_Rectangle is

   use GFX;
   use GFX.Implementation.Clipping;
   use type GFX.Points.GF_Point;

   Rectangle : GFX.Implementation.Clipping.GF_Clipping_Rectangle;
   Left      : Long_Float;
   Right     : Long_Float;
   Top       : Long_Float;
   Bottom    : Long_Float;
   Checks    : Natural := 0;

   procedure Check_Code (Point : GFX.Points.GF_Point; Expected : Outcode);

   procedure Check_Clip
     (P          : GFX.Points.GF_Point;
      Q          : GFX.Points.GF_Point;
      Expected   : Boolean;
      Expected_A : GFX.Points.GF_Point := (0.0, 0.0);
      Expected_B : GFX.Points.GF_Point := (0.0, 0.0));

   procedure Check_Reference
     (P : GFX.Points.GF_Point;
      Q : GFX.Points.GF_Point);

   procedure Require (Condition : Boolean; Message : String);

   procedure Set_Area
     (X_Min : GX_Integer;
      X_Max : GX_Integer;
      Y_Min : GX_Integer;
      Y_Max : GX_Integer);

   ----------------
   -- Check_Clip --
   ----------------

   procedure Check_Clip
     (P          : GFX.Points.GF_Point;
      Q          : GFX.Points.GF_Point;
      Expected   : Boolean;
      Expected_A : GFX.Points.GF_Point := (0.0, 0.0);
      Expected_B : GFX.Points.GF_Point := (0.0, 0.0))
   is
      A        : GFX.Points.GF_Point;
      B        : GFX.Points.GF_Point;
      Accepted : Boolean;

   begin
      for Reverse_Order in Boolean loop
         if Reverse_Order then
            Accepted := Clip (Rectangle, Q, P, A, B);

         else
            Accepted := Clip (Rectangle, P, Q, A, B);
         end if;

         Require (Accepted = Expected, "Unexpected clipping decision");

         if Accepted then
            if Reverse_Order then
               Require
                 (A = Expected_B and B = Expected_A,
                  "Unexpected reversed endpoints");

            else
               Require
                (A = Expected_A and B = Expected_B,
                 "Unexpected endpoints");
            end if;
         end if;

         Checks := @ + 1;
      end loop;
   end Check_Clip;

   ----------------
   -- Check_Code --
   ----------------

   procedure Check_Code (Point : GFX.Points.GF_Point; Expected : Outcode) is
   begin
      Require
        (Compute_Outcode (Rectangle, Point) = Expected,
         "Unexpected outcode at" & Point.X'Image & Point.Y'Image);
   end Check_Code;

   ---------------------
   -- Check_Reference --
   ---------------------

   procedure Check_Reference
     (P : GFX.Points.GF_Point;
      Q : GFX.Points.GF_Point)
   is
      --  Independent slab intersection in double precision. Intersect
      --  parameter intervals of P + T * (Q - P), with T in [0, 1].

      T_Min    : Long_Float := 0.0;
      T_Max    : Long_Float := 1.0;
      Visible  : Boolean := True;
      DX       : constant Long_Float := Long_Float (Q.X) - Long_Float (P.X);
      DY       : constant Long_Float := Long_Float (Q.Y) - Long_Float (P.Y);
      A        : GFX.Points.GF_Point;
      B        : GFX.Points.GF_Point;
      Accepted : Boolean;
      Context  : constant String :=
        " for" & P.X'Image & P.Y'Image & " ->" & Q.X'Image & Q.Y'Image;

      procedure Axis
        (Origin      : Long_Float;
         Delta_Value : Long_Float;
         Min         : Long_Float;
         Max         : Long_Float);

      procedure Near (Actual : GX_Real; Expected : Long_Float);

      ----------
      -- Axis --
      ----------

      procedure Axis
        (Origin      : Long_Float;
         Delta_Value : Long_Float;
         Min         : Long_Float;
         Max         : Long_Float)
      is
         T_0 : Long_Float;
         T_1 : Long_Float;

      begin
         if Delta_Value = 0.0 then
            Visible := @ and Origin in Min .. Max;

         else
            T_0   := (Min - Origin) / Delta_Value;
            T_1   := (Max - Origin) / Delta_Value;
            T_Min := Long_Float'Max (T_Min, Long_Float'Min (T_0, T_1));
            T_Max := Long_Float'Min (T_Max, Long_Float'Max (T_0, T_1));
         end if;
      end Axis;

      ----------
      -- Near --
      ----------

      procedure Near (Actual : GX_Real; Expected : Long_Float) is
         Tolerance : constant Long_Float :=
           0.00002 + 8.0 * Long_Float (GX_Real'Model_Epsilon)
             * abs Expected;

      begin
         Require
           (abs (Long_Float (Actual) - Expected) <= Tolerance,
            "Endpoint differs from reference" & Context);
      end Near;

   begin
      Axis (Long_Float (P.X), DX, Left, Right);
      Axis (Long_Float (P.Y), DY, Top, Bottom);

      Visible  := @ and T_Min <= T_Max;
      Accepted := Clip (Rectangle, P, Q, A, B);

      Require
        (Accepted = Visible,
         "Decision differs from reference" & Context);

      if Accepted then
         Require
           (Compute_Outcode (Rectangle, A) = Inside
              and Compute_Outcode (Rectangle, B) = Inside,
            "Clipped endpoint is outside" & Context);

         Near (A.X, Long_Float (P.X) + T_Min * DX);
         Near (A.Y, Long_Float (P.Y) + T_Min * DY);
         Near (B.X, Long_Float (P.X) + T_Max * DX);
         Near (B.Y, Long_Float (P.Y) + T_Max * DY);
      end if;

      Checks := @ + 1;
   end Check_Reference;

   -------------
   -- Require --
   -------------

   procedure Require (Condition : Boolean; Message : String) is
   begin
      if not Condition then
         raise Program_Error with Message;
      end if;
   end Require;

   --------------
   -- Set_Area --
   --------------

   procedure Set_Area
     (X_Min : GX_Integer;
      X_Max : GX_Integer;
      Y_Min : GX_Integer;
      Y_Max : GX_Integer) is
   begin
      Set_Bound (Rectangle, X_Min, X_Max, Y_Min, Y_Max);

      Left   := Long_Float (X_Min);
      Right  := Long_Float (X_Max);
      Top    := Long_Float (Y_Min);
      Bottom := Long_Float (Y_Max);
   end Set_Area;

begin
   --  Set_Bound takes geometric boundary coordinates, without adding 1
   --  to X_Max or Y_Max. Use unequal, translated bounds to detect axis
   --  swaps and test all nine outcode regions.

   Set_Area (-3, 7, 2, 6);

   for X in -1 .. 1 loop
      for Y in -1 .. 1 loop
         declare
            PX : constant GX_Real :=
              (if X < 0 then -4.0 elsif X > 0 then 8.0 else 0.0);
            PY : constant GX_Real :=
              (if Y < 0 then 1.0 elsif Y > 0 then 7.0 else 4.0);

         begin
            Check_Code
              ((PX, PY),
               [X_Decrease => X < 0,
                X_Increase => X > 0,
                Y_Decrease => Y < 0,
                Y_Increase => Y > 0]);
         end;
      end loop;
   end loop;

   Check_Code ((-3.0, 2.0), Inside);
   Check_Code ((7.0, 6.0), Inside);
   Check_Code
     ((GX_Real'Pred (-3.0), 4.0), [X_Decrease => True, others => False]);
   Check_Code
     ((GX_Real'Succ (7.0), 4.0), [X_Increase => True, others => False]);
   Check_Code
     ((0.0, GX_Real'Pred (2.0)), [Y_Decrease => True, others => False]);
   Check_Code
     ((0.0, GX_Real'Succ (6.0)), [Y_Increase => True, others => False]);

   Check_Clip ((-5.0, 4.0), (9.0, 4.0), True, (-3.0, 4.0), (7.0, 4.0));
   Check_Clip ((0.0, 0.0), (0.0, 8.0), True, (0.0, 2.0), (0.0, 6.0));

   Set_Area (0, 10, 0, 10);
   Check_Clip ((2.0, 3.0), (8.0, 7.0), True, (2.0, 3.0), (8.0, 7.0));
   Check_Clip ((-1.0, 5.0), (11.0, 5.0), True, (0.0, 5.0), (10.0, 5.0));
   Check_Clip ((5.0, -1.0), (5.0, 11.0), True, (5.0, 0.0), (5.0, 10.0));
   Check_Clip ((-1.0, -1.0), (11.0, 11.0), True, (0.0, 0.0), (10.0, 10.0));
   Check_Clip ((-1.0, 11.0), (11.0, -1.0), True, (0.0, 10.0), (10.0, 0.0));

   --  Exact upper boundaries, last subpixel and the former extrapolation
   --  regression: clipping 9.99 .. 10.1 must not extend past 9.99.
   Check_Clip ((10.0, 0.0), (10.0, 10.0), True, (10.0, 0.0), (10.0, 10.0));
   Check_Clip ((0.0, 10.0), (10.0, 10.0), True, (0.0, 10.0), (10.0, 10.0));
   Check_Clip
     ((9.984375, 5.0), (11.0, 5.0), True, (9.984375, 5.0), (10.0, 5.0));
   Check_Clip ((9.99, 5.0), (10.1, 5.0), True, (9.99, 5.0), (10.0, 5.0));
   Check_Clip ((5.0, 9.99), (5.0, 10.1), True, (5.0, 9.99), (5.0, 10.0));

   --  Corner touches, degenerate segments and rejection without a shared
   --  outcode bit. Every named case also runs with endpoints reversed.
   Check_Clip ((-1.0, 9.0), (1.0, 11.0), True, (0.0, 10.0), (0.0, 10.0));
   Check_Clip ((-1.0, 1.0), (1.0, -1.0), True, (0.0, 0.0), (0.0, 0.0));
   Check_Clip ((-2.0, 9.0), (1.0, 12.0), False);
   Check_Clip ((1.0, 1.0), (1.0, 1.0), True, (1.0, 1.0), (1.0, 1.0));
   Check_Clip ((10.0, 10.0), (10.0, 10.0), True, (10.0, 10.0), (10.0, 10.0));
   Check_Clip ((-1.0, -1.0), (-1.0, -1.0), False);
   Check_Clip ((-1.0, 0.0), (-1.0, 10.0), False);
   Check_Clip ((11.0, 0.0), (11.0, 10.0), False);
   Check_Clip ((0.0, -1.0), (10.0, -1.0), False);
   Check_Clip ((0.0, 11.0), (10.0, 11.0), False);

   --  Exhaust all 5,764,801 ordered segments on a quarter-pixel grid,
   --  including parallel edges, corners, points and repeated clipping.
   for X_A in -4 .. 44 loop
      for Y_A in -4 .. 44 loop
         for X_B in -4 .. 44 loop
            for Y_B in -4 .. 44 loop
               Check_Reference
                 ((GX_Real (X_A) / 4.0, GX_Real (Y_A) / 4.0),
                  (GX_Real (X_B) / 4.0, GX_Real (Y_B) / 4.0));
            end loop;
         end loop;
      end loop;
   end loop;

   --  Single-pixel extent and replacing old bounds.
   Set_Area (3, 4, 4, 5);
   Check_Clip ((2.0, 4.5), (5.0, 4.5), True, (3.0, 4.5), (4.0, 4.5));
   Check_Clip ((0.0, 0.0), (1.0, 1.0), False);
   Set_Area (-3, 7, 2, 6);
   Check_Clip ((3.0, 0.0), (3.0, 8.0), True, (3.0, 2.0), (3.0, 6.0));

   --  Large positive and negative coordinates in the supported range.
   Set_Area (-16_000, 16_000, -15_000, 15_000);
   Check_Reference ((-16_384.0, -16_384.0), (16_382.0, 16_382.0));
   Check_Reference ((16_382.0, -16_384.0), (-16_384.0, 16_382.0));
   Check_Reference ((-16_384.0, 1.0), (16_382.0, 2.0));
   Check_Reference ((1.0, -16_384.0), (2.0, 16_382.0));

   Ada.Text_IO.Put_Line ("GF_Clip_Rectangle checks passed:" & Checks'Image);
end GF_Clip_Rectangle;
