--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Subpixel_Operations;

package body GFX.Implementation.Fixed_Types.Exact_DDA is

   use GFX.Implementation.Subpixel_Operations;

   Pixel_Scale : constant := 2 ** 16;
   --  Scale of pixel in Fixed_16 units.

   Small_Delta : constant := 2 ** 15;
   --  Deltas less than this value in subpixel units (512 pixels) allow to
   --  use native 32-bit arithmetic, see Divide.

   procedure Divide
     (Factor      : GX_Integer;
      Delta_Out   : GX_Integer;
      Correction  : GX_Integer;
      Denominator : GX_Integer;
      Quotient    : out GX_Integer;
      Remainder   : out GX_Integer)
     with Inline,
          Pre => Factor in 0 .. Pixel_Scale
                   and abs Correction < Small_Delta
                   and Denominator > 0;
   --  Divide Factor * Delta_Out + Correction by Denominator rounding toward
   --  negative infinity, so Remainder is in 0 .. Denominator - 1. Quotient
   --  saturates on overflow, Remainder is zero in this case.

   ---------------
   -- At_Offset --
   ---------------

   procedure At_Offset
     (Self    : out Coordinate;
      Origin  : Fixed_16;
      Through : Slope;
      Offset  : Fixed_16)
   is
      Quotient : GX_Integer;

   begin
      Divide
        (GX_Integer (Offset),
         Through.Delta_Out,
         0,
         Through.Denominator,
         Quotient,
         Self.Error);
      Self.Value := Origin + Fixed_16 (Quotient);
   end At_Offset;

   -----------
   -- Cross --
   -----------

   procedure Cross
     (Self       : out Coordinate;
      Out_Origin : Fixed_16;
      In_Origin  : Coordinate;
      Through    : Slope;
      In_Value   : Fixed_16)
   is
      --  The exact "in" coordinate of the origin is
      --  In_Origin.Value + In_Origin.Error / abs Delta_Out, thus the
      --  distance along the "in" axis multiplied by Delta_Out is
      --  (In_Value - In_Origin.Value) * Delta_Out
      --    - In_Origin.Error * sign (Delta_Out).

      Quotient : GX_Integer;

   begin
      Divide
        (GX_Integer (In_Value - In_Origin.Value),
         Through.Delta_Out,
         (if Through.Delta_Out < 0
          then In_Origin.Error
          else -In_Origin.Error),
         Through.Denominator,
         Quotient,
         Self.Error);
      Self.Value := Out_Origin + Fixed_16 (Quotient);
   end Cross;

   ------------
   -- Divide --
   ------------

   procedure Divide
     (Factor      : GX_Integer;
      Delta_Out   : GX_Integer;
      Correction  : GX_Integer;
      Denominator : GX_Integer;
      Quotient    : out GX_Integer;
      Remainder   : out GX_Integer) is
   begin
      if abs Delta_Out < Small_Delta then
         --  The numerator fits into GX_Integer. Native arithmetic is much
         --  faster than 64-bit one on 32-bit CPUs.

         declare
            Numerator : constant GX_Integer :=
              Factor * Delta_Out + Correction;

         begin
            Quotient  := Numerator / Denominator;
            Remainder := Numerator rem Denominator;
         end;

      else
         declare
            Numerator : constant GX_Integer_2 :=
              GX_Integer_2 (Factor) * GX_Integer_2 (Delta_Out)
                + GX_Integer_2 (Correction);
            Q         : constant GX_Integer_2 :=
              Numerator / GX_Integer_2 (Denominator);

         begin
            if Q not in GX_Integer_2 (GX_Integer'First + 1)
                          .. GX_Integer_2 (GX_Integer'Last)
            then
               Quotient  :=
                 (if Q < 0 then GX_Integer'First + 1 else GX_Integer'Last);
               Remainder := 0;

               return;
            end if;

            Quotient  := GX_Integer (Q);
            Remainder :=
              GX_Integer (Numerator - Q * GX_Integer_2 (Denominator));
         end;
      end if;

      if Remainder < 0 then
         Quotient  := Quotient - 1;
         Remainder := Remainder + Denominator;
      end if;
   end Divide;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out Slope;
      Delta_Out : Fixed_16;
      Delta_In  : Fixed_16)
   is
      Lift : GX_Integer;

   begin
      Self.Delta_Out   := To_Subpixel (Delta_Out);
      Self.Denominator := To_Subpixel (Delta_In);

      if Self.Denominator >= To_Subpixel (One) then
         --  Step is used only when Delta_In is at least one pixel.

         Divide
           (Pixel_Scale,
            Self.Delta_Out,
            0,
            Self.Denominator,
            Lift,
            Self.Remainder);
         Self.Lift := Fixed_16 (Lift);

      else
         Self.Lift      := 0;
         Self.Remainder := 0;
      end if;
   end Initialize;

   ----------
   -- Step --
   ----------

   procedure Step (Self : in out Coordinate; Through : Slope) is
   begin
      Self.Value := Self.Value + Through.Lift;
      Self.Error := Self.Error + Through.Remainder;

      if Self.Error >= Through.Denominator then
         Self.Value := Self.Value + 1;
         Self.Error := Self.Error - Through.Denominator;
      end if;
   end Step;

   ---------------
   -- Step_Back --
   ---------------

   procedure Step_Back (Self : in out Coordinate; Through : Slope) is
   begin
      Self.Value := Self.Value - Through.Lift;
      Self.Error := Self.Error - Through.Remainder;

      if Self.Error < 0 then
         Self.Value := Self.Value - 1;
         Self.Error := Self.Error + Through.Denominator;
      end if;
   end Step_Back;

end GFX.Implementation.Fixed_Types.Exact_DDA;
