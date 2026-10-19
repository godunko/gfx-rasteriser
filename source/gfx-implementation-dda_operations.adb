--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Subpixel_Operations;

package body GFX.Implementation.DDA_Operations is

   use GFX.Implementation.Fixed_Types;
   use GFX.Implementation.Subpixel_Operations;

   ---------------
   -- At_Offset --
   ---------------

   procedure At_Offset
     (Self    : out Coordinate;
      Origin  : Fixed_16;
      Through : Slope;
      Offset  : Fixed_16)
   is
      Quotient : Fixed_16;

   begin
      Multiply_Divide_Floor
        (Offset,
         Through.Delta_Out,
         0,
         Through.Denominator,
         Quotient,
         Self.Error);
      Self.Value := Origin + Quotient;
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

      Quotient : Fixed_16;

   begin
      Multiply_Divide_Floor
        (In_Value - In_Origin.Value,
         Through.Delta_Out,
         (if Through.Delta_Out < 0
          then In_Origin.Error
          else -In_Origin.Error),
         Through.Denominator,
         Quotient,
         Self.Error);
      Self.Value := Out_Origin + Quotient;
   end Cross;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self      : out Slope;
      Delta_Out : Fixed_16;
      Delta_In  : Fixed_16)
   is
   begin
      Self.Delta_Out   := To_Subpixel (Delta_Out);
      Self.Denominator := To_Subpixel (Delta_In);

      if Self.Denominator >= To_Subpixel (One) then
         --  Step is used only when Delta_In is at least one pixel.

         Multiply_Divide_Floor
           (One,
            Self.Delta_Out,
            0,
            Self.Denominator,
            Self.Lift,
            Self.Remainder);

      else
         Self.Lift      := Zero;
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
         Self.Value := Self.Value + Fixed_16_Delta_Fixed;
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
         Self.Value := Self.Value - Fixed_16_Delta_Fixed;
         Self.Error := Self.Error + Through.Denominator;
      end if;
   end Step_Back;

end GFX.Implementation.DDA_Operations;
