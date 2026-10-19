--
--  Copyright (C) 2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Exact incremental evaluation of a straight segment whose endpoints lie on
--  the 1/64 subpixel grid.
--
--  A segment is described by its slope Delta_Out / Delta_In: the change of
--  the "out" coordinate when the "in" coordinate changes. A coordinate on
--  the "out" axis is represented by its value rounded toward negative
--  infinity to `Fixed_16` precision, and by the remainder of this rounding,
--  so it is exact: no error is accumulated while stepping. Stepping uses
--  additions and comparisons only; divisions are needed to compute slopes
--  and initial coordinates. They use native 32-bit division unless deltas
--  of the segment are too large (more than 512 pixels).

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Fixed_Types;
with GFX.Implementation.Subpixel_Operations;

package GFX.Implementation.DDA_Operations
  with Pure
is

   use type GFX.Implementation.Fixed_Types.Fixed_16;

   type Slope is private;
   --  Exact slope Delta_Out / Delta_In. Objects are not initialized by
   --  default to avoid overhead, see Initialize.

   type Coordinate is private;
   --  Exact coordinate on the "out" axis. Its remainder is relative to
   --  Delta_In of the slope which has been used to compute it. Objects are
   --  not initialized by default to avoid overhead, see To_Coordinate.

   procedure Initialize
     (Self      : out Slope;
      Delta_Out : GFX.Implementation.Fixed_Types.Fixed_16;
      Delta_In  : GFX.Implementation.Fixed_Types.Fixed_16)
   with
     Pre =>
       Delta_In >= GFX.Implementation.Fixed_Types.Zero
       and
         GFX.Implementation.Subpixel_Operations.Ghost.Is_On_Subpixel_Grid
           (Delta_Out)
       and
         GFX.Implementation.Subpixel_Operations.Ghost.Is_On_Subpixel_Grid
           (Delta_In);
   --  Both deltas must be on the 1/64 subpixel grid, so the slope is exact.
   --  When Delta_In is zero the slope can't be used to compute coordinates;
   --  when it is less than one pixel, Step and Step_Back can't be used.

   function To_Coordinate (Value : GFX.Implementation.Fixed_Types.Fixed_16) return Coordinate
     with Inline;
   --  Return coordinate of the given exact value.

   function Value (Self : Coordinate) return GFX.Implementation.Fixed_Types.Fixed_16
     with Inline;
   --  Return value rounded toward negative infinity.

   procedure At_Offset
     (Self    : out Coordinate;
      Origin  : GFX.Implementation.Fixed_Types.Fixed_16;
      Through : Slope;
      Offset  : GFX.Implementation.Fixed_Types.Fixed_16)
   with
     Pre =>
       GFX.Implementation.Fixed_Types.Zero <= Offset
       and Offset <= GFX.Implementation.Fixed_Types.One;
   --  Set Self to Origin + Offset * Delta_Out / Delta_In: the "out"
   --  coordinate of the point at Offset along the "in" axis from the point
   --  with exact "out" coordinate Origin. Offset must be on the 1/64
   --  subpixel grid.

   procedure Step (Self : in out Coordinate; Through : Slope)
     with Inline;
   --  Advance coordinate by one pixel along the "in" axis. Delta_In must be
   --  at least one pixel.

   procedure Step_Back (Self : in out Coordinate; Through : Slope)
     with Inline;
   --  Move coordinate back by one pixel along the "in" axis. Delta_In must
   --  be at least one pixel.

   procedure Cross
     (Self       : out Coordinate;
      Out_Origin : GFX.Implementation.Fixed_Types.Fixed_16;
      In_Origin  : Coordinate;
      Through    : Slope;
      In_Value   : GFX.Implementation.Fixed_Types.Fixed_16)
   with
     Pre =>
       In_Value - Value (In_Origin) >= GFX.Implementation.Fixed_Types.Zero
       and In_Value - Value (In_Origin) <= GFX.Implementation.Fixed_Types.One;
   --  Set Self to the "out" coordinate of the point with the given exact
   --  "in" coordinate, on the line through the point (In_Origin,
   --  Out_Origin).
   --
   --  In_Origin is an exact coordinate computed with the inverse slope of
   --  the same line, so its remainder is relative to abs Delta_Out of
   --  Through. This allows to compute intersections of the segment with
   --  column boundaries from its intersections with row boundaries without
   --  loss of precision.

private

   type Slope is record
      Delta_Out   : GX_Integer;
      Denominator : GX_Integer;
      --  Deltas in 1/64 subpixel units; Denominator is Delta_In.
      Lift        : GFX.Implementation.Fixed_Types.Fixed_16;
      Remainder   : GX_Integer;
      --  Change of the coordinate when the "in" coordinate changes by one
      --  pixel: Lift + Remainder / Denominator.
   end record;

   type Coordinate is record
      Value : GFX.Implementation.Fixed_Types.Fixed_16;
      Error : GX_Integer;
      --  Exact value is Value + Error / Denominator, Error is in
      --  0 .. Denominator - 1.
   end record;

   function To_Coordinate
     (Value : GFX.Implementation.Fixed_Types.Fixed_16) return Coordinate
   is ((Value => Value, Error => 0));

   function Value
     (Self : Coordinate) return GFX.Implementation.Fixed_Types.Fixed_16
   is (Self.Value);

end GFX.Implementation.DDA_Operations;
