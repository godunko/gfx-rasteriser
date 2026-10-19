--
--  Copyright (C) 2024-2026, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

--  Coverage of the cell by edges of the contour, and integration of cells
--  of the row from left to right.

pragma Restrictions (No_Elaboration_Code);

with GFX.Implementation.Fixed_Types;

private package GFX.Rasteriser.Coverage
  with Pure
is

   type Cell is record
      Coverage   : GFX.Implementation.Fixed_Types.Fixed_6 :=
        GFX.Implementation.Fixed_Types.Zero;
      --  Vertical height of edges inside the cell.
      Twice_Area : GFX.Implementation.Fixed_Types.Fixed_12 :=
        GFX.Implementation.Fixed_Types.Zero;
      --  Twice the signed area of trapezoids matching the right edge.
   end record;

   procedure Add
     (Item   : in out Cell;
      Height : GFX.Implementation.Fixed_Types.Fixed_6;
      Width  : GFX.Implementation.Fixed_Types.Fixed_6)
     with Inline_Always;
   --  Add contribution of the part of the edge inside the cell. Height is
   --  the signed vertical height of the part, Width is the sum of distances
   --  from both ends of the part to the right edge of the cell.

   procedure Integrate
     (Accumulator : in out GFX.Implementation.Fixed_Types.Fixed_6;
      Item        : Cell;
      Alpha       : out Grayscale)
     with Inline_Always;
   --  Compute coverage of the cell from the accumulated height of edges to
   --  the left of the cell and contribution of edges inside of it, and add
   --  this contribution to Accumulator.

end GFX.Rasteriser.Coverage;
