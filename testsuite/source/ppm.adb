--
--  Copyright (C) 2024, Vadim Godunko <vgodunko@gmail.com>
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

pragma Ada_2022;

with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;

package body PPM is

   type Raster is
     array (GFX.Drawing.Device_Pixel_Count range <>,
            GFX.Drawing.Device_Pixel_Count range <>)
       of GFX.RGBA8888;

   type Raster_Access is access all Raster;

   Buffer : Raster_Access;

   procedure Finalize is
      procedure Free is
        new Ada.Unchecked_Deallocation (Raster, Raster_Access);

   begin
      Free (Buffer);
   end Finalize;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel
     (X : GFX.Drawing.Device_Pixel_Index;
      Y : GFX.Drawing.Device_Pixel_Index) return GFX.RGBA8888 is
   begin
      return Buffer (X, Y);
   end Get_Pixel;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Width  : GFX.Drawing.Device_Pixel_Count;
      Height : GFX.Drawing.Device_Pixel_Count;
      Color  : GFX.RGBA8888 := GFX.To_RGBA (0, 0, 0, 0))
   is
      use type GFX.Drawing.Device_Pixel_Count;

   begin
      Buffer := new Raster (0 .. Width - 1, 0 .. Height - 1);
      Buffer.all := [others => [others => Color]];
   end Initialize;

   ----------
   -- Save --
   ----------

   procedure Save (File_Name : String) is
      File : Ada.Text_IO.File_Type;

      W : constant String :=
        Ada.Strings.Fixed.Trim
          (GFX.Drawing.Device_Pixel_Count'Image (Buffer'Length (1)),
           Ada.Strings.Both);
      H : constant String :=
        Ada.Strings.Fixed.Trim
          (GFX.Drawing.Device_Pixel_Count'Image (Buffer'Length (2)),
           Ada.Strings.Both);

   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, File_Name);

      Ada.Text_IO.Put_Line (File, "P3");
      Ada.Text_IO.Put_Line (File, W & ' ' & H);
      Ada.Text_IO.Put_Line (File, "255");

      for Row in Buffer'Range (2) loop
         for Column in Buffer'Range (1) loop
            declare
               VR : Interfaces.Unsigned_8;
               VG : Interfaces.Unsigned_8;
               VB : Interfaces.Unsigned_8;
               VA : Interfaces.Unsigned_8;

            begin
               GFX.From_RGBA8888
                 (C => Buffer (Column, Row),
                  R => VR,
                  G => VG,
                  B => VB,
                  A => VA);

               declare
                  R : constant String :=
                    Ada.Strings.Fixed.Trim
                      (Interfaces.Unsigned_8'Image (VR),
                       Ada.Strings.Both);
                  G : constant String :=
                    Ada.Strings.Fixed.Trim
                      (Interfaces.Unsigned_8'Image (VG),
                       Ada.Strings.Both);
                  B : constant String :=
                    Ada.Strings.Fixed.Trim
                      (Interfaces.Unsigned_8'Image (VB),
                       Ada.Strings.Both);

               begin
                  Ada.Text_IO.Put_Line (File, R & ' ' & G & ' ' & B);
               end;
            end;
         end loop;
      end loop;

      Ada.Text_IO.Close (File);
   end Save;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (X     : GFX.Drawing.Device_Pixel_Index;
      Y     : GFX.Drawing.Device_Pixel_Index;
      Color : GFX.RGBA8888) is
   begin
      Buffer (X, Y) := Color;
   end Set_Pixel;

end PPM;
