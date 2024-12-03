pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Containers.Vectors;
procedure Day_01 is
   Input : constant String := "input.txt";
   F     : File_Type;
   Sum   : Natural := 0;
   package ID_List is new
      Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Integer);
   package Sorter is new ID_list.Generic_Sorting;

   List1, List2 : ID_List.Vector;

   function Count (List : ID_List.Vector; Item : Integer) return Integer is
      Result : Integer := 0;
   begin
      for I of List when I = Item loop
         Result := @ + 1;
      end loop;
      return Result;
   end Count;
begin
   --  Open the file, parse the lists.
   Open (F, In_File, "input.txt");
   while not End_Of_File (F) loop
      declare
         Line : constant String := Get_Line (F);
         Sp   : constant Natural := Index (Line, " ");
         I1   : constant Integer := Integer'Value (Trim (Line (Line'First .. Sp), Both));
         I2   : constant Integer := Integer'Value (Trim (Line (Sp .. Line'Last), Both));
      begin
         List1.Append (I1);
         List2.Append (I2);
      end;
   end loop;
   Close (F);
   --  Sort list
   Sorter.Sort (List1);
   Sorter.Sort (List2);
   --  Part 1
   --  Sum both items
   for I in List1.First_Index .. List1.Last_Index loop
      declare
         Diff : constant Integer := List1.Element (I) - List2.Element (I);
      begin
         Sum := @ + (abs Diff);
      end;
   end loop;
   Put_Line (Sum'Image);
   --  Part 2, calcultae similarity score
   Sum := 0;
   for I of List1 loop
      Sum := @ + (I * Count (List2, I));
   end loop;
   Put_Line (Sum'Image);

end Day_01;
