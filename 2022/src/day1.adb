with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day1 is
   --  Decided to use a vector for this, since it was the quickest
   package Nat_Vect is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type                                            => Natural);
   package Nat_Sort is new Nat_Vect.Generic_Sorting;
   use Nat_Vect, Nat_Sort;

   My_File : File_Type;
   Sums    : Vector;
   Sum     : Natural := 0;
begin
   Open (My_File, In_File, "input/Day1-1.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String := Get_Line (My_File);
      begin
         if Next_Line = "" then
            --  Add this sum to our vector.
            Sums.Append (Sum);
            --  Reset sum
            Sum := 0;
         else
            --  Continue to accumulate Sum
            Sum := Sum + Natural'Value (Next_Line);
         end if;
      end;
   end loop;
   --  Add whatever is left.  TODO: Stop doing this?
   Sums.Append (Sum);
   --  Now sort our vector
   Sort (Sums);
   Put_Line
     ("The highest calory elf has " & Natural'Image (Sums.Last_Element) &
      " calories.");
   --  Now all we have to do is sum the top 3.
   Sum := 0;
   for I in --  This gets our top 3 picks, since it's been sorted.
     (if Sums.Length > 2 then Sums.Last_Index - 2 else Sums.First_Index) ..
       Sums.Last_Index
   loop
      Sum := Sum + Sums (I);
   end loop;
   Put_Line ("Top 3 elf calories combined are: " & Sum'Image);
end Day1;
