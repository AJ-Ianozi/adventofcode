with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure Day4 is

   type Assignments is record
      From : Natural;
      To   : Natural;
   end record;

   function Assign (Pair : String) return Assignments is
      Pos    : constant Natural := Index (Pair, "-");
      Result : Assignments      :=
        (From => Natural'Value (Pair (Pair'First .. Pos - 1)),
         To   => Natural'Value (Pair (Pos + 1 .. Pair'Last)));
   begin
      return Result;
   end Assign;
   Sum1 : Natural := 0;
   Sum2 : Natural := 0;

   My_File : File_Type;
begin
   Open (My_File, In_File, "input/Day4.txt");
   Put_Line ("Starting");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String      := Get_Line (My_File);
         Pos       : constant Natural     := Index (Next_Line, ",");
         Elf1      : constant Assignments :=
           Assign (Next_Line (Next_Line'First .. Pos - 1));
         Elf2 : constant Assignments :=
           Assign (Next_Line (Pos + 1 .. Next_Line'Last));
      begin
         if (Elf1.From <= Elf2.From and then Elf1.To >= Elf2.To)
           or else (Elf2.From <= Elf1.From and then Elf2.To >= Elf1.To)
         then
            --  One overlaps completely in the other
            Sum1 := Sum1 + 1;
         end if;
         if (Elf1.From <= Elf2.From and then Elf1.To >= Elf2.From)
           or else (Elf2.From <= Elf1.From and then Elf2.To >= Elf1.From)
         then
            --  They have SOME overlap.
            Sum2 := Sum2 + 1;
         end if;
      end;
   end loop;
   Put_Line ("Complete overlap: " & Sum1'Image);
   Put_Line ("Some overlap: " & Sum2'Image);
   Close (My_File);
end Day4;
