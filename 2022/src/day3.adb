with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
procedure Day3 is

   type Character_Set is array (Character range 'A' .. 'z') of Natural;

   Lookup_Score : constant Character_Set :=
     ('a' => 1, 'b' => 2, 'c' => 3, 'd' => 4, 'e' => 5, 'f' => 6, 'g' => 7,
      'h' => 8, 'i' => 9, 'j' => 10, 'k' => 11, 'l' => 12, 'm' => 13,
      'n' => 14, 'o' => 15, 'p' => 16, 'q' => 17, 'r' => 18, 's' => 19,
      't' => 20, 'u' => 21, 'v' => 22, 'w' => 23, 'x' => 24, 'y' => 25,
      'z' => 26, 'A' => 27, 'B' => 28, 'C' => 29, 'D' => 30, 'E' => 31,
      'F' => 32, 'G' => 33, 'H' => 34, 'I' => 35, 'J' => 36, 'K' => 37,
      'L' => 38, 'M' => 39, 'N' => 40, 'O' => 41, 'P' => 42, 'Q' => 43,
      'R' => 44, 'S' => 45, 'T' => 46, 'U' => 47, 'V' => 48, 'W' => 49,
      'X' => 50, 'Y' => 51, 'Z' => 52, others => 0);

   type Character_Block is record
      Set        : Character_Set := (others => 0);
      Iterations : Natural       := 0;
   end record;

   procedure Process (Block : in out Character_Block; Str : String) is
   begin
      for C of Str loop
         if Block.Set (C) = Block.Iterations then
            Block.Set (C) := Block.Set (C) + 1;
         end if;
      end loop;
      Block.Iterations := Block.Iterations + 1;
   end Process;

   function Sum (Block : Character_Block) return Natural is
      Sum : Natural := 0;
   begin
      for C in Block.Set'Range loop
         if Block.Set (C) > Block.Iterations-1 then
            Sum := Sum + Lookup_Score (C);
         end if;
      end loop;
      return Sum;
   end Sum;

   --  Day 3-1 sum
   Sum1 : Natural := 0;

   --  Day 3-2 sum
   Sum2 : Natural := 0;
   --  Day 3-2 stuff
   Cursor     : Natural                            := 1;
   Next_Three : array (1 .. 3) of Unbounded_String :=
     (others => Null_Unbounded_String);

   My_File : File_Type;
begin
   Open (My_File, In_File, "input/Day3-1.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String := Get_Line (My_File);
         --  Split left and right lines
         Left : constant String :=
           Next_Line (Next_Line'First .. Next_Line'Last / 2);
         Right : constant String :=
           Next_Line (Left'Last + 1 .. Next_Line'Last);
         --  For day3-1
         Block : Character_Block;
      begin
         --  For day3-1
         Process (Block, Left);
         Process (Block, Right);
         Sum1 := Sum1 + Sum (Block);
         --  For day3-2:
         --  Copy the current line into our next-three array
         Next_Three (Cursor) := To_Unbounded_String (Next_Line);
         --  Was this our third entry?
         if Cursor = 3 then
            declare
               Tmp_Block : Character_Block;
            begin
               for I in Next_Three'Range loop
                  Process (Tmp_Block, To_String (Next_Three (I)));
               end loop;
               Sum2 := Sum2 + Sum (Tmp_Block);
            end;
            --  Set cursor back to 1.
            Cursor := 1;
         else
            Cursor := Cursor + 1;
         end if;
      end;
   end loop;
   Put_Line ("Total priorities for day 3-1 are:" & Sum1'Image);
   Put_Line ("Total priorities for day 3-2 are:" & Sum2'Image);
   Close (My_File);
end Day3;
