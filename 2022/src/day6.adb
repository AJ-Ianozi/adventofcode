with Ada.Text_IO; use Ada.Text_IO;

procedure Day6 is

   function Count_Packets (Unique_Req : Natural) return Natural is

      type Buffer is array (Natural range 0 .. (Unique_Req - 1)) of Character;

      subtype Bound_Range is Natural range 0 .. (Unique_Req - 1);

      function Unique (Item : Buffer) return Boolean is
      begin
         for I in Item'Range loop
            declare
               Char : constant Character := Item (I);
            begin
               for J in (I + 1) .. (Item'Last) loop
                  if Char = Item (J) then
                     return False;
                  end if;
               end loop;
            end;
         end loop;
         return True;
      end Unique;

      function Next_Char (Item : File_Type) return Character is
         Result : Character;
      begin
         Get (Item, Result);
         return Result;
      end Next_Char;

      Buff  : Buffer      := (others => '-');
      Index : Bound_Range := 0;
      Count : Natural     := Unique_Req;

      My_File : File_Type;
   begin
      Open (My_File, In_File, "input/Day6.txt");
      --  Read in the X four characters to fill our buffer.
      for I in Buffer'Range loop
         Buff (I) := Next_Char (My_File);
      end loop;
      --   Process characters until buffer is no longer unique.
      while not End_Of_File (My_File) loop
         exit when Unique (Buff);
         Buff (Index) := Next_Char (My_File);
         Count        := Count + 1;
         Index        :=
           (if Index = Bound_Range'Last then Bound_Range'First else Index + 1);
      end loop;
      Close (My_File);
      return Count;
   end Count_Packets;

   Result_P1 : constant Natural := Count_Packets (4);
   Result_P2 : constant Natural := Count_Packets (14);
begin
   Put_Line ("Result Pt1 is: " & Result_P1'Image);
   Put_Line ("Result Pt2 is: " & Result_P2'Image);

end Day6;
