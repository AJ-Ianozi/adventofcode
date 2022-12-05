with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
procedure Day5 is
   woo : exception;
   --  This will be our "stack"
   package Stack is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type                                         => Character);
   use Stack;
   procedure Push (S : in out Stack.Vector; C : Character) is
   begin
      Stack.Append (S, New_Item => C);
   end Push;
   function Top (S : Stack.Vector) return Character is
   begin
      return Stack.Last_Element (S);
   end Top;
   function Pop (S : in out Stack.Vector) return Character is
      Result : constant Character := Top (S);
   begin
      Stack.Delete_Last (S);
      return Result;
   end Pop;

   type Action is record
      Count : Natural;
      To    : Natural;
      From  : Natural;
   end record;

   --  This will hold our list of stacks.
   package Row is new Ada.Containers.Vectors (Index_Type => Natural,
      Element_Type                                       => Stack.Vector);
   use Row;

   Stacks  : Row.Vector;
   Stacks2 : Row.Vector;

   My_File : File_Type;
begin
   Open (My_File, In_File, "input/Day5.txt");
   --  First, fill up the stack.
   Fill_Stack :
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String  := Get_Line (My_File);
         Max_Rows  : constant Natural := (Next_Line'Length + 1) / 4;
         --  The length (plus an extra space) is divisible by 4
         Cursor : Natural := Next_Line'First;
         Rows   : Natural := 0;
      begin
         --  Exit this loop after building the stack.
         exit Fill_Stack when Next_Line (Next_Line'First) /= '['
           and then Next_Line'Length >= 2 and then Next_Line (2) = '1';
         if Stacks.Is_Empty then
            --  Fill our rows.
            for I in 1 .. Max_Rows loop
               declare
                  --  Create new stack.
                  S  : Stack.Vector;
                  S2 : Stack.Vector;
               begin
                  --  Add it to our Stacks vector
                  Stacks.Append (S);
                  Stacks2.Append (S2);
               end;
            end loop;
         end if;
         while Cursor < Next_Line'Length loop
            declare
               --  Get either 'X' or ' ' in '[X]' or '   '
               Char : constant Character := Next_Line (Cursor + 1);
            begin
               if Char /= ' ' then
                  --  This is a character to put on N stack.
                  Push (Stacks (Rows), Char);
                  Push (Stacks2 (Rows), Char);
               end if;
               Cursor := Cursor + 4;
               Rows   := Rows + 1;
            end;
         end loop;
      end;
   end loop Fill_Stack;
   --  Next, we quickly reverse our stacks.
   for I of Stacks loop
      Stack.Reverse_Elements (I);
   end loop;
   for I of Stacks2 loop
      Stack.Reverse_Elements (I);
   end loop;
   --  Finally, we process the moves.
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String := Get_Line (My_File);
      begin
         if Next_Line'Length > 0 and then Next_Line (1) = 'm' then
            declare
               Count : constant Natural :=
                 Natural'Value
                   (Trim
                      (Next_Line
                         (Index (Next_Line, "move") + 4 ..
                              Index (Next_Line, "from") - 1),
                       Both));
               From : constant Natural :=
                 Natural'Value
                   (Trim
                      (Next_Line
                         (Index (Next_Line, "from") + 4 ..
                              Index (Next_Line, "to") - 1),
                       Both)) - 1;
               To : constant Natural :=
                 Natural'Value
                   (Trim
                      (Next_Line
                         (Index (Next_Line, "to") + 2 .. Next_Line'Last),
                       Both)) - 1;
            begin
               --  Day 5-1 Loop through our moves.
               for I in 1 .. Count loop
                  declare
                     Move : constant Character := Pop (Stacks (From));
                  begin
                     Push (Stacks (To), Move);
                  end;
               end loop;
               --  Day 5-2 do moves in bulk (in a really poor way)
               declare
                  Tmp : Stack.Vector;
               begin
                  --  Fill up the temp stack with Count crates on stack From
                  for I in 1 .. Count loop
                     declare
                        Move : constant Character := Pop (Stacks2 (From));
                     begin
                        Push (Tmp, Move);
                     end;
                  end loop;
                  --  Now put the items in the To stack.
                  for I in 1 .. Count loop
                     declare
                        Move : constant Character := Pop (Tmp);
                     begin
                        Push (Stacks2 (To), Move);
                     end;
                  end loop;
               end;
            end;
         end if;
      end;
   end loop;
   Close (My_File);
   --  Print the top of each row.
   Put ("Result for Day5-1: ");
   for I of Stacks loop
      Put (Top (I));
   end loop;
   New_Line;
   Put ("Result for Day5-2: ");
   for I of Stacks2 loop
      Put (Top (I));
   end loop;
   New_Line;
end Day5;
