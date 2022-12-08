with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Containers.Multiway_Trees;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings;           use Ada.Strings;

procedure Day7 is

   type Fake_Types is (File, Directory);

   type Fake_File is record
      Name  : Unbounded_String := Null_Unbounded_String;
      T     : Fake_Types       := File;
      Value : Natural          := 0;
   end record;

   package Trees is new Ada.Containers.Multiway_Trees (Fake_File);
   use Trees;
   My_File : File_Type;

   My_Tree : Tree;
   Curse   : Cursor := My_Tree.Root;

   function My_Find (C : Cursor; Match : String) return Cursor is
      Tmp : Cursor := First_Child (Curse);
   begin
      loop
         if Has_Element (Tmp) and then Match = To_String (Element (Tmp).Name)
         then
            return Tmp;
         end if;
         if Tmp = Last_Child (C) then
            exit;
         else
            Tmp := Next_Sibling (Tmp);
         end if;
      end loop;
      return C;
   end My_Find;

   procedure Sum (C : Cursor) is
   begin
      for TmpC in My_Tree.Iterate_Children (C) loop
         Sum (TmpC);
      end loop;
      --  Sum the parent.
      if not Is_Root (C) and then Has_Element (Parent (C)) then
         declare
            P : constant Cursor := Parent (C);
            F : Fake_File       := Element (P);
         begin
            F.Value := F.Value + Element (C).Value;
            My_Tree.Replace_Element (P, F);
         end;
      end if;
   end Sum;

   Result : Natural := 0;

   Total_Available : constant Natural := 70_000_000;
   Update_Space    : constant Natural := 30_000_000;

begin
   Open (My_File, In_File, "input/Day7.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line      : constant String := Get_Line (My_File);
         First_Argument : constant String :=
           Trim
             (Next_Line (Next_Line'First .. Index (Next_Line, " ") - 1), Both);
      begin
         if First_Argument = "$" then --  Command
            declare
               Command : constant String :=
                 Trim
                   (Next_Line (Next_Line'First + 2 .. Next_Line'First + 3),
                    Both);
            begin
               if Command = "cd" then --  Prepare to move to another node.
                  declare
                     Argument : constant String :=
                       Trim
                         (Next_Line (Next_Line'First + 4 .. Next_Line'Last),
                          Both);
                  begin
                     if Argument = "/" then --  Create our "root" node.
                        declare
                           F : constant Fake_File :=
                             (Name => To_Unbounded_String ("/"),
                              T    => Directory, Value => 0);
                        begin
                           My_Tree.Append_Child
                             (Parent => Curse, New_Item => F);
                           Curse := My_Find (Curse, "/");
                        end;
                     elsif Argument = ".." then --  Set our curser to parent.
                        Curse := Parent (Curse);
                     else --  Find node with the cursor we need.
                        Curse := My_Find (Curse, Argument);
                     end if;
                  end;
               elsif Command = "ls" then --  prepare for node capturing.
                  null;
               end if;
            end;
         else -- It must be a file listing
            declare
               Filename : constant String :=
                 Trim
                   (Next_Line (Index (Next_Line, " ") + 1 .. Next_Line'Last),
                    Both);
               F : constant Fake_File :=
                 (if First_Argument = "dir" then
                    (Name => To_Unbounded_String (Filename), Value => 0,
                     T    => Directory)
                  else
                    ((Name  => To_Unbounded_String (Filename),
                      Value => Natural'Value (First_Argument), T => File)));
            begin
               My_Tree.Append_Child (Parent => Curse, New_Item => F);
            end;
         end if;
      end;
   end loop;
   Close (My_File);
   --  Go through the tree, summing all the directories. Bad way to do this :)
   Sum (My_Tree.Root);
   for E of My_Tree loop
      if E.T = Directory and then E.Value <= 100_000 then
         Result := Result + E.Value;
      end if;
   end loop;
   Put_Line ("Part 1 result is " & Result'Image);
   --  Find the smallest one that we can delete.
   Result := 0;

   declare
      --  First child element holds "/" directory, with full size
      Smallest_One    : Natural := First_Child_Element (My_Tree.Root).Value;
      Available_Space : constant Natural := Total_Available - Smallest_One;
   begin
      for E of My_Tree loop
         if E.T = Directory and then E.Value < Smallest_One
           and then Available_Space + E.Value > Update_Space
         then
            Smallest_One := E.Value;
         end if;
      end loop;
      Put_Line ("Part 2 result is " & Smallest_One'Image);
   end;
end Day7;
