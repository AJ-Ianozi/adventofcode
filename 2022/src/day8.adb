with Ada.Text_IO; use Ada.Text_IO;

procedure Day8 is
   type Forest is array (Positive range <>, Positive range <>) of Natural;
   type Direction is (Up, Down, Left, Right);
   function Initiate return Forest is
      My_File : File_Type;
      Columns : Positive;
      Rows    : Positive := 1;
   begin
      --  First figure out the length and width of our forest. This is the easy
      --  way for me to do it.
      Open (My_File, In_File, "input/Day8.txt");
      Columns := Get_Line (My_File)'Length;
      while not End_Of_File (My_File) loop
         Skip_Line (My_File);
         Rows := Rows + 1;
      end loop;
      Close (My_File);
      declare
         --  Create our forest based on the columns and rows.
         Result : Forest (1 .. Rows, 1 .. Columns) :=
           (others => (others => 0));
         Row_Count : Positive := 1;
      begin
         --  Now read it into the matrix.
         Open (My_File, In_File, "input/Day8.txt");
         while not End_Of_File (My_File) loop
            declare
               Next_Line    : constant String := Get_Line (My_File);
               Column_Count : Positive        := 1;
            begin
               for I in Next_Line'Range loop
                  declare
                     --  Because Natural'Value doesn't work on characters
                     Value : constant String := Next_Line (I .. I);
                  begin
                     Result (Row_Count, Column_Count) := Natural'Value (Value);
                     Column_Count                     := Column_Count + 1;
                  end;
               end loop;
               Row_Count := Row_Count + 1;
            end;
         end loop;
         Close (My_File);
         return Result;
      end;
   end Initiate;
   --  The very ugly processing function.
   procedure Process
     (Trees    : Forest; Row : Positive; Column : Positive; Dir : Direction;
      Distance : out Positive; Is_Visible : out Boolean)
   is
      Search_Start : constant Positive :=
        (case Dir is when Up => Trees'First (1), when Down => Row + 1,
           when Left         => Trees'First (2), when Right => Column + 1);
      Search_End : constant Positive :=
        (case Dir is when Up => Row - 1, when Down => Trees'Last (1),
           when Left         => Column - 1, when Right => Trees'Last (2));
      Counter : Positive := 1;
   begin
      --  This is for pt2
      Distance :=
        (case Dir is when Up | Left => Search_End,
           when Down => Search_End - Row, when Right => Search_End - Column);
      case Dir is
         when Left => --  To the left
            for Tmp_Column in reverse Search_Start .. Search_End loop
               if Trees (Row, Tmp_Column) >= Trees (Row, Column) then
                  Distance   := Counter;
                  Is_Visible := False;
                  return;
               end if;
               Counter := Counter + 1;
            end loop;
         when Right => --  Take it back now, y'all
            for Tmp_Column in Search_Start .. Search_End loop
               if Trees (Row, Tmp_Column) >= Trees (Row, Column) then
                  Distance   := Counter;
                  Is_Visible := False;
                  return;
               end if;
               Counter := Counter + 1;
            end loop;
         when Up => --  Go up this time
            for Tmp_Row in reverse Search_Start .. Search_End loop
               if Trees (Tmp_Row, Column) >= Trees (Row, Column) then
                  Distance   := Counter;
                  Is_Visible := False;
                  return;
               end if;
               Counter := Counter + 1;
            end loop;
         when Down => --  How low can you go?
            for Tmp_Row in Search_Start .. Search_End loop
               if Trees (Tmp_Row, Column) >= Trees (Row, Column) then
                  Distance   := Counter;
                  Is_Visible := False;
                  return;
               end if;
               Counter := Counter + 1;
            end loop;
      end case;
      Is_Visible := True;
   end Process;
   --  Wrap if visible or not. For pt1.
   function Visible
     (Trees : Forest; Row : Positive; Column : Positive; Dir : Direction)
      return Boolean
   is
      Scrap  : Positive := 1;
      Result : Boolean;
   begin
      Process (Trees, Row, Column, Dir, Scrap, Result);
      return Result;
   end Visible;
   --  Wraps the distance, for part 2.
   function Get_Distance
     (Trees : Forest; Row : Positive; Column : Positive; Dir : Direction)
      return Positive
   is
      Result : Positive := 1;
      Scrap  : Boolean;
   begin
      Process (Trees, Row, Column, Dir, Result, Scrap);
      return Result;
   end Get_Distance;
   --  See the forest from the trees.
   Trees       : constant Forest   := Initiate;
   Last_Row    : constant Positive := Trees'Last (1);
   Last_Column : constant Positive := Trees'Last (2);
   --  Amount of visible trees, for pt1
   Visible_Trees : Natural := 0;
   --  Highest scoring senic index for pt2
   Scenic_Index : Natural := 0;

begin
   for Row in Trees'Range (1) loop
      for Column in Trees'Range (2) loop
         declare
            --  Assume it's visible unless proven otherwise.
            Is_Visible : Boolean := True;
         begin
            --  Make sure we're not on an edge.
            if Column /= 1 and then Column /= Last_Column and then Row /= 1
              and then Row /= Last_Row
            then
               --  Continue searching if all rows are visible for pt1
               if not Visible (Trees, Row, Column, Up)
                 and then not Visible (Trees, Row, Column, Down)
                 and then not Visible (Trees, Row, Column, Left)
                 and then not Visible (Trees, Row, Column, Right)
               then
                  Is_Visible := False;
               end if;
               --  Calculate scenic index for pt2... this is inefficient I know
               declare
                  Tmp : constant Natural :=
                    Get_Distance (Trees, Row, Column, Up) *
                    Get_Distance (Trees, Row, Column, Down) *
                    Get_Distance (Trees, Row, Column, Left) *
                    Get_Distance (Trees, Row, Column, Right);
               begin
                  if Tmp > Scenic_Index then
                     Scenic_Index := Tmp;
                  end if;
               end;
            end if;
            if Is_Visible then
               Visible_Trees := Visible_Trees + 1;
            end if;
         end;
      end loop;
   end loop;
   Put_Line ("Visible trees are " & Visible_Trees'Image);
   Put_Line ("Highest scenic index is " & Scenic_Index'Image);

end Day8;
