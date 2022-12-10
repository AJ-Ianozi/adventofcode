with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
procedure Day9 is
   My_File : File_Type;
   type Coord is record
      X, Y : Integer := 0;
   end record;
   function "<" (Left, Right : Coord) return Boolean is
   begin
      return
        Left.Y < Right.Y or else (Left.Y = Right.Y and then Left.X < Right.X);
   end "<";
   overriding function "=" (Left, Right : Coord) return Boolean is
   begin
      return Left.X = Right.X and then Left.Y = Right.Y;
   end "=";
   package Coords is new Ada.Containers.Ordered_Sets (Coord);
   use Coords;

   procedure Move (Head : in out Coord; Dir : Character) is
   begin
      case Dir is
         when 'U'    => Head.Y := Head.Y + 1;
         when 'D'    => Head.Y := Head.Y - 1;
         when 'R'    => Head.X := Head.X + 1;
         when 'L'    => Head.X := Head.X - 1;
         when others => raise Constraint_Error with """UDRL"" only!";
      end case;
   end Move;
   procedure Move_Towards (Head : Coord; Tail : in out Coord) is
      Y_Dist : constant Natural := abs (Head.Y - Tail.Y);
      X_Dist : constant Natural := abs (Head.X - Tail.X);
   begin
      if Y_Dist > 1 or else X_Dist > 1 then
         if Tail.Y /= Head.Y then
            --  In a different row. Move towards head.y
            Tail.Y := Tail.Y + (if Head.Y > Tail.Y then 1 else -1);
         end if;
         if Tail.X /= Head.X then
            --  In a different column.  Move towards head.x
            Tail.X := Tail.X + (if Head.X > Tail.X then 1 else -1);
         end if;
      end if;
   end Move_Towards;

   --  Pt1 and Pt2 results
   Coord_List   : Set;
   Coord_List_2 : Set;

   --  Pt1 rope's heads and tails.  Coords are currently x,y=0,0 by default.
   Head : Coord;
   Tail : Coord;

   --  Array of ropes, for pt2.
   Rope      : array (1 .. 10) of Coord;
   Rope_Head : constant Natural := Rope'First;
   Rope_Tail : constant Natural := Rope'Last;

begin
   --  Store starting point.
   Open (My_File, In_File, "input/Day9.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String    := Get_Line (My_File);
         Dir       : constant Character := Next_Line (1);
         Amount    : constant Positive  :=
           Positive'Value (Next_Line (Next_Line'First + 2 .. Next_Line'Last));
      begin
         for tmp in 1 .. Amount loop
            --  Pt 1.... Move the head and tail.
            Move (Head, Dir);
            Move_Towards (Head, Tail);
            --  First move the rope's head.
            Move (Rope (Rope_Head), Dir);
            --  Now iterate through the rest until we hit the tail.
            for I in Rope_Head .. Rope_Tail - 1 loop
               Move_Towards (Rope (I), Rope (I + 1));
            end loop;
            Coord_List.Include (Tail);
            Coord_List_2.Include (Rope (Rope_Tail));
         end loop;
      end;
   end loop;
   Close (My_File);

   Put_Line ("Pt1 result is " & Coord_List.Length'Image);
   Put_Line ("Pt2 result is " & Coord_List_2.Length'Image);

end Day9;
