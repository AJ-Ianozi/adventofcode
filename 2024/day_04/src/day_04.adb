pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Vectors;
procedure Day_04 is
   package String_Vector is new
     Ada.Containers.Indefinite_Vectors (Positive, String);

   type Grid is array (Positive range <>, Positive range <>) of Character;

   function From_Vector (Item : String_Vector.Vector) return Grid
   with pre => Item.Length > 0 and then
               (for all X of Item => X'Length = Item.First_Element'Length);

   function From_Vector (Item : String_Vector.Vector) return Grid
   is
      Result_Width  : constant Natural := Item.First_Element'Length;
      Result_Height : constant Natural := Natural (Item.Length);
      Result        : constant Grid (1 .. Result_Width, 1 .. Result_Height) :=
                        [for Y in 1 .. Result_Height =>
                           [for X in 1 .. Result_Width =>
                                             Item.Element (Y)(X)]];
   begin
      return Result;
   end From_Vector;

   function My_Count (S : String; Pattern : String) return Natural is
      Cur    : Natural := S'First;
      Next   : Natural := 0;
      Result : Natural := 0;
   begin
      while Cur < S'Last and then  S (Cur .. S'Last)'Length >= Pattern'Length loop
         Next := Index (S (Cur .. S'Last), Pattern);
         --  Break loop if result was not found.
         exit when Next = 0;
         --  result was found.
         Result := @ + 1;
         Cur := Next + Pattern'Length;
      end loop;
      return Result;
   end My_Count;

   function Count (G : Grid;
                   Pattern : String) return Natural is
      Result : Natural := 0;
      --  First go across each line.
   begin
      --  First count across each line
      for Row in G'range (1) loop
         declare
            Next : String := [for Col in G'range (2) => G (Row, Col)];
         begin
            Result := @ + My_Count (Next, Pattern);
         end;
      end loop;
      --  Then do it backwards
      for Row in G'range (1) loop
         declare
            Next : String :=
               [for Col in G'range (2) =>
                  G ((G'Last (1) - Row) + G'First (1),
                     (G'Last (2) -Col) + G'First (2))];
         begin
            Result := @ + My_Count (Next, Pattern);
         end;
      end loop;

      --  Now search top to bottom
      for Col in G'range (2) loop
         declare
            Next : String := [for Row in G'range (1) => G (Row, Col)];
         begin
            Result := @ + My_Count (Next, Pattern);
         end;
      end loop;

      --  Now bottom to top
      for Col in G'range (2) loop
         declare
            Next : String :=
               [for Row in G'range (1) =>
                  G ((G'Last (1) - Row) + G'First (1),
                     (G'Last (2) -Col) + G'First (2))];
         begin
            Result := @ + My_Count (Next, Pattern);
         end;

      end loop;

      --  Now search diagnal, going this direction: \
      for Col in G'First (2) .. G'Last (2) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (Col + Col_2) <= G'Last (2) and then
                     (G'First (1) + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((G'First (1) + Col_2), (Col + Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  We captured 1 the first time around
      for Row in (G'First (1) + 1) .. G'Last (1) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (G'First (2)  + Col_2) <= G'Last (2) and then
                     (Row + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((Row + Col_2), (G'First (2)  + Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  Now search diagnal, going left-down
      for Col in reverse G'First (2) .. G'Last (2) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (Col - Col_2) >= G'First (2) and then
                     (G'First (1) + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((G'First (1) + Col_2), (Col - Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  We captured 1 the first time around
      for Row in (G'First (1) + 1) .. G'Last (1) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (G'Last (2) - Col_2) >= G'First (2) and then
                     (Row + Col_2) <= G'Last (1)
            loop
               Next := Next & G ((Row + Col_2), (G'Last (2) - Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  Now search diagnal, going left-up
      for Col in reverse G'First (2) .. G'Last (2) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in reverse G'First (2) - 1 .. G'Last (2) - 1
               when (Col - Col_2) >= G'First (2) and then
                     (G'First (1) + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((G'First (1) + Col_2), (Col - Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      for Col in G'First (2) + 1 .. G'Last (2) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (Col + Col_2) <= G'Last (2) and then
                     (G'Last (1) - Col_2) >= G'First (2)
            loop
               Next := Next & G ((G'Last (1) - Col_2), (Col + Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  Now search diagnal, going right-up
      for Col in G'First (2) .. G'Last (2) loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in reverse G'First (2) - 1 .. G'Last (2) - 1
               when (Col + Col_2) <= G'Last (2) and then
                     (G'First (1) + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((G'First (1) + Col_2), (Col + Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      --  Now search diagnal, going right-up
      for Col in reverse G'First (2) .. G'Last (2) - 1 loop
         declare
            --  Have to do this because 'when' isn't supported in
            --  array aggregates
            Next : Unbounded_String := Null_Unbounded_String;
         begin
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (Col - Col_2) >= G'First (2) and then
                     (G'Last (1) - Col_2) >= G'First (2)
            loop
               Next := Next & G ((G'Last (1) - Col_2), (Col - Col_2));
            end loop;
            Result := @ + My_Count (To_String (Next), Pattern);
         end;
      end loop;

      return Result;
   end Count;

   --  It's December 5th so I'm running behind.
   --  Not going to do anything fancy for pt 2.
   --  Please don't read this code :(
   function Contains_Cross (G : Grid; Pattern : String) return Boolean is
      Found : Boolean := False;
      Next : Unbounded_String;
   begin
      --  Criss: down-\
      Next := Null_Unbounded_String;
      for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
         when (G'First (2) + Col_2) <= G'Last (2) and then
               (G'First (1) + Col_2) <= G'Last (2)
      loop
         Next := Next & G ((G'First (1) + Col_2), (G'First (2) + Col_2));
      end loop;
      if My_Count (To_String (Next), Pattern) > 0 then
         Found := True;
      else
         --  We didn't find mas going in one direction. Check the other
         --  Criss up-\
         Next := Null_Unbounded_String;
         for Col_2 in reverse G'First (2) - 1 .. G'Last (2) - 1
            when (G'First (2) + Col_2) <= G'Last (2) and then
                  (G'First (1) + Col_2) <= G'Last (2)
         loop
            Next := Next & G ((G'First (1) + Col_2), (G'First (2) + Col_2));
         end loop;
         Found := My_Count (To_String (Next), Pattern) > 0;
      end if;

      if Found then
         --  Cross up /
         Next := Null_Unbounded_String;
         for Col_2 in reverse G'First (2) - 1 .. G'Last (2) - 1
            when ( G'Last (2) - Col_2) >= G'First (2) and then
                  (G'First (1) + Col_2) <= G'Last (2)
         loop
            Next := Next & G ((G'First (1) + Col_2), ( G'Last (2) - Col_2));
         end loop;
         if My_Count (To_String (Next), Pattern) > 0 then
            Found := True;
         else
            --  Cross down /
            Next := Null_Unbounded_String;
            for Col_2 in G'First (2) - 1 .. G'Last (2) - 1
               when (G'Last (2) - Col_2) >= G'First (2) and then
                     (G'First (1) + Col_2) <= G'Last (2)
            loop
               Next := Next & G ((G'First (1) + Col_2), (G'Last (2) - Col_2));
            end loop;
            Found := My_Count (To_String (Next), Pattern) > 0;
         end if;
      end if;
      return Found;
   end Contains_Cross;

   function Count_X_Mas (G : Grid) return Natural is
      X_MAS : constant String := "MAS";
      Result : Natural := 0;
   begin
      for
         Row in G'First (1) .. G'Last (1)
            when (Row + 2) <= G'Last (1)
      loop
         for Col in G'First (2) .. G'Last (2)
            when (Col + 2) <= G'Last (2)
         loop
            declare
               Next_G : constant Grid (1 .. 3, 1 .. 3) :=
                  [for Row_N in Row .. (Row + 2) =>
                     [for Col_N in Col .. (Col + 2) =>
                        G (Row_N, Col_N)]];
            begin
               end loop;
               --  ...please don't judge me too harsely
               if Contains_Cross (Next_G, X_MAS) then
                  Result := @ + 1;
               end if;
            end;
         end loop;
      end loop;
      return Result;
   end Count_X_Mas;

   Input     : constant String := "input.txt";
   Result_P1 : Natural := 0;
   Result_P2 : Natural := 0;

begin
   declare
      F      : File_Type;
      Buffer : String_Vector.Vector;
   begin
      --  Read file into buffer
      Open (F, In_File, Input);
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
         begin
            Buffer.Append (Line);
         end;
      end loop;
      Close (F);
      declare
         Our_Grid : constant Grid := From_Vector (Buffer);
      begin
         Result_P1 := Count (Our_Grid, "XMAS");
         Result_P2 := Count_X_Mas (Our_Grid);
      end;
   end;
   Put_Line (Result_P1'Image);
   Put_Line (Result_P2'Image);
end Day_04;
