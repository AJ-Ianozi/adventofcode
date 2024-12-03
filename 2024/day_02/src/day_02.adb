pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Containers.Indefinite_Vectors;
procedure Day_02 is
   --  For splitting the string.
   package String_Vector is new
     Ada.Containers.Indefinite_Vectors (Natural, String);

   --  Our array of numbers
   type Levels is array (Positive range <>) of Natural;

   Input     : constant String := "input.txt";
   F         : File_Type;
   Result    : Natural := 0;
   Result_P2 : Natural := 0;

   --  TODO: Add this to a library or something
   function Split (S : String; By : String) return String_Vector.Vector
   is
      use type Ada.Containers.Count_Type;
      Result : String_Vector.Vector;
      Cur     : Natural := S'First;
      Sp      : Natural;
   begin
      while Cur <= S'Last loop
         Sp := Ada.Strings.Fixed.Index (S (Cur .. S'Last), By);
         if Sp > 0 then
            Result.Append (S (Cur .. Sp - 1));
         else
            Result.Append (S (Cur .. S'Last));
         end if;
         exit when Sp = 0;
         Cur := Sp + 1;
      end loop;
      if Result.Length = 0 then
         Result.Append (S);
      end if;
      return Result;
   end Split;

   --  Tests if it's safe based on the rules given of an array of numbers
   function Is_Safe (T : Levels) return Boolean is
      Last_Level  : Natural;
      Cur_Level : Natural;
      Diff       : Natural;
      Increased : Boolean := False;
      Decreased : Boolean := False;
      Safe : Boolean := True;
   begin
      for I in T'First .. T'Last loop
         if I = T'First then
            Last_Level := T (I);
         else
            Cur_Level := T (I);
            --  Check if increasing or decreasing
            if Cur_Level > Last_Level then
               Increased := True;
            elsif Cur_Level < Last_Level then
               Decreased := True;
            else
               --  Neither increasing or decreasing.  Error.
               Safe := False;
            end if;
            --  If increasing and suddenly decreasing, then invalid
            if Increased and Decreased then
               Safe := False;
            end if;
            Diff := abs (Cur_Level - Last_Level);
            --  If the difference is too great or not at all, then error
            if Diff > 3 or else Diff < 1 then
               Safe := False;
            end if;
            exit when not Safe; --  Break loop on error
            Last_Level := T (I);
         end if;
      end loop;
      return Safe;
   end Is_Safe;

begin
   --  Read file, tally up errors
   Open (F, In_File, Input);
   while not End_Of_File (F) loop
      declare
         --  I need init the vector first then the array besauce I get an index
         --  error when trying:
         --   [for I of Split (Get_Line (F), " ") => Natural'Value (I)];
         Sp      : constant String_Vector.Vector := Split (Get_Line (F), " ");
         Numbers : constant Levels := [for I of Sp => Natural'Value (I)];
      begin
         --  Part 1 requires no errors
         if Is_Safe (Numbers) then
            Result := @ + 1;
         end if;
         --  Part 2 is allowed if it passes with a single level removed.
         if
            (for some N in Numbers'Range =>
               Is_Safe --  Pass a dynamically-created array into is_safe
                  ((if N = Numbers'First then
                     Numbers (N + 1 .. Numbers'Last)
                   elsif N = Numbers'Last then
                     Numbers (Numbers'First .. N - 1)
                   else
                     Numbers (Numbers'First .. N - 1) &
                     Numbers (N + 1 .. Numbers'Last))))
         then
            Result_P2 := @ + 1;
         end if;
      end;
   end loop;
   Put_Line (Result'Image);
   Put_Line (Result_P2'Image);
   Close (F);
end Day_02;
