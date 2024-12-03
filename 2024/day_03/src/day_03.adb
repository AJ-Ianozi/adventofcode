pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with GNAT.Regexp; use GNAT.Regexp;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure Day_03 is
   Input : constant String := "input.txt";
   F     : File_Type;
   --  This regex catches do/don't/mul(x,y)
   Re : constant Pattern_Matcher :=
         Compile ("(don\'t\(\)|do\(\)|mul\((\d{1,3}),(\d{1,3})\))");
   Result : Natural := 0;
   Result_2 : Natural := 0;
   Enabled : Boolean := True;
begin
   Open (F, In_File, Input);
   while not End_Of_File (F) loop
      declare
         Line    : constant String := Get_Line (F);
         Parens  : constant Match_Count := Paren_Count (Re);
         Matches : Match_Array (0 .. Parens); -- Will hold our matches
         Cur     : Natural := Line'First;
         A, B    : Natural;
         Product : Natural;
      begin
         --  Crawl the line looking for matches
         while Cur < Line'Last loop
            Match (Re, Line (Cur .. Line'Last), Matches);
            exit when Matches (0) = No_Match;
            --  Check the matches
            if Line (Matches (0).First .. Matches (0).Last) = "don't()" then
               --  Stop processing multiplication for pt 2
               Enabled := False;
            elsif Line (Matches (0).First .. Matches (0).Last) = "do()" then
               --  Continue processing multiplication for pt 2
               Enabled := True;
            else
               --  Multiply the two values
               A := Natural'Value (Line (Matches (Parens-1).First ..
                                         Matches (Parens-1).Last));
               B := Natural'Value (Line (Matches (Parens).First ..
                                         Matches (Parens).Last));
               Product := A * B;
               --  Part 1
               Result := @ + Product;
               --  Part 2
               if Enabled then
                  Result_2 :=  @ + Product;
               end if;
            end if;
            --  Continue searching the string slice
            Cur := Matches (0).Last + 1;
         end loop;
      end;
   end loop;
   Close (F);
   Put_Line (Result'Image);
   Put_Line (Result_2'Image);
end Day_03;
