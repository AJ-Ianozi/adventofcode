--  For https://adventofcode.com/2023/day/1
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Maps;      use Ada.Strings.Maps;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  For part 2
with GNAT.Regexp; use GNAT.Regexp;
with GNAT.Regpat; use GNAT.Regpat;

procedure Day_01 is
   --  For part 1 I just dropped it in a character set.
   procedure Part_1 is
      Numbers : constant Character_Set := To_Set (Sequence => "0123456789");
      Sum     : Natural := 0;
      F       : File_Type;
   begin
      --  Open the file
      Open (F, In_File, "input.txt");
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            --  Get index of first number in string
            I1 : constant Natural :=
               Index (Source => Line,
                     Set => Numbers,
                     Test => Inside,
                     Going => Forward);
            --  Get index of last number in string
            I2 : constant Natural :=
               Index (Source => Line,
                     Set => Numbers,
                     Test => Inside,
                     Going => Backward);
            --  Use this to create the actual number
            Number : constant Natural :=
               Natural'Value (Line (I1 .. I1)) * 10 +
               Natural'Value (Line (I2 .. I2));
         begin
            Sum := Sum + Number;
         end;
      end loop;
      Close (F);
      Put_Line ("Result: " & Sum'Image);
   end Part_1;

   --  For part 2, I decided to mess with regexes
   procedure Part_2 is
      --  To quickly convert a string to numerical value
      function String_To_Natural (Str : String) return Natural
      is
         Numbers : constant Character_Set := To_Set (Sequence => "0123456789");
         type Lookup is
            (one, two, three, four, five, six, seven, eight, nine);
         Idx : constant Natural := Index (Source => Str,
                                          Set => Numbers,
                                          Test => Inside,
                                          Going => Forward);
      begin
         return
            (if Idx > 0 then
               Natural'Value (Str (Idx .. Idx))
               else
               (case Lookup'Value (Str) is
                  when one   => 1,
                  when two   => 2,
                  when three => 3,
                  when four  => 4,
                  when five  => 5,
                  when six   => 6,
                  when seven => 7,
                  when eight => 8,
                  when nine  => 9));
      end String_To_Natural;

      --  Our regex
      Re : constant Pattern_Matcher :=
         Compile ("(0|1|2|3|4|5|6|7|8|9|" &
                  "zero|one|two|three|four|five|six|seven|eight|nine)");

      Sum : Natural := 0;
      F   : File_Type;
   begin
      --  Open the file
      Open (F, In_File, "input.txt");
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            --  This will hold our match
            Matches : Match_Array (0 .. 0);
            --  For crawling the matches
            Current : Natural := Line'First;
            --  To quickly store the first and last match string
            First_Match : Unbounded_String := Null_Unbounded_String;
            Last_Match  : Unbounded_String := Null_Unbounded_String;
         begin
            loop
               --  Crawl through the matches
               Match (Re, Line, Matches, Current);
               exit when Matches (0) = No_Match;
               --  If this is the first match
               if Current = Line'First then
                  First_Match :=
                     To_Unbounded_String
                        (Line (Matches (0).First .. Matches (0).Last));
               end if;
               --  Set most recent match
               Last_Match :=
                  To_Unbounded_String
                     (Line (Matches (0).First .. Matches (0).Last));
               Current := Current + 1;
            end loop;
            --  Use this to create the actual number
            declare
               Number : constant Natural :=
                  String_To_Natural (To_String (First_Match)) * 10 +
                  String_To_Natural (To_String (Last_Match));
            begin
               Sum := Sum + Number;
            end;
         end;
      end loop;
      Close (F);
      Put_Line ("Result: " & Sum'Image);
   end Part_2;

begin
   Part_1;
   Part_2;
end Day_01;
