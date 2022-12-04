with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

procedure Day2 is

   Invalid_Character, Invalid_Truth : exception;

   type Action is (Rock, Paper, Scissors);
   type Outcomes is (Win, Lose, Draw);

   --  This gives us all possible results.
   type Truths is array (Action'Range) of Outcomes;
   TruthTable : constant array (Action'Range) of Truths :=
     (Rock     => (Rock => Draw, Paper => Lose, Scissors => Win),
      Paper    => (Rock => Win, Paper => Draw, Scissors => Lose),
      Scissors => (Rock => Lose, Paper => Win, Scissors => Draw));

   --  This holds the results of each round.
   type Round is record
      Me     : Action;
      Them   : Action;
      Result : Outcomes;
   end record;

   --  To quickly calculate your base score.
   function Base_Score (A : Action) return Natural is
   begin
      case A is
         when Rock =>
            return 1;
         when Paper =>
            return 2;
         when Scissors =>
            return 3;
      end case;
   end Base_Score;

   --  To calculate the results based on an outcome.
   function Result_Score (R : Outcomes) return Natural is
   begin
      case R is
         when Lose =>
            return 0;
         when Draw =>
            return 3;
         when Win =>
            return 6;
      end case;
   end Result_Score;

   --  Calculate what I need to get the desired outcome.
   function What_I_Need (Them : Action; Need : Outcomes) return Action is
      --  We're doing a reverse lookup, so find the "reverse" of outcome.
      Rev_Outcome : constant Outcomes :=
        (case Need is when Win => Lose, when Lose => Win, when Draw => Draw);
   begin
      --  I was hoping to use iterators for this, but couldn't get the index!
      for T in TruthTable (Them)'Range loop
         if TruthTable (Them) (T) = Rev_Outcome then
            return T;
         end if;
      end loop;
      raise Invalid_Truth with "We should never get here.";
   end What_I_Need;

   --  Plays the round.  Only really useful for Day-2-1
   function Set_Round_Day2_1 (Test : String) return Round is
      --  Grab the values from the string.
      Split : constant Integer   := Index (Test, " ");
      Them : constant Character := Trim (Test (Test'First .. Split), Both) (1);
      Me    : constant Character := Trim (Test (Split .. Test'Last), Both) (1);
      --  Decodes the character, and sets what each one selected.
      Me_A : constant Action :=
        (case Me is when 'X' => Rock, when 'Y' => Paper, when 'Z' => Scissors,
           when others       => raise Invalid_Character);
      Them_A : constant Action :=
        (case Them is when 'A' => Rock, when 'B' => Paper,
           when 'C' => Scissors, when others => raise Invalid_Character);
      --  Play the round, looking up the result in the truth table.
      Result : constant Round :=
        (Me => Me_A, Them => Them_A, Result => TruthTable (Me_A) (Them_A));
   begin
      return Result;
   end Set_Round_Day2_1;

   --  Plays the round for Day-2-2
   function Set_Round_Day2_2 (Test : String) return Round is
      --  Grab the values from the string.
      Split : constant Integer   := Index (Test, " ");
      Them : constant Character := Trim (Test (Test'First .. Split), Both) (1);
      Me    : constant Character := Trim (Test (Split .. Test'Last), Both) (1);
      --  Decodes the character, and sets what each one selected.
      Me_O : constant Outcomes :=
        (case Me is when 'X' => Lose, when 'Y' => Draw, when 'Z' => Win,
           when others       => raise Invalid_Character);
      Them_A : constant Action :=
        (case Them is when 'A' => Rock, when 'B' => Paper,
           when 'C' => Scissors, when others => raise Invalid_Character);
      Me_A : constant Action := What_I_Need (Them_A, Me_O);
      --  Play the round, looking up the result in the truth table.
      Result : constant Round :=
        (Me => Me_A, Them => Them_A, Result => TruthTable (Me_A) (Them_A));
   begin
      return Result;
   end Set_Round_Day2_2;

   My_File : File_Type;
   --  Day-2-1 score
   Total_Score : Natural := 0;
   --  Day 2-2 score
   New_Total : Natural := 0;
begin
   Open (My_File, In_File, "input/Day2-1.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String := Get_Line (My_File);
         Rnd       : constant Round  := Set_Round_Day2_1 (Next_Line);
         Rnd2      : constant Round  := Set_Round_Day2_2 (Next_Line);
      begin
         --  Day 2-1 stuff: just run the numbers thinking Y is Rock, etc.
         Total_Score :=
           Total_Score + Base_Score (Rnd.Me) + Result_Score (Rnd.Result);
         --  Day 2-2 stuff: calculate the score if I put in what I need.
         New_Total :=
           New_Total + Base_Score (Rnd2.Me) + Result_Score (Rnd2.Result);
      end;
   end loop;
   Close (My_File);
   Put_Line ("Day 2-1 Score is: " & Total_Score'Image);
   Put_Line ("Day 2-2 Score is: " & New_Total'Image);
end Day2;
