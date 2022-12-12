with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
procedure Day10 is
   --  Holds our instruction set.
   type ISA is (noop, addx);
   --  Holds our argument
   subtype Argument is Integer range -127 .. 127;
   --  Holds an individual instruction. Is this RISC?
   type Instruction is record
      Inst : ISA;
      Arg  : Argument := 0;
   end record;
   --  Holds cpu stuff
   type CPU is record
      Cycle    : Natural := 0;
      Register : Integer := 1;
   end record;
   --  Holds the program
   package Program is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Instruction);
   --  For the CRT
   type Pixel is (off, on);
   type Ray_Gun is mod 40; --  This is the location of the pixel on per row.
   type CRT_Line is mod 6; --  This keeps track of where in the CRT.
   type Screen is
     array (CRT_Line range 0 .. 5, Ray_Gun range 0 .. 39) of Pixel;
   type CRT is record
      Grid : Screen   := (others => (others => off));
      Gun  : Ray_Gun  := 0;
      Line : CRT_Line := 0;
   end record;
   --  The computer.
   type Computer is record
      Monitor   : CRT;
      Processor : CPU;
      Memory    : Program.Vector;
   end record;

   My_File : File_Type;

--   My_Program : Vector;
   My_Computer : Computer;

   --  This is for logging.
   Checkpoint_Start : constant Positive := 20;
   Checkpoint_Stop  : constant Positive := 220;
   Checkpoint_Inc   : constant Positive := 40;
   Next_Checkpoint  : Positive          := Checkpoint_Start;
   Still_Logging    : Boolean           := True;
   Sum              : Integer           := 0;

begin
   --  Read the program into memory.
   Open (My_File, In_File, "input/Day10.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line        : constant String      := Get_Line (My_File);
         Next_Instruction : constant Instruction :=
           (Inst => ISA'Value (Next_Line (Next_Line'First .. 4)),
            Arg  =>
              (if Next_Line'Length > 4 then
                 Argument'Value (Next_Line (5 .. Next_Line'Last))
               else 0));
      begin
         My_Computer.Memory.Append (Next_Instruction);
      end;
   end loop;
   Close (My_File);
   --  Run the program.
   for I of My_Computer.Memory loop
      declare --  This is how many cycles each takes (plus +1)
         Cycles_Remaining : constant Natural := ISA'Pos (I.Inst);
      begin
         for X in 0 .. Cycles_Remaining loop
            --  Increase the cycle
            My_Computer.Processor.Cycle := My_Computer.Processor.Cycle + 1;
            --  Log the cycle for part 1
            if Still_Logging
              and then My_Computer.Processor.Cycle = Next_Checkpoint
            then
               declare
                  Log : constant Integer :=
                    My_Computer.Processor.Cycle *
                    My_Computer.Processor.Register;
               begin
                  Sum             := Sum + Log;
                  Next_Checkpoint := Next_Checkpoint + Checkpoint_Inc;
                  --  We've reached a benchmark.  Sum it.
                  --  Is the next chepckpoint greater than our maximum?
                  if Next_Checkpoint > Checkpoint_Stop then
                     --  Stop logging.
                     Still_Logging := False;
                  end if;
               end;
            end if;

            --  Draw the pixel for pt 2.
            declare
               --  Just playing around with exceptions.
               Truth : constant array
                 (Integer range My_Computer.Processor.Register - 1 ..
                      My_Computer.Processor.Register + 1) of Boolean :=
                 (others => True);
            begin
               if Truth (Natural (My_Computer.Monitor.Gun)) then
                  My_Computer.Monitor.Grid
                    (My_Computer.Monitor.Line, My_Computer.Monitor.Gun) :=
                    on;
               end if;
            exception
               when Constraint_Error =>
                  null; --  Constraint_Error here means don't draw a pixel.
            end;
            My_Computer.Monitor.Gun := My_Computer.Monitor.Gun + 1;
            if My_Computer.Monitor.Gun = 0 then
               My_Computer.Monitor.Line := My_Computer.Monitor.Line + 1;
            end if;

         end loop;
         if I.Inst = addx then
            My_Computer.Processor.Register :=
              My_Computer.Processor.Register + I.Arg;
         end if;
      end;
   end loop;
   Put_Line ("Sum of the six signal strengths are: " & Sum'Image);
   Put_Line ("Output of CRT is:");
   for I in My_Computer.Monitor.Grid'Range (1) loop
      for J in My_Computer.Monitor.Grid'Range (2) loop
         Put (if My_Computer.Monitor.Grid (I, J) = on then "#" else ".");
      end loop;
      New_Line;
   end loop;
end Day10;
