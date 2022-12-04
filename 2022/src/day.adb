with Ada.Text_IO; use Ada.Text_IO;

procedure Day is
   My_File : File_Type;
begin
   Open (My_File, In_File, "input/Day.txt");
   while not End_Of_File (My_File) loop
      declare
         Next_Line : constant String := Get_Line (My_File);
      begin
         Put_Line (Next_Line);
      end;
   end loop;
   Close (My_File);
end Day;
