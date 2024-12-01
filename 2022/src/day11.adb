with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Containers.Vectors;
procedure Day11 is

   --  This will be our "queue" of items
   package Item_Queue is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Natural);
   use Item_Queue;
   procedure Queue (Q : in out Item_Queue.Vector; N : Natural) is
   begin
      Item_Queue.Append (Q, New_Item => N);
   end Queue;
   function Dequeue (Q : in out Item_Queue.Vector) return Natural is
      Result : constant Natural := Item_Queue.First_Element (Q);
   begin
      Item_Queue.Delete_First (Q);
      return Result;
   end Dequeue;

   type Operators is (Plus, Multiply);
   type Operation is record
      Right_Old : Boolean;
      Op        : Operators;
      Value     : Natural := 0;
   end record;

   type Monkey is record
      Number         : Natural;
      Items        : Item_Queue.Vector;
      Op           : Operation;
      Div          : Natural;
      Monkey_True  : Natural;
      Monkey_False : Natural;
   end record;

   package Monkey_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Monkey);
   use Monkey_List;

   Monkies : Monkey_List.Vector;

   My_File : File_Type;

   type Parse_Type is (Monkey_Names, Starting_Items, Tests, If_Trues, If_Falses);
   function Parse(Item : String) return Operation is
      Result : Operation := (Right_Old => (Tail(Item, 3) = "old"), Op => (if Index(Item, "+") > 0 then Plus else Multiply), Value => 0);
   begin
      if not Result.Right_Old then
         Result.Value := Natural'Value(Item(Item'First + 23 .. Item'Last));
      end if;
      return Result;
   end Parse;
   function Parse(Item : String; What : Parse_Type) return Natural is
      Result : constant Natural := (case What is
                           when Monkey_Names => Natural'Value(Item(Item'First + 7 .. Item'Last-1)),
                           when Tests => Natural'Value(Item(Item'First + 19 .. Item'Last)),
                           when If_Trues => Natural'Value(Item(Item'First + 25 .. Item'Last)),
                           when If_Falses => Natural'Value(Item(Item'First + 26 .. Item'Last)),
                           when others => raise Constraint_Error with "No");
   begin
      Put_Line(What'Image);
      return Result;
   end Parse;
   function Parse(Item : String) return Item_Queue.Vector is
      Result : Item_Queue.Vector;
      Counter : Natural := Index(Item, ": ");
   begin
      while Counter < Item'Last loop
         declare
            Next_Item : constant Natural := Counter + 1;
            Comma : constant Natural := Index(Item(Next_Item .. Item'Last), ", ");
            To_Item : constant Natural := (if Comma > 0 then Comma-1 else Item'Last);
            Next_Number : constant Natural := Natural'Value(Item(Next_Item .. To_Item));
         begin
            Queue(Result, Next_Number);
            Counter := To_Item;
         end;
      end loop;
      return Result;
   end Parse;


begin
   --  Get the data.
   Open (My_File, In_File, "input/Day11.txt");
   while not End_Of_File (My_File) loop
      declare
         --  Set up our monkey.
         Next_Monkey : Monkey := (Number => Parse(Trim (Get_Line (My_File), Both), Monkey_Names),
                                 Items => Parse(Trim (Get_Line (My_File), Both)),
                                 Op => Parse(Trim (Get_Line (My_File), Both)),
                                 Div => Parse( Trim (Get_Line (My_File), Both), Tests),
                                 Monkey_True => Parse(Trim (Get_Line (My_File), Both), If_Trues),
                                 Monkey_False => Parse(Trim (Get_Line (My_File), Both), If_Falses));
      begin --  Finish the rest.
         for X of Next_Monkey.Items loop
            Put(X'Image);
         end loop;
         New_Line;
      end;
      if not End_Of_File (My_File) then
         Skip_Line(My_File);
      end if;
   end loop;
   Close (My_File);
end Day11;
