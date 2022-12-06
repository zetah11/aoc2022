with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Containers.Vectors;

procedure Day5 is
   subtype Item is Character range 'A' .. 'Z';

   package Stack is new Vectors
     (Index_Type => Natural, Element_Type => Item, "=" => "=");

   use Stack;

   type Dock is array (Positive range <>) of Stack.Vector;

   --  The movement procedure used by CrateMover 9000 in part 1.
   procedure Move_Single
     (Crates : in out Dock; Number : Natural; From, To : Positive)
   is
      Moved : Item;
   begin
      for I in 1 .. Number loop
         Moved := Last_Element (Crates (From));
         Delete_Last (Crates (From));

         Append (Crates (To), Moved);
      end loop;
   end Move_Single;

   --  The movement procedure used by CrateMover 9001 in part 2.
   procedure Move_Multiple
     (Crates : in out Dock; Number : Natural; From, To : Positive)
   is
      Moved : array (1 .. Number) of Item;
   begin
      for I in reverse Moved'Range loop
         Moved (I) := Last_Element (Crates (From));
         Delete_Last (Crates (From));
      end loop;

      for I of Moved loop
         Append (Crates (To), I);
      end loop;
   end Move_Multiple;

   procedure Output (Crates : Dock) is
   begin
      for S of Crates loop
         if Length (S) > 0 then
            Put (Last_Element (S));
         end if;
      end loop;

      Put_Line ("");
   end Output;

   procedure Parse_Items (Crates1, Crates2 : in out Dock; Line : String) is
      Dock_Index : Positive := 1;
      C          : Character;
   begin
      for I in Line'Range loop
         if I mod 4 = 2 then
            C := Line (I);
            if C in Item then
               Prepend (Crates1 (Dock_Index), Item (C));
               Prepend (Crates2 (Dock_Index), Item (C));
            end if;

            Dock_Index := Dock_Index + 1;
         end if;
      end loop;
   end Parse_Items;

   procedure Parse_Instruction (Crates1, Crates2 : in out Dock; Line : String)
   is
      type State_Type is
        (Op_Move, Num_Amount, Op_From, Num_From, Op_To, Num_To);

      Zero : constant Natural := Character'Pos ('0');

      State  : State_Type := Op_Move;
      Amount : Natural    := 0;
      From   : Natural    := 0;
      To     : Natural    := 0;
   begin
      for C of Line loop
         case State is
            when Op_Move =>
               if C = ' ' then
                  State := Num_Amount;
               end if;

            when Op_From =>
               if C = ' ' then
                  State := Num_From;
               end if;

            when Op_To =>
               if C = ' ' then
                  State := Num_To;
               end if;

            when Num_Amount =>
               if C in '0' .. '9' then
                  Amount := Amount * 10 + (Character'Pos (C) - Zero);
               else
                  State := Op_From;
               end if;

            when Num_From =>
               if C in '0' .. '9' then
                  From := From * 10 + (Character'Pos (C) - Zero);
               else
                  State := Op_To;
               end if;

            when Num_To =>
               if C in '0' .. '9' then
                  To := To * 10 + (Character'Pos (C) - Zero);
               end if;
         end case;
      end loop;

      Move_Single (Crates1, Amount, Positive (From), Positive (To));
      Move_Multiple (Crates2, Amount, Positive (From), Positive (To));
   end Parse_Instruction;

   type Parse_State is (Items, Spaces, Instruction);

   File : File_Type;
   Line : String (1 .. 256);
   Last : Natural;

   State : Parse_State := Items;

   Crates_Part1 : Dock (1 .. 9);
   Crates_Part2 : Dock (1 .. 9);
begin
   Open (File, In_File, "input.txt");

   loop
      Get_Line (File, Line, Last);

      declare
         This_Line : constant String := Line (Line'First .. Last);
      begin
         case State is
            when Items =>
               if (for some C of This_Line => C in '0' .. '9') then
                  State := Spaces;
               else
                  Parse_Items (Crates_Part1, Crates_Part2, This_Line);
               end if;

            when Spaces =>
               State := Instruction;

            when others =>
               Parse_Instruction (Crates_Part1, Crates_Part2, This_Line);
         end case;
      end;

      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Output (Crates_Part1);

   Put ("Part 2: ");
   Output (Crates_Part2);
end Day5;
