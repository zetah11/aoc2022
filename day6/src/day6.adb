with Ada.Text_IO; use Ada.Text_IO;

procedure Day6 with
  SPARK_Mode
is
   subtype Data is Character range 'a' .. 'z';
   type Window is array (Positive range <>) of Data;

   function All_Different (W : Window) return Boolean is
   begin
      for I in W'Range loop
         for J in I .. W'Last loop
            if I /= J and then W (I) = W (J) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end All_Different;

   procedure Shift (W : in out Window; D : Data) is
   begin
      for I in W'First .. W'Last - 1 loop
         W (I) := W (I + 1);
      end loop;

      W (W'Last) := D;
   end Shift;

   type State_Type is (Init, Run, Done);

   File : File_Type;
   C    : Character;

   Window_Part1 : Window (1 .. 4) := [others => 'a'];
   State_Part1  : State_Type      := Init;

   Window_Part2 : Window (1 .. 14) := [others => 'a'];
   State_Part2  : State_Type       := Init;

   Index : Positive := 1;
begin
   Open (File, In_File, "input.txt");

   loop
      pragma Loop_Invariant
        (if State_Part1 = Init then Index <= Window_Part1'Length);
      pragma Loop_Invariant
        (if State_Part2 = Init then Index <= Window_Part2'Length);

      Get (File, C);
      if C not in Data then
         goto Continue;
      end if;

      declare
         D : constant Data := Data (C);
      begin
         case State_Part1 is
            when Init =>
               Window_Part1 (Index) := D;

               if Index = Window_Part1'Last then
                  State_Part1 := Run;
               end if;

            when Run =>
               Shift (Window_Part1, D);

               if All_Different (Window_Part1) then
                  Put ("Part 1: ");
                  Put_Line (Positive'Image (Index));
                  State_Part1 := Done;
               end if;

            when Done =>
               null;
         end case;

         case State_Part2 is
            when Init =>
               Window_Part2 (Index) := D;

               if Index = Window_Part2'Last then
                  State_Part2 := Run;
               end if;

            when Run =>
               Shift (Window_Part2, D);

               if All_Different (Window_Part2) then
                  Put ("Part 2: ");
                  Put_Line (Positive'Image (Index));
                  State_Part2 := Done;
               end if;

            when Done =>
               null;
         end case;
      end;

      if Index = Natural'Last then
         Put_Line ("Too much data!");
         exit;
      end if;

      Index := Index + 1;

      <<Continue>>
      exit when End_Of_File (File);
      exit when State_Part1 = Done and then State_Part2 = Done;
   end loop;
end Day6;
