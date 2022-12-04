with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

with Section; use Section;
with Parse;   use Parse;

procedure Day4 with
  SPARK_Mode
is
   File : File_Type;
   Line : String (1 .. 256);
   Last : Natural;

   Part1_Count : Big_Integer := 0;
   Part2_Count : Big_Integer := 0;
begin
   Open (File, In_File, "input.txt");

   loop
      Get_Line (File, Line, Last);

      declare
         Result : constant Parse_Result :=
           Parse_Pair (Line (Line'First .. Last));
      begin
         if Result.Kind = Syntax then
            Put ("Malformed input in '");
            Put (Line (Line'First .. Last));
            Put_Line ("'");
         elsif Result.Kind = Overflow then
            Put ("Overflowing input in '");
            Put (Line (Line'First .. Last));
            Put_Line ("'");
         else
            if Covers (Result.First, Result.Second) then
               Part1_Count := Part1_Count + 1;
            end if;

            if Overlaps (Result.First, Result.Second) then
               Part2_Count := Part2_Count + 1;
            end if;
         end if;
      end;

      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Put_Line (To_String (Part1_Count));

   Put ("Part 2: ");
   Put_Line (To_String (Part2_Count));
end Day4;
