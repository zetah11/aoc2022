with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day10 with
  SPARK_Mode
is
   function Is_Integer_Literal (S : String) return Boolean is
     (S'Length > 0
      and then
      (if S (S'First) = '-' then
         S'Length > 1
         and then (for all C of S (S'First + 1 .. S'Last) => C in '0' .. '9')
       else (for all C of S => C in '0' .. '9')));

   function Parse_Integer (S : String) return Big_Integer with
     Pre => Is_Integer_Literal (S)
   is
      Zero   : constant Natural := Character'Pos ('0');
      Result : Big_Integer      := 0;
      Sign   : Big_Integer      := 1;
   begin
      if S (S'First) = '-' then
         Sign := -1;
      end if;

      for C of S loop
         if C in '0' .. '9' then
            Result := Result * 10 + To_Big_Integer (Character'Pos (C) - Zero);
         end if;
      end loop;

      return Result * Sign;
   end Parse_Integer;

   File : File_Type;
   Line : String (1 .. 10);
   Last : Natural;

   X         : Big_Integer  := 1;
   New_X     : Big_Integer  := X;
   Cycle     : Big_Positive := 1;
   Milestone : Big_Positive := 20;
   Wait      : Positive;

   Total : Big_Integer := 0;
begin
   Open (File, In_File, "input.txt");

   Put_Line ("Part 2: ");

   loop
      Get_Line (File, Line, Last);

      if Last < 4 then
         Put_Line ("Bad input: line too short");
         return;
      end if;

      if Index (Line (Line'First .. Last), "noop") = 1 then
         Wait := 1;
      elsif Index (Line (Line'First .. Last), "addx") = 1 then
         Wait := 2;

         if Last < 6 then
            Put_Line ("Bad input: addx missing argument");
            return;
         elsif not Is_Integer_Literal (Line (6 .. Last)) then
            Put_Line ("Bad input: addx argument must be an integer");
            return;
         end if;

         New_X := X + Parse_Integer (Line (6 .. Last));
      else
         Put_Line
           ("Bad input: unrecognized command '" & Line (Line'First .. Last) &
            "'");
         return;
      end if;

      for I in 1 .. Wait loop
         declare
            Col : constant Big_Natural := (Cycle - 1) mod 40;
         begin
            if abs (X - Col) <= 1 then
               Put ("#");
            else
               Put (".");
            end if;

            if Col = 39 then
               Put_Line ("");
            end if;
         end;

         if Cycle = Milestone then
            Total     := Total + X * Cycle;
            Milestone := Milestone + 40;
         end if;

         Cycle := Cycle + 1;
      end loop;

      X     := New_X;
      New_X := X;

      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Put_Line (To_String (Total));
end Day10;
