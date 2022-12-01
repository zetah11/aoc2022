with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day1 with
  SPARK_Mode => On
is
   function Numeric (S : String) return Boolean is
     (S'Length > 0 and then (for all C of S => C in '0' .. '9'));
   --  Checks if a string is numeric (composed only of decimal digits) or not.
   --  Returns false for empty strings.

   --  I'm unclear on what pre-conditions 'Big_Integers.From_String' has, so to
   --  avoid a 'pragma Annotate', I've just reimplemented it here.
   function To_Number (S : String) return Big_Integer with
     Pre => Numeric (S)
   is
      package Natural_Conversion is new Signed_Conversions (Int => Natural);

      Zero   : constant Big_Integer :=
        Natural_Conversion.To_Big_Integer (Character'Pos ('0'));
      Result : Big_Integer          := 0;
      Digit  : Big_Integer;
   begin
      for C of S loop
         pragma Loop_Invariant
           (C in '0' .. '9'
            and then Character'Pos (C) - Character'Pos ('0') in 0 .. 9);

         Digit := Natural_Conversion.To_Big_Integer (Character'Pos (C)) - Zero;

         pragma Assert (Digit >= 0 and then Digit <= 9);

         Result := Result * 10 + Digit;
      end loop;

      return Result;
   end To_Number;

   File : File_Type;

   Line : String (1 .. 10);
   Last : Natural;

   Sum : Big_Integer := 0;

   Third_Max  : Big_Integer := 0;
   Second_Max : Big_Integer := 0;
   Max        : Big_Integer := 0;
begin
   Open (File, In_File, "input.txt");

   loop
      pragma Loop_Invariant (Max >= 0 and then Max >= Sum);

      Get_Line (File, Line, Last);

      if Numeric (Line (Line'First .. Last)) then
         declare
            Value : constant Big_Integer :=
              To_Number (Line (Line'First .. Last));
         begin
            Sum := Sum + Value;

            if Sum > Max then
               Third_Max  := Second_Max;
               Second_Max := Max;
               Max        := Sum;
            elsif Sum > Second_Max then
               Third_Max  := Second_Max;
               Second_Max := Sum;
            elsif Sum > Third_Max then
               Third_Max := Sum;
            end if;
         end;
      else
         Sum := 0;
      end if;

      exit when End_Of_File (File);
   end loop;

   declare
      Total : constant Big_Integer := Max + Second_Max + Third_Max;
   begin
      Put_Line (To_String (Max));
      Put_Line (To_String (Total));
   end;
end Day1;
