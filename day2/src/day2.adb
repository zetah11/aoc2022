with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day2 with
  SPARK_Mode => On
is
   type Shape is (Rock, Paper, Scissors);
   for Shape use (Rock => 1, Paper => 2, Scissors => 3);

   type Result is (Loss, Draw, Victory);
   for Result use (Loss => 0, Draw => 3, Victory => 6);

   subtype Response is Character range 'X' .. 'Z';
   subtype Opponent is Character range 'A' .. 'C';

   function Opponent_Shape (C : Opponent) return Shape is
     (case C is when 'A' => Rock, when 'B' => Paper, when 'C' => Scissors);

   function Response_Shape (C : Response) return Shape is
     (case C is when 'X' => Rock, when 'Y' => Paper, when 'Z' => Scissors);

   function Response_Result (C : Response) return Result is
     (case C is when 'X' => Loss, when 'Y' => Draw, when 'Z' => Victory);

   --  Returns whether 'A' beats, draws, or is beaten by 'B'.
   function Fight (A, B : Shape) return Result is
   begin
      if A = B then
         return Draw;
      elsif Shape'Val ((Shape'Pos (A) - 1) mod 3) = B then
         return Victory;
      else
         return Loss;
      end if;
   end Fight;

   --  Returns the shape 'A' such that 'Fight (A, B) = State'.
   function Fight_Inverse (B : Shape; State : Result) return Shape is
      Off : constant array (Result) of Integer := [-1, 0, 1];
   begin
      return Shape'Val ((Shape'Pos (B) + Off (State)) mod 3);
   end Fight_Inverse;

   package Natural_Conversion is new Signed_Conversions (Int => Natural);

   File : File_Type;
   Last : Natural;
   Line : String (1 .. 4);

   Mine  : Shape;
   Other : Shape;
   State : Result;

   Part1_Score : Big_Integer := 0;
   Part2_Score : Big_Integer := 0;
begin
   Open (File, In_File, "input.txt");

   loop
      Get_Line (File, Line, Last);

      exit when Last /= 3 or else not (Line (1) in Opponent)
        or else not (Line (3) in Response);

      Other := Opponent_Shape (Opponent (Line (1)));
      Mine  := Response_Shape (Response (Line (3)));

      State := Fight (Mine, Other);

      Part1_Score :=
        Part1_Score + Natural_Conversion.To_Big_Integer (Mine'Enum_Rep) +
        Natural_Conversion.To_Big_Integer (State'Enum_Rep);

      State := Response_Result (Response (Line (3)));
      Mine  := Fight_Inverse (Other, State);

      Part2_Score :=
        Part2_Score + Natural_Conversion.To_Big_Integer (Mine'Enum_Rep) +
        Natural_Conversion.To_Big_Integer (State'Enum_Rep);

      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Put_Line (To_String (Part1_Score));

   Put ("Part 2: ");
   Put_Line (To_String (Part2_Score));
end Day2;
