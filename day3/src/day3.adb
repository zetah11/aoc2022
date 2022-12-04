with Ada.Text_IO; use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day3 with
  SPARK_Mode => On
is
   --  Items are identified by their value
   type Item is range 1 .. 52;

   --  Compartments are identified by which items they contain, though not how
   --  many of each.
   type Compartment is array (Item) of Boolean;

   --  Does the given string represent a valid rucksack description?
   function Valid_Rucksack (S : String) return Boolean is
     (for all C of S => C in 'a' .. 'z' or else C in 'A' .. 'Z');

   --  The intersection of two compartments is a new compartment which contains
   --  only the items present in both.
   function Intersect (A, B : Compartment) return Compartment is
      Result : Compartment;
   begin
      for I in Item loop
         Result (I) := A (I) and then B (I);
      end loop;

      return Result;
   end Intersect;

   --  Create an item from a character.
   function From_Character (C : Character) return Item is
     (if C in 'a' .. 'z' then Character'Pos (C) - Character'Pos ('a') + 1
      else Character'Pos (C) - Character'Pos ('A') + 27) with
     Pre => C in 'a' .. 'z' or else C in 'A' .. 'Z';

   -- Create a compartment from a line of characters.
   function From_Line (S : String) return Compartment is
      Result : Compartment := [others => False];
   begin
      for C of S loop
         Result (From_Character (C)) := True;
      end loop;

      return Result;
   end From_Line;

   --  Compute the priority of a line given its two halves, which is the value
   --  of the item present in both.
   function Line_Priority
     (First_Half, Second_Half : Compartment) return Natural
   is
      Both : constant Compartment := Intersect (First_Half, Second_Half);
      Sum  : Natural              := 0;
   begin
      for I in Item loop
         declare
            J : constant Natural := Natural (I);
         begin
            pragma Loop_Invariant (J <= 52);
            pragma Loop_Invariant (Sum <= J * (J + 1) / 2);  --  Thank Gauss!

            if Both (I) then
               Sum := Sum + Natural (I);
            end if;
         end;
      end loop;

      return Sum;
   end Line_Priority;

   --  Compute the priority of a group of three compartments, which is the value
   --  of the item present in all three.
   function Group_Priority (A, B, C : Compartment) return Natural is
      In_All : constant Compartment := Intersect (Intersect (A, B), C);
      Sum    : Natural              := 0;
   begin
      for I in Item loop
         declare
            J : constant Natural := Natural (I);
         begin
            pragma Loop_Invariant (J <= 52);
            pragma Loop_Invariant (Sum <= J * (J + 1) / 2);

            if In_All (I) then
               Sum := Sum + J;
            end if;
         end;
      end loop;

      return Sum;
   end Group_Priority;

   package Natural_Conversion is new Signed_Conversions (Int => Natural);

   File : File_Type;
   Last : Natural;
   Line : String (1 .. 256);

   type Step_Type is range 1 .. 3;

   Lines : array (Step_Type) of Compartment := [others => [others => False]];
   Step  : Step_Type                        := Step_Type'First;

   Part1_Sum : Big_Integer := 0;
   Part2_Sum : Big_Integer := 0;
begin
   Open (File, In_File, "input.txt");

   loop
      Get_Line (File, Line, Last);
      if not Valid_Rucksack (Line (1 .. Last)) then
         goto Continue;
      end if;

      declare
         Half1 : constant Compartment :=
           From_Line (Line (Line'First .. Last / 2));

         Half2 : constant Compartment :=
           From_Line (Line (Last / 2 + 1 .. Last));
      begin
         Part1_Sum :=
           Part1_Sum +
           Natural_Conversion.To_Big_Integer (Line_Priority (Half1, Half2));
      end;

      Lines (Step) := From_Line (Line (1 .. Last));

      if Step = Step_Type'Last then
         Step := Step_Type'First;
         declare
            A : constant Compartment := Lines (1);
            B : constant Compartment := Lines (2);
            C : constant Compartment := Lines (3);
         begin
            Part2_Sum :=
              Part2_Sum +
              Natural_Conversion.To_Big_Integer (Group_Priority (A, B, C));
         end;
      else
         Step := Step + 1;
      end if;

      <<Continue>>
      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Put_Line (To_String (Part1_Sum));

   Put ("Part 2: ");
   Put_Line (To_String (Part2_Sum));
end Day3;
