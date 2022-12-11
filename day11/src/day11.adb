with Ada.Containers;  use Ada.Containers;
with Ada.Text_IO;     use Ada.Text_IO;
with Monkey_Business; use Monkey_Business;
with Parser;          use Parser;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

procedure Day11 with
  SPARK_Mode
is
   use Item_Vectors;

   function Evaluate_Part1
     (Op : Expression; Prev : Big_Natural) return Big_Natural
   is
      Right : constant Big_Natural :=
        (case Op.Right.Kind is when Old => Prev, when Int => Op.Right.Value);
   begin
      case Op.Op is
         when Add =>
            return Prev + Right;
         when Mul =>
            return Prev * Right;
      end case;
   end Evaluate_Part1;

   function Evaluate_Part2
     (Op : Expression; Prev : Big_Natural) return Big_Natural
   is
      Right : constant Big_Natural :=
        (case Op.Right.Kind is when Old => Prev, when Int => Op.Right.Value);
   begin
      case Op.Op is
         when Add =>
            return Prev + Right;
         when Mul =>
            return Prev * Right;
      end case;
   end Evaluate_Part2;

   procedure Do_Turn_Part1 (Monkeys : in out Monkeys_Type; Name : Monkey_Name)
   is
      M : constant Monkey := Monkeys (Name);
   begin
      for Item of M.Items loop
         pragma Loop_Invariant (Monkeys (Name).Inspections >= 0);
         Monkeys (Name).Inspections := @ + 1;

         declare
            Value     : constant Big_Integer :=
              Evaluate_Part1 (M.Operation, Item) / 3;
            Recipient : constant Monkey_Name :=
              (if Value mod M.Divisibility = 0 then M.If_True else M.If_False);
         begin
            if Length (Monkeys (Recipient).Items) =
              Monkeys (Recipient).Items.Capacity
            then
               Put_Line ("Error: too many items for one monkey to handle.");
            else
               Append (Monkeys (Recipient).Items, Value);
            end if;
         end;
      end loop;

      Clear (Monkeys (Name).Items);
   end Do_Turn_Part1;

   procedure Do_Turn_Part2
     (Monkeys : in out Monkeys_Type; Name : Monkey_Name;
      Modulus :        Big_Positive)
   is
      M : constant Monkey := Monkeys (Name);
   begin
      for Item of M.Items loop
         pragma Loop_Invariant (Monkeys (Name).Inspections >= 0);
         Monkeys (Name).Inspections := @ + 1;

         declare
            Value     : constant Big_Integer :=
              Evaluate_Part2 (M.Operation, Item) mod Modulus;
            Recipient : constant Monkey_Name :=
              (if Value mod M.Divisibility = 0 then M.If_True else M.If_False);
         begin
            if Length (Monkeys (Recipient).Items) =
              Monkeys (Recipient).Items.Capacity
            then
               Put_Line ("Error: too many items for one monkey to handle.");
            else
               Append (Monkeys (Recipient).Items, Value);
            end if;
         end;
      end loop;

      Clear (Monkeys (Name).Items);
   end Do_Turn_Part2;

   procedure Do_Round_Part1 (Monkeys : in out Monkeys_Type) is
   begin
      for Name in Monkeys'Range loop
         Do_Turn_Part1 (Monkeys, Name);
      end loop;
   end Do_Round_Part1;

   function Get_Modulus (Monkeys : Monkeys_Type) return Big_Positive is
      Result : Big_Positive := 1;
   begin
      for Monkey of Monkeys loop
         Result := Result * Monkey.Divisibility;
      end loop;

      return Result;
   end Get_Modulus;

   procedure Do_Round_Part2
     (Monkeys : in out Monkeys_Type; Modulus : Big_Positive)
   is
   begin
      for Name in Monkeys'Range loop
         Do_Turn_Part2 (Monkeys, Name, Modulus);
      end loop;
   end Do_Round_Part2;

   Monkeys_Part1 : Monkeys_Type;
   Monkeys_Part2 : Monkeys_Type;

   Modulus : Big_Positive;
begin
   Read_Monkeys (Monkeys_Part1);
   Copy (Monkeys_Part1, Monkeys_Part2);

   for Name in Monkeys_Part1'Range loop
      Put (Monkey_Name'Image (Name));
      Put (" ");
      Put_Line (Monkeys_Part1 (Name));
   end loop;

   for I in 1 .. 20 loop
      Do_Round_Part1 (Monkeys_Part1);
   end loop;

   declare
      Max_1 : Big_Natural := 0;
      Max_2 : Big_Natural := 0;
   begin
      for Monkey of Monkeys_Part1 loop
         if Monkey.Inspections > Max_1 then
            Max_2 := Max_1;
            Max_1 := Monkey.Inspections;
         elsif Monkey.Inspections > Max_2 then
            Max_2 := Monkey.Inspections;
         end if;
      end loop;

      Put ("Part 1: ");
      Put_Line (To_String (Max_1 * Max_2));
   end;

   Modulus := Get_Modulus (Monkeys_Part2);
   for I in 1 .. 10_000 loop
      Do_Round_Part2 (Monkeys_Part2, Modulus);
   end loop;

   declare
      Max_1 : Big_Natural := 0;
      Max_2 : Big_Natural := 0;
   begin
      for Monkey of Monkeys_Part2 loop
         if Monkey.Inspections > Max_1 then
            Max_2 := Max_1;
            Max_1 := Monkey.Inspections;
         elsif Monkey.Inspections > Max_2 then
            Max_2 := Monkey.Inspections;
         end if;
      end loop;

      Put ("Part 2: ");
      Put_Line (To_String (Max_1 * Max_2));
   end;
end Day11;
