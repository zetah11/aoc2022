with Ada.Containers; use Ada.Containers;

with Ada.Containers.Formal_Vectors;
with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

package Monkey_Business with
  SPARK_Mode
is

   package Item_Vectors is new Formal_Vectors
     (Index_Type => Positive, Element_Type => Big_Natural);

   type Operand_Kind is (Old, Int);
   type Operand is record
      Kind  : Operand_Kind;
      Value : Big_Natural := 0;
   end record;

   type Operation is (Add, Mul);
   type Expression is record
      Op    : Operation;
      Right : Operand;
   end record;

   type Monkey_Name is range 0 .. 7;

   type Monkey is record
      Items             : Item_Vectors.Vector (100);
      Operation         : Expression;
      Divisibility      : Big_Positive;
      If_True, If_False : Monkey_Name;
      Inspections       : Big_Natural := 0;
   end record;

   type Monkeys_Type is array (Monkey_Name) of Monkey;

   procedure Put_Line (M : Monkey);

   procedure Copy (From : Monkeys_Type; To : out Monkeys_Type) with
     Global => null, Depends => (To => From);

end Monkey_Business;
