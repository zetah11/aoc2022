with Monkey_Business; use Monkey_Business;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

package Parser is

   procedure Read_Monkeys (Monkeys : out Monkeys_Type);

private

   function Parse_Item_List (Line : String) return Item_Vectors.Vector with
     Pre => (Line'Length >= 1 and then Line (Line'First) in '0' .. '9');
   function Parse_Operation (Line : String) return Expression with
     Pre => (Line'Length >= 3);
   function Parse_Number (S : String) return Big_Positive with
     Pre => (for all C of S => C in '0' .. '9');
   function Parse_Name (S : String) return Monkey_Name with
     Pre => S'Length = 1;

end Parser;
