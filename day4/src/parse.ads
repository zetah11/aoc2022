with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

with Section; use Section;

package Parse with
  SPARK_Mode
is
   pragma Preelaborate;

   type Parse_Result_Kind is (Ok, Syntax, Overflow);
   type Parse_Result (Kind : Parse_Result_Kind) is record
      case Kind is
         when Ok =>
            First  : Section_Range;
            Second : Section_Range;
         when Syntax | Overflow =>
            null;
      end case;
   end record;

   function Parse_Pair (Line : String) return Parse_Result;

private

   type Range_Result (Valid : Boolean := False) is record
      case Valid is
         when True =>
            Low, High : Big_Natural;
         when False =>
            null;
      end case;
   end record;

   function Valid_Section (S : String) return Boolean;
   function Valid_Range (S : String) return Boolean;

   function Parse_Section (S : String) return Big_Natural with
     Pre => Valid_Section (S);

   function Parse_Range (S : String) return Range_Result;

end Parse;
