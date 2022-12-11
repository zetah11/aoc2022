with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

package body Parser is

   procedure Read_Monkeys (Monkeys : out Monkeys_Type) is
      File : File_Type;
      Line : String (1 .. 256);
      Last : Natural;

      Items             : Item_Vectors.Vector (100);
      Op                : Expression;
      Divisibility      : Big_Positive;
      If_True, If_False : Monkey_Name;
   begin
      Open (File, In_File, "input.txt");

      for Name in Monkeys'Range loop
         --  Monkey no.
         Skip_Line (File);

         --  Starting items
         Get_Line (File, Line, Last);
         Items := Parse_Item_List (Line (19 .. Last));

         --  Operation
         Get_Line (File, Line, Last);
         Op := Parse_Operation (Line (24 .. Last));

         --  Divisibility
         Get_Line (File, Line, Last);
         Divisibility := Parse_Number (Line (22 .. Last));

         --  Throw to
         Get_Line (File, Line, Last);
         If_True := Parse_Name (Line (30 .. Last));

         Get_Line (File, Line, Last);
         If_False := Parse_Name (Line (31 .. Last));

         Monkeys (Name) := (Items, Op, Divisibility, If_True, If_False, 0);

         exit when End_Of_File (File);

         --  Skip empty line
         Skip_Line (File);

         exit when End_Of_File (File);
      end loop;
   end Read_Monkeys;

   --  Parse a list of items given by worry level separated by commas, such as
   --  '79, 98'. The List must contain at least one number.
   function Parse_Item_List (Line : String) return Item_Vectors.Vector is
      type State_Type is (Digit, Comma);

      Zero   : constant Natural := Character'Pos ('0');
      Items  : Item_Vectors.Vector (100);
      State  : State_Type       := Digit;
      Number : Big_Natural      := 0;
   begin
      for C of Line loop
         case State is
            when Digit =>
               if C = ',' then
                  State := Comma;

                  if Item_Vectors.Length (Items) = Items.Capacity then
                     Put_Line ("Bad input: too many items!");
                     exit;
                  end if;

                  Item_Vectors.Append (Items, Number);
                  Number := 0;
               elsif C in '0' .. '9' then
                  Number :=
                    10 * Number + To_Big_Integer (Character'Pos (C) - Zero);
               else
                  Put_Line ("Bad input: expected a digit");
               end if;

            when Comma =>
               if C /= ' ' then
                  Put_Line ("Bad input: expected a space");
               end if;

               State := Digit;
         end case;
      end loop;

      Item_Vectors.Append (Items, Number);

      return Items;
   end Parse_Item_List;

   --  Parse an expression such as '+ 4' (with an implicit 'old' on the front)
   function Parse_Operation (Line : String) return Expression is
      type State_Type is (Start, Operator, Space, Digit);

      Zero : constant Natural := Character'Pos ('0');

      Op    : Operation;
      Right : Operand;

      Index  : Positive    := Line'First;
      State  : State_Type  := Start;
      Number : Big_Integer := 0;

      C : Character;
   begin
      loop
         C := Line (Index);

         case State is
            when Start =>
               if C = '+' then
                  Op := Add;
               elsif C = '*' then
                  Op := Mul;
               else
                  Put_Line
                    ("Syntax error: expected plus or star, got " &
                     Character'Image (C));
               end if;

               State := Operator;

            when Operator =>
               if C /= ' ' then
                  Put_Line
                    ("Syntax error: expected a space, got " &
                     Character'Image (C));
               end if;

               State := Space;

            when Space =>
               if C = 'o' then
                  return (Op, (Old, 0));
               elsif C in '0' .. '9' then
                  Number := To_Big_Integer (Character'Pos (C) - Zero);
                  State  := Digit;
               else
                  Put_Line
                    ("Syntax error: expected a number, got " &
                     Character'Image (C));
                  exit;
               end if;

            when Digit =>
               if C in '0' .. '9' then
                  Number :=
                    10 * Number + To_Big_Integer (Character'Pos (C) - Zero);
               else
                  Put_Line
                    ("Syntax error: expected a digit, got " &
                     Character'Image (C));
                  exit;
               end if;
         end case;

         Index := Index + 1;
         exit when Index not in Line'Range;
      end loop;

      Right := (Int, Number);
      return (Op, Right);
   end Parse_Operation;

   --  Parse a numeric value.
   function Parse_Number (S : String) return Big_Positive is
      Zero   : constant Natural := Character'Pos ('0');
      Result : Big_Natural      := 0;
   begin
      for C of S loop
         Result := 10 * Result + To_Big_Integer (Character'Pos (C) - Zero);
      end loop;

      return Result;
   end Parse_Number;

   --  Parse a monkey name.
   function Parse_Name (S : String) return Monkey_Name is
     (Character'Pos (S (S'First)) - Character'Pos ('0'));

end Parser;
