with Ada.Text_IO; use Ada.Text_IO;

package body Monkey_Business is

   procedure Put_Line (M : Monkey) is
   begin
      Put ("[");
      for Item of M.Items loop
         Put (To_String (Item));
         Put (", ");
      end loop;

      Put_Line ("]");
   end Put_Line;

   procedure Copy (From : Monkeys_Type; To : out Monkeys_Type) is
   begin
      for Name in From'Range loop
         To (Name).Items        := Item_Vectors.Copy (From (Name).Items, 100);
         To (Name).Operation    := From (Name).Operation;
         To (Name).Divisibility := From (Name).Divisibility;
         To (Name).If_True      := From (Name).If_True;
         To (Name).If_False     := From (Name).If_False;
         To (Name).Inspections  := From (Name).Inspections;
      end loop;
   end Copy;

end Monkey_Business;
