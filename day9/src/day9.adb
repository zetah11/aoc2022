with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Containers.Formal_Ordered_Sets;

procedure Day9 with
  SPARK_Mode
is
   type Position is record
      X, Y : Big_Integer;
   end record;

   type Direction is (Up, Left, Right, Down);

   function Less (A, B : Position) return Boolean is
     (A.X < B.X or else (A.X = B.X and then A.Y < B.Y));

   package Position_Sets is new Formal_Ordered_Sets
     (Element_Type => Position, "<" => Less);

   use Position_Sets;

   function Start return Position is (0, 0);

   function Parse_Direction (C : Character) return Direction is
     (case C is when 'U' => Up, when 'R' => Right, when 'L' => Left,
        when 'D' => Down, when others => raise Constraint_Error) with
     Pre => C in 'U' | 'R' | 'L' | 'D';

   function Parse_Count (S : String) return Big_Natural with
     Pre => (for all C of S => C in '0' .. '9')
   is
      Zero   : constant Natural := Character'Pos ('0');
      Result : Big_Natural      := 0;
      Digit  : Natural;
   begin
      for I in S'Range loop
         Digit  := Character'Pos (S (I)) - Zero;
         Result := 10 * Result + To_Big_Integer (Digit);
      end loop;

      return Result;
   end Parse_Count;

   --  Perform a move by direction.
   function Move (Pos : Position; Dir : Direction) return Position is
     (case Dir is when Up => (Pos.X, Pos.Y - 1),
        when Down => (Pos.X, Pos.Y + 1), when Left => (Pos.X - 1, Pos.Y),
        when Right => (Pos.X + 1, Pos.Y));

   --  Get a normalized version of the given vector. Because both components
   --  are integers, the result will be either 1 or -1 in one component, and 0
   --  in the other.
   function Normalized (Pos : Position) return Position is
      Largest : constant Big_Integer := Max (abs Pos.X, abs Pos.Y);
   begin
      if Largest = 0 then
         return (0, 0);
      else
         return (Pos.X / Largest, Pos.Y / Largest);
      end if;
   end Normalized;

   --  Get a negation of the given position.
   function Negate (Pos : Position) return Position is (-Pos.X, -Pos.Y);

   --  Compute the new position of the tail given its old position and the
   --  position of the head.
   function Follow (Head, Tail : Position) return Position is
      Delta_X : constant Big_Integer := Head.X - Tail.X;
      Delta_Y : constant Big_Integer := Head.Y - Tail.Y;
      Offset  : constant Position    :=
        Negate (Normalized (Position'(Delta_X, Delta_Y)));
   begin
      return (Head.X + Offset.X, Head.Y + Offset.Y);
   end Follow;

   File : File_Type;
   Line : String (1 .. 8);
   Last : Natural;

   Visited_Part1 : Set (10_000);
   Visited_Part2 : Set (10_000);
   Head, Tail    : Position                   := Start;
   Knots         : array (0 .. 9) of Position := [others => Start];
begin
   Open (File, In_File, "input.txt");

   loop
      Get_Line (File, Line, Last);

      if Last < 3 then
         Put_Line ("Bad input: line too short");
         return;
      end if;

      if Line (1) not in 'U' | 'L' | 'R' | 'D' then
         Put_Line ("Bad input: first character is not a direction");
         return;
      end if;

      if not (for all C of Line (3 .. Last) => C in '0' .. '9') then
         Put_Line ("Bad input: count is not a number");
         return;
      end if;

      declare
         Dir   : constant Direction   := Parse_Direction (Line (1));
         Count : constant Big_Natural := Parse_Count (Line (3 .. Last));
         C     : Big_Positive         := 1;
      begin
         while C <= Count loop
            C := C + 1;

            Head := Move (Head, Dir);
            Tail := Follow (Head, Tail);

            Knots (0) := Move (Knots (0), Dir);
            for I in 1 .. 9 loop
               Knots (I) := Follow (Knots (I - 1), Knots (I));
            end loop;

            if Length (Visited_Part1) = Visited_Part1.Capacity then
               Put_Line ("Bad input, part 1: too many visited tail positions");
               return;
            elsif Length (Visited_Part2) = Visited_Part2.Capacity then
               Put_Line ("Bad input, part 2: too many visited tail positions");
               return;
            end if;

            Include (Visited_Part1, Tail);
            Include (Visited_Part2, Knots (9));
         end loop;
      end;

      exit when End_Of_File (File);
   end loop;

   Put ("Part 1: ");
   Put_Line (Count_Type'Image (Length (Visited_Part1)));

   Put ("Part 2: ");
   Put_Line (Count_Type'Image (Length (Visited_Part2)));
end Day9;
