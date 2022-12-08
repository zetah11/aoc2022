with Ada.Text_IO; use Ada.Text_IO;

procedure Day8 with
  SPARK_Mode
is
   --  Am I hardcoding the input size? Yes! I did vector trickery yesterday,
   --  and I do not intend on repeating the effort today!
   type Width is range 1 .. 99;
   type Height is range 0 .. 9;
   type Forest is array (Width, Width) of Height;
   type Direction is (Up, Left, Right, Down);

   Size : constant Natural := Width'Range_Length;

   procedure Parse (F : out Forest; Success : out Boolean) is
      File : File_Type;
      Line : String (1 .. 256);
      Last : Natural;

      Row : Width := Width'First;
   begin
      Open (File, In_File, "input.txt");

      --  Default value in case parsing fails
      F := [others => [others => 1]];

      loop
         Get_Line (File, Line, Last);

         if Last /= Size then
            Put_Line ("Bad input: line of incorrect size");
            Success := False;
            return;
         end if;

         if not (for all C of Line (Line'First .. Last) => C in '0' .. '9')
         then
            Put_Line ("Bad input: non-digit in line");
            Success := False;
            return;
         end if;

         for I in Line'First .. Last loop
            declare
               Col   : constant Width  :=
                 Width (I - Line'First + Integer (Width'First));
               Digit : constant Height :=
                 Character'Pos (Line (I)) - Character'Pos ('0');
            begin
               F (Row, Col) := Digit;
            end;
         end loop;

         if Row = Width'Last then
            exit;
         end if;

         Row := Row + 1;

         exit when End_Of_File (File);
      end loop;

      Success := Row = Width'Last;
   end Parse;

   --  Is the given position on the edge of the board?
   function On_Edge (Row, Col : Width) return Boolean is
     (Row = Width'First or else Row = Width'Last or else Col = Width'First
      or else Col = Width'Last);

   --  Does the given position have visibility to the outside world?
   function Is_Visible (F : Forest; Row, Col : Width) return Boolean is
   begin
      if On_Edge (Row, Col) then
         return True;
      end if;

      declare
         H : constant Height := F (Row, Col);

         Top    : constant Boolean :=
           (for all R in Width'First .. Row - 1 => F (R, Col) < H);
         Left   : constant Boolean :=
           (for all C in Width'First .. Col - 1 => F (Row, C) < H);
         Right  : constant Boolean :=
           (for all C in Col + 1 .. Width'Last => F (Row, C) < H);
         Bottom : constant Boolean :=
           (for all R in Row + 1 .. Width'Last => F (R, Col) < H);
      begin
         return Top or else Left or else Right or else Bottom;
      end;
   end Is_Visible;

   --  Compute the scenic score in a single direction
   function Score_Direction
     (F : Forest; Row, Col : Width; D : Direction) return Natural with
     Post => Score_Direction'Result <= Size
   is
      H    : constant Height := F (Row, Col);
      Dist : Natural;
   begin
      if On_Edge (Row, Col) then
         return 0;
      end if;

      --  Redundancy is good engineering, or something
      case D is
         when Up =>
            for R in reverse Width'First .. Row - 1 loop
               if F (R, Col) >= H then
                  Dist := Natural (Row - R);
                  return Dist;
               end if;
            end loop;

            return Natural (Row - Width'First);

         when Left =>
            for C in reverse Width'First .. Col - 1 loop
               if F (Row, C) >= H then
                  Dist := Natural (Col - C);
                  return Dist;
               end if;
            end loop;

            return Natural (Col - Width'First);

         when Right =>
            for C in Col + 1 .. Width'Last loop
               if F (Row, C) >= H then
                  Dist := Natural (C - Col);
                  return Dist;
               end if;
            end loop;

            return Natural (Width'Last - Col);

         when Down =>
            for R in Row + 1 .. Width'Last loop
               if F (R, Col) >= H then
                  Dist := Natural (R - Row);
                  return Dist;
               end if;
            end loop;

            return Natural (Width'Last - Row);
      end case;
   end Score_Direction;

   --  Get the scenic score for the given position.
   function Scenic_Score (F : Forest; Row, Col : Width) return Natural is
      Score_Up    : constant Natural := Score_Direction (F, Row, Col, Up);
      Score_Down  : constant Natural := Score_Direction (F, Row, Col, Down);
      Score_Left  : constant Natural := Score_Direction (F, Row, Col, Left);
      Score_Right : constant Natural := Score_Direction (F, Row, Col, Right);
   begin
      return Score_Up * Score_Down * Score_Left * Score_Right;
   end Scenic_Score;

   Success : Boolean;
   F       : Forest;
   Visible : Natural := 0;
   Scenic  : Natural := 0;
begin
   Parse (F, Success);
   if not Success then
      Put_Line ("Exiting early due to malformed input");
      return;
   end if;

   for Row in Width'Range loop
      pragma Loop_Invariant
        (Visible <= Natural (Row - Width'First) * Width'Range_Length);

      declare
         Visible_Col : Natural := 0;
      begin
         for Col in Width'Range loop
            pragma Loop_Invariant (Visible_Col <= Natural (Col - Width'First));

            if Is_Visible (F, Row, Col) then
               Visible_Col := Visible_Col + 1;
            end if;

            Scenic := Natural'Max (Scenic_Score (F, Row, Col), Scenic);
         end loop;

         Visible := Visible + Visible_Col;
      end;
   end loop;

   Put ("Part 1: ");
   Put_Line (Natural'Image (Visible));

   Put ("Part 2: ");
   Put_Line (Natural'Image (Scenic));
end Day8;
