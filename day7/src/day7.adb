--  The solution today depends upon the fact that the directories are traversed
--  in a depth-first search-like manner, and that every directory is visited
--  at most once. This means that, instead of keeping track of directory names
--  and paths, we can instead just keep a list of directory sizes, together
--  with a kind of path stack containing indicies into that list. Every time we
--  encounter a file size, we can add that value to all the directories in the
--  path stack.

with Ada.Containers;    use Ada.Containers;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;

with Ada.Numerics.Big_Numbers.Big_Integers;
use Ada.Numerics.Big_Numbers.Big_Integers;

with Ada.Containers.Formal_Vectors;

procedure Day7 with
  SPARK_Mode
is
   package Directories is new Ada.Containers.Formal_Vectors
     (Index_Type => Natural, Element_Type => Big_Natural, "=" => "=");

   package Indicies is new Ada.Containers.Formal_Vectors
     (Index_Type => Natural, Element_Type => Directories.Extended_Index,
      "="        => "=");

   use Directories;
   use Indicies;

   --  Push a size to the directory vector.
   procedure Add_Directory (Dirs : in out Directories.Vector) with
     Depends => (Dirs => Dirs), Global => null,
     Pre     => Length (Dirs) < Capacity (Dirs),
     Post    => Length (Dirs) = Length (Dirs'Old) + 1
   is
   begin
      Append (Dirs, 0);
   end Add_Directory;

   --  Push an index to the path stack.
   procedure Push
     (Inds : in out Indicies.Vector; Dirs : Directories.Vector;
      I    :        Directories.Extended_Index) with
     Depends => (Inds => (Inds, I), null => Dirs), Global => null,
     Pre     =>
      Length (Inds) < Capacity (Inds)
      and then I in First_Index (Dirs) .. Last_Index (Dirs)
      and then
      (for all I of Inds => I in First_Index (Dirs) .. Last_Index (Dirs)),
     Post    =>
      Length (Inds) = Length (Inds'Old) + 1
      and then
      (for all I of Inds => I in First_Index (Dirs) .. Last_Index (Dirs))
   is
   begin
      Append (Inds, I);
   end Push;

   --  Add the given size to all the paths in the path stack.
   procedure Update_Sizes
     (Inds : Indicies.Vector; Dirs : in out Directories.Vector;
      Size : Big_Natural) with
     Depends => (Dirs => (Dirs, Inds, Size)), Global => null,
     Pre     =>
      (for all I of Inds => I in First_Index (Dirs) .. Last_Index (Dirs)),
     Post    => Length (Dirs) = Length (Dirs'Old)
   is
      L : constant Directories.Capacity_Range := Length (Dirs);
   begin
      for I of Inds loop
         pragma Loop_Invariant (Length (Dirs) = L);
         pragma Loop_Invariant (I in First_Index (Dirs) .. Last_Index (Dirs));

         declare
            Dir_Size : constant Big_Natural := Element (Dirs, I);
         begin
            Replace_Element (Dirs, I, Dir_Size + Size);
         end;
      end loop;
   end Update_Sizes;

   --  Parse a size value. The parsing stops the moment it hits a non-digital
   --  character.
   function Parse_Size (Line : String) return Big_Natural is
      Zero   : constant Natural := Character'Pos ('0');
      Result : Big_Natural      := 0;
   begin
      for C of Line loop
         exit when C not in '0' .. '9';

         Result := 10 * Result + To_Big_Integer (Character'Pos (C) - Zero);
      end loop;

      return Result;
   end Parse_Size;

   --  Sum all the sizes less than 100_000.
   function Small_Sum (Dirs : Directories.Vector) return Big_Natural is
      Sum : Big_Natural := 0;
   begin
      for Dir of Dirs loop
         if Dir < 100_000 then
            Sum := Sum + Dir;
         end if;
      end loop;

      return Sum;
   end Small_Sum;

   --  Find the smallest directory whose size is larger than Required.
   function Smallest_Deletable
     (Dirs : Directories.Vector; Required : Big_Natural) return Big_Natural
   is
      Smallest : Big_Natural := 70_000_000;
   begin
      for Size of Dirs loop
         if Size >= Required and then Size < Smallest then
            Smallest := Size;
         end if;
      end loop;

      return Smallest;
   end Smallest_Deletable;

   Fs_Size     : constant Big_Natural := 70_000_000;
   Needed_Size : constant Big_Natural := 30_000_000;

   Dirs  : Directories.Vector (2_000);
   Stack : Indicies.Vector (2_000);

   File : File_Type;
   Line : String (1 .. 256);
   Last : Natural;
begin
   Open (File, In_File, "input.txt");

   loop
      pragma Loop_Invariant
        (for all I of Stack => I in First_Index (Dirs) .. Last_Index (Dirs));

      Get_Line (File, Line, Last);

      if Last = 0 then
         goto Continue;
      end if;

      --  '$ cd ..' means we pop a path element from the path stack
      if Index (Line (Line'First .. Last), "$ cd ..", 1) = 1 then
         if Length (Stack) = 0 then
            Put_Line ("Bad input: tried to cd out of root");
            return;
         end if;

         Delete_Last (Stack);
         --  '$ cd <directory>' means we push a path element to the path stack
      elsif Index (Line (Line'First .. Last), "$ cd ", 1) = 1 then
         if Length (Stack) = Stack.Capacity then
            Put_Line ("Bad input: directories are too deeply nested");
            return;
         end if;

         if Length (Dirs) = Dirs.Capacity then
            Put_Line ("Bad input: Too many directories");
            return;
         end if;

         Add_Directory (Dirs);
         Push (Stack, Dirs, Last_Index (Dirs));
         --  '123 filename' is the size of a file in the current path
      elsif Line (Line'First) in '0' .. '9' then
         declare
            Size : constant Big_Natural :=
              Parse_Size (Line (Line'First .. Last));
         begin
            Update_Sizes (Stack, Dirs, Size);
         end;
      end if;

      <<Continue>>
      exit when End_Of_File (File);
   end loop;

   declare
      Total : constant Big_Natural := Small_Sum (Dirs);
   begin
      Put ("Part 1: ");
      Put_Line (To_String (Total));
   end;

   if Length (Dirs) = 0 or else First_Element (Dirs) < Fs_Size - Needed_Size
   then
      Put_Line ("Bad input: no directories to delete.");
      return;
   end if;

   declare
      Required : constant Big_Natural :=
        Needed_Size - (Fs_Size - First_Element (Dirs));
      Smallest : constant Big_Natural := Smallest_Deletable (Dirs, Required);
   begin
      Put ("Part 2: ");
      Put_Line (To_String (Smallest));
   end;
end Day7;
