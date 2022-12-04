package Section with
  SPARK_Mode
is
   pragma Preelaborate;

   type Section_Range is record
      First, Last : Natural;
   end record;

   function Within (Value : Natural; S : Section_Range) return Boolean is
     (S.First <= Value and then Value <= S.Last);

   function Covers (A, B : Section_Range) return Boolean is
     ((for all Value in A.First .. A.Last => Within (Value, B))
      or else (for all Value in B.First .. B.Last => Within (Value, A)));

   function Overlaps (A, B : Section_Range) return Boolean is
     (for some Value in A.First .. A.Last => Within (Value, B));
end Section;
