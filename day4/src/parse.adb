package body Parse with
  SPARK_Mode
is
   function Parse_Pair (Line : String) return Parse_Result is
      package Natural_Conversion is new Signed_Conversions (Int => Natural);

      function In_Natural (I : Big_Integer) return Boolean is
        (In_Range
           (I, To_Big_Integer (Natural'First), To_Big_Integer (Natural'Last)));

      function All_Natural (R : Range_Result) return Boolean is
        (In_Natural (R.Low) and then In_Natural (R.High)) with
        Pre  => R.Valid,
        Post =>
         (if All_Natural'Result then
            In_Natural (R.Low) and then In_Natural (R.High));
   begin
      for I in Line'Range loop
         if Line (I) = ',' then
            if I = Line'First or else I = Line'Last then
               return (Kind => Syntax);
            end if;

            declare
               First : constant Range_Result :=
                 Parse_Range (Line (Line'First .. I - 1));

               Second : constant Range_Result :=
                 Parse_Range (Line (I + 1 .. Line'Last));

               First_Result  : Section_Range;
               Second_Result : Section_Range;
            begin
               if not (First.Valid and then Second.Valid) then
                  return (Kind => Syntax);
               elsif not (All_Natural (First) and then All_Natural (Second))
               then
                  return (Kind => Overflow);
               end if;

               First_Result :=
                 (First => Natural_Conversion.From_Big_Integer (First.Low),
                  Last  => Natural_Conversion.From_Big_Integer (First.High));

               Second_Result :=
                 (First => Natural_Conversion.From_Big_Integer (Second.Low),
                  Last  => Natural_Conversion.From_Big_Integer (Second.High));

               return (Ok, First_Result, Second_Result);
            end;
         end if;
      end loop;

      return (Kind => Syntax);
   end Parse_Pair;

   function Parse_Range (S : String) return Range_Result is
   begin
      for I in S'Range loop
         if S (I) = '-' then
            if I = S'First or else I = S'Last then
               return (Valid => False);
            elsif not Valid_Section (S (S'First .. I - 1)) then
               return (Valid => False);
            elsif not Valid_Section (S (I + 1 .. S'Last)) then
               return (Valid => False);
            end if;

            declare
               Low  : constant Big_Natural :=
                 Parse_Section (S (S'First .. I - 1));
               High : constant Big_Natural :=
                 Parse_Section (S (I + 1 .. S'Last));
            begin
               return (True, Low, High);
            end;
         end if;
      end loop;

      return (Valid => False);
   end Parse_Range;

   function Parse_Section (S : String) return Big_Natural is
      type Digit_Type is range 0 .. 9;

      package Digit_Conversion is new Signed_Conversions (Int => Digit_Type);

      Zero   : constant Natural := Character'Pos ('0');
      Result : Big_Natural      := 0;
      Digit  : Digit_Type;
   begin
      for I in S'Range loop
         Digit := Digit_Type (Character'Pos (S (I)) - Zero);

         Result := 10 * Result + Digit_Conversion.To_Big_Integer (Digit);
      end loop;

      return Result;
   end Parse_Section;

   function Valid_Range (S : String) return Boolean is
     (for all I in S'Range =>
        (S (I) in '0' .. '9')
        or else (I > S'First and then I < S'Last and then S (I) = '-'));

   function Valid_Section (S : String) return Boolean is
     (for all C of S => C in '0' .. '9');
end Parse;
