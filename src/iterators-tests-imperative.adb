procedure Iterators.Tests.Imperative is

    --  Regular sequences
   use Ints.Operators.Linking;

   Seq : constant Ints.Iterator'Class := Ints.Op.Just (1) & 2 & 3;
   Imp : Imp_Ints.Iterator;
   I2S : Imp_Int_Str.Operator;
   Str : Imp_Strings.Iterator;

   Pos : Positive;
   Chk : constant array (Positive range <>) of Integer := (2, 6);

begin
   Imp.Start (Seq);
   Imp.Copy;
   Imp.Filter (Is_Odd'Access);
   Imp.Map (Double'Access);

   Pos := 1;
   for Int of Imp.Iterate loop
      pragma Assert (Chk (Pos) = Int);
      Pos := Pos + 1;
   end loop;

   Imp.Start (Seq);
   Imp.Copy;
   I2S.Map (Imp, Image'Access);
   Str.Resume (I2S);
   Str.No_Op;

   Pos := 1;
   for Img of Str.Iterate loop
      pragma Assert (Pos'Img = Img);
      Pos := Pos + 1;
   end loop;
end Iterators.Tests.Imperative;
