procedure Iterators.Tests.Imperative is

    --  Regular sequences
   use Ints.Operators.Linking;

   Seq : constant Ints.Iterator'Class := Ints.Op.Just (1) & 2 & 3;
   Int : Ints.Sequence;
   I2S : Int2str.Sequence;
   Str : Strings.Sequence;
   S2I : Str2int.Sequence;

begin

   --  Test mere setting up an imperative iterator:

   Int.Start (Seq);
   Check (Int, (1, 2, 3));

   --  Check copy is innocuous

   Int.Start (Seq);
   Int.Copy;
   Int.Copy;
   Check (Int, (1, 2, 3));

   --  Check filtering

   Int.Start (Seq);
   Int.Filter (Is_Odd'Access);
   Check (Int, (1, 3));

   --  Filtering plus copy

   Int.Start (Seq);
   Int.Copy;
   Int.Filter (Is_Odd'Access);
   Check (Int, (1, 3));

   --  Check mapping

   Int.Start (Seq);
   Int.Map (Int, Double'Access);
   Int.Copy;
   Check (Int, (2, 4, 6));

   --  Check type conversion with No_Op on top

   Int.Start (Seq);
   Int.Copy;
   Int.Map (Double'Access);
   Int.No_Op;
   I2S.Map (Int, Image'Access);
   Str.Start (I2S);
   Str.No_Op;
   S2I.Map (Str, Value'Access);
   Check (S2I, (2, 4, 6));

end Iterators.Tests.Imperative;
