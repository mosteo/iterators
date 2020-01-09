procedure Iterators.Tests.Imperative is

    --  Regular sequences
   use Ints.Operators.Linking;

   Seq : constant Ints.Iterator'Class := Ints.Op.Just (1) & 2 & 3;
   Imp : Ints.Sequence;
   I2S : Int2str.Sequence;
   Str : Strings.Sequence;

   Pos : Positive;

   type Number_Array is array (Positive range <>) of Integer;

   procedure Check (Imp : Ints.Sequence;
                    Ok  : Number_Array)
   is
      --  Ensure that all items in the iterator match expected values
      Pos : Positive := Ok'First;
   begin
      for Int of Imp.Iterate loop
         pragma Assert (Int = Ok (Pos));
         Pos := Pos + 1;
      end loop;
      pragma Assert (Pos = Ok'Last + 1);
   end Check;

begin

   --  Test mere setting up an imperative iterator:

   Imp.Start (Seq);
   Imp.Copy; -- So our sequence isn't consumed forever
   Check (Imp, (1, 2, 3));

   --  Check filtering

--     Imp.Restart (Seq);
--     Imp.Copy;
--     Imp.Filter (Is_Odd'Access);
--     Check (Imp, (1, 3));
--
--     --  Check mapping
--
--     Imp.Restart (Seq);
--     Imp.Copy;
--     Imp.Map (Double'Access);
--     Check (Imp, (2, 4, 6));
--
--     --  Check type conversion with No_Op on top
--
--     Imp.Restart (Seq);
--     Imp.Copy;
--     I2S.Map (Imp, Image'Access);
--     Str.Resume (I2S);
--     Str.No_Op;
--
--     Pos := 1;
--     for Img of Str.Iterate loop
--        pragma Assert (Pos'Img = Img);
--        Pos := Pos + 1;
--     end loop;
end Iterators.Tests.Imperative;
