with Ada.Text_IO;

procedure Iterators.Tests.Imperative is

    --  Regular sequences
   use Ints.Operators.Linking;

   Seq : constant Ints.Iterator'Class := Ints.Op.Just (1) & 2 & 3;
   Int : Ints.Sequence;
   I2S : Int2str.Sequence;
   Str : Strings.Sequence;
   S2I : Str2int.Sequence;

   type Number_Array is array (Positive range <>) of Integer;

   procedure Check (Imp : Ints.Iterators.Iterable'Class;
                    Ok  : Number_Array)
      --  Ensure that all items in the iterator match expected values
   is
      use Ada.Text_IO;
      Pos : Positive := Ok'First;
      Dbg : constant Boolean := False;
   begin
      if Dbg then
         Put_Line ("---");
      end if;

      for Int of Imp.Iterate loop
         if Dbg then
            Put_Line ("Next:" & Int'Img);
         end if;

         if Pos > Ok'Last then
            raise Constraint_Error with
              "More elements in sequence than expected, index:" & Pos'Img
              & "; Item:" & Int'Img;
         end if;

         if Int /= Ok (Pos) then
            raise Constraint_Error with
              "Expected:" & Ok (Pos)'img & "; Got:" & Int'Img;
         end if;

         Pos := Pos + 1;
      end loop;

      if Pos /= Ok'Last + 1 then
         raise Constraint_Error with
           "Fewer elements in sequence than expected, index:" & Pos'Img
           & "; Ok'Length =" & Ok'Length'Img;
      end if;
   end Check;

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
