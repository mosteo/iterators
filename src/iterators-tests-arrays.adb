procedure Iterators.Tests.Arrays is

   RO : aliased constant Number_Array := (1, 2, 3);

   use Ints.Linking;
   use type Number_Array;

begin
   --  Constant iteration
   Check (Arrs.Const_Iter (RO), (1, 2, 3));

   --  Mapping over constant. This is doable because map creates a copy.
   Check (Arrs.Const_Iter (RO)
          & Ints.Op.Map (Double'Access),
          (2, 4, 6));

   --  Check filtering
   Check (Arrs.Const_Iter (RO)
          & Ints.Op.Filter (Is_Odd'Access),
          (1, 3));

   --  Check in-place modification through filter
   declare
      RW   : aliased Number_Array := RO;
      RWit : Ints.Iterator'Class :=
               Arrs.Iter (RW)
               & Ints.Op.Filter (Is_Odd'Access);
   begin
      for Int of RWit loop
         Int := 0;
      end loop;
      pragma Assert (RW = (0, 2, 0));
   end;

   --  Abbreviated in-place iteration + modification
   declare
      RW : aliased Number_Array := RO;
   begin
      Put_Line ("XXX");
      for Int of Ints.Iterator'Class'(
                                      Arrs.Iter (RW)
                                      & Ints.Op.Filter (Is_Odd'Access)
                                      & Ints.Op.No_Op
                                     ).Print_Tag
                                      .Iter.all
      loop
         Put_Line ("YYY");
         Int := 0;
      end loop;
      pragma Assert (RW = (0, 2, 0));
   end;

   --  In-place constant filtering + iteration
   for Int of Ints.Looper'(Arrs.Const_Iter (RO)
                           & Ints.Op.Filter (Is_Odd'Access))
   loop
      if Int not in 1 | 3 then
         raise Constraint_Error with "Unexpected value:" & Int'Img;
      end if;
   end loop;

end Iterators.Tests.Arrays;
