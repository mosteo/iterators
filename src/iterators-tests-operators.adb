procedure Iterators.Tests.Operators is

   RO : aliased constant Number_Array := (1, 2, 3);

   use Ints.Linking;
--     use type Number_Array;

begin

   --  Flat map
   Check (Arrs.Const_Iter (RO)
          & Ints.Op.Flat_Map (Ints.Op.Just'Access),
          OK => (1, 2, 3));

   --  Map
   Check (Arrs.Const_Iter (RO)
          & Ints.Op.Map (Double'Access),
          OK => (2, 4, 6));

end Iterators.Tests.Operators;
