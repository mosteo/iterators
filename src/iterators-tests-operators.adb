procedure Iterators.Tests.Operators is

   use Ints.Linking;
   use type Number_Array;

   Empty : aliased constant Number_Array := (1 .. 0 => <>);
   A_1_3 : aliased constant Number_Array := (1, 2, 3);

--     function Arr (X : Number_Array) return access Number_Array is
--       (X'Unrestricted_Access);

   function Seq (I : Integer) return Ints.Iterator'Class is
     (if I <= 1
      then Ints.Op.Just (1)
      else Seq (I - 1) & Ints.Op.Append (I));

   function Odd_Seq (I : Integer) return Ints.Iterator'Class is
     (if I <= 1 then Ints.Op.Just (1)
      elsif I mod 2 = 0 then Ints.Op.Empty
      else Odd_Seq (I - 2) & Ints.Op.Append (I));

--     use type Number_Array;

begin

   --  Flat map
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Flat_Map (Ints.Op.Just'Access),
          OK => A_1_3);
   Check (Arrs.Const_Iter (Empty)
          & Ints.Op.Flat_Map (Ints.Op.Just'Access),
          OK => Empty);
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Flat_Map (Seq'Access),
          OK => (1, 1, 2, 1, 2, 3));
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Flat_Map (Odd_Seq'Access),
          OK => (1, 1, 3));

   --  For each
   declare
      Arr : aliased Number_Array := (1, 2, 3);
      procedure Nullify (Int : in out Integer) is
      begin
         Int := 0;
      end Nullify;
   begin
      Ints.Op.For_Each (Arrs.Iter (Arr).Iter.all, Nullify'Access);
      pragma Assert (Arr = (0, 0, 0));
   end;

   --  Map
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Map (Double'Access),
          OK => (2, 4, 6));

   --  Take
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Take (2),
          OK => (1, 2));
   Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Take (1),
          OK => (1 => 1));
      Check (Arrs.Const_Iter (A_1_3)
          & Ints.Op.Take (0),
          OK => (1 .. 0 => <>));

end Iterators.Tests.Operators;
