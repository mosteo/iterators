package body Iterators.Root.Operators.Impl_Empty is

   type Iterator is new Root.Iterator with null record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
     (New_Empty_Cursor);

   function Create return Root.Iterator'Class is (Iterator'(null record));

end Iterators.Root.Operators.Impl_Empty;
