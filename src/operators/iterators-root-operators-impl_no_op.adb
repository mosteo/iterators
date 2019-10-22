package body Iterators.Root.Operators.Impl_No_Op is

   type Operator is new Operators.Operator with null record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      return This.Upstream.Next;
   end Next;

   function Create return Operators.Operator'Class is
     (Operator'(Operators.Operator with null record));

end Iterators.Root.Operators.Impl_No_Op;
