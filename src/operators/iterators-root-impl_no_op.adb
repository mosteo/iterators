package body Iterators.Root.Impl_No_Op is

   type Operator is new Root.Operator with null record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      return This.Upstream.Next;
   end Next;

   function Create return Root.Operator'Class is
     (Operator'(Up => <>));

end Iterators.Root.Impl_No_Op;
