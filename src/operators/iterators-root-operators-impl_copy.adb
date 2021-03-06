package body Iterators.Root.Operators.Impl_Copy is

   type Operator is new Root.Operators.Operator with record
      Copy : Holder;
   end record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      if not This.Copy.Is_Valid then
         This.Copy := This.Upstream.To_Holder;
      end if;

      return This.Copy.As_Iterator.Next;
   end Next;

   function Create return Root.Operators.Operator'Class is
     (Operator'(Root.Operators.Operator with
                Copy => <>));

end Iterators.Root.Operators.Impl_Copy;
