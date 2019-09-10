package body Iterators.Root.Impl_Copy is

   type Operator is new Root.Operator with record
      Copy : Holder;
   end record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      if not This.Copy.Is_Valid then
         This.Copy := This.Up;
      end if;

      return This.Copy.As_Iterator.Next;
   end Next;

   function Create return Root.Operator'Class is
     (Operator'(Up   => <>,
                Copy => <>));

end Iterators.Root.Impl_Copy;
