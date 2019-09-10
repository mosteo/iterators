package body Iterators.Root.Impl_Copy is

   type Iterator is new Root.Iterator with record
      Copy : Holder;
   end record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
   begin
      if not This.Copy.Is_Valid then
         This.Copy := This.Up;
      end if;

      return This.Copy.As_Iterator.Next;
   end Next;

   function Create return Root.Iterator'Class is
     (Iterator'(Up   => <>,
                Copy => <>));

end Iterators.Root.Impl_Copy;
