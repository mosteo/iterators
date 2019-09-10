package body Iterators.Root.Impl_No_Op is

   type Iterator is new Root.Iterator with null record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
   begin
      return This.Upstream.Next;
   end Next;

   function Create return Root.Iterator'Class is
     (Iterator'(Up => <>));

end Iterators.Root.Impl_No_Op;
