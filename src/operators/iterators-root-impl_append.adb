package body Iterators.Root.Impl_Append is

   type Operator is new Root.Operator with record
      Extra : Holder; -- A Just iterator
   end record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
      Prev : constant Cursor'Class := This.Upstream.Next;
   begin
      if Prev.Has_Element then
         return Prev;
      else
         return This.Extra.As_Iterator.Next;
      end if;
   end Next;

   function Create (Element : Any_Element) return Root.Operator'Class is
     (Operator'(Up    => <>,
                Extra => Just (Element).To_Holder));

   function "&" (L : Root.Iterator'Class;
                 R : Any_Element)
                 return Root.Iterator'Class is
     (Operator'(Up    => L.To_Holder,
                Extra => Just (R).To_Holder));

end Iterators.Root.Impl_Append;
