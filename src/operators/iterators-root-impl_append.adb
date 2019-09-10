package body Iterators.Root.Impl_Append is

   type Iterator is new Root.Iterator with record
      Extra : Holder; -- A Just iterator
   end record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
      Prev : constant Cursor'Class := This.Upstream.Next;
   begin
      if Prev.Has_Element then
         return Prev;
      else
         return This.Extra.As_Iterator.Next;
      end if;
   end Next;

   function Create (Element : Any_Element) return Root.Iterator'Class is
     (Iterator'(Up    => <>,
                Extra => Just (Element).To_Holder));

   function "&" (L : Root.Iterator'Class;
                 R : Any_Element)
                 return Root.Iterator'Class is
     (Iterator'(Up    => L.To_Holder,
                Extra => Just (R).To_Holder));

end Iterators.Root.Impl_Append;
