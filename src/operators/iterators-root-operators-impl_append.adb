package body Iterators.Root.Operators.Impl_Append is

   type Operator is new Root.Operators.Operator with record
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

   function Create (Element : Any_Element) return Root.Operators.Operator'Class is
     (Operator'(Root.Operators.Operator with
                Extra => Just (Element).To_Iterator.To_Holder));

   function Append (L : Root.Iterable'Class;
                    R : Any_Element)
                    return Root.Iterable'Class is
        (Operators.Concatenate (L, Append (R)));

end Iterators.Root.Operators.Impl_Append;
