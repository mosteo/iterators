package body Iterators.Root.Impl_Just is

   type Iterator is new Root.Iterator with record
      Element : Elem_Holders.Holder;
      Given   : Boolean := False;
   end record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
   begin
      if not This.Given then
         return Pos : constant Cursor :=
           New_Cursor (This.Element.Reference)
         do
            This.Given := True;
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Create (Element : Any_Element) return Root.Iterator'Class is
     (Iterator'(Element => Elem_Holders.To_Holder (Element),
                Given   => <>));

end Iterators.Root.Impl_Just;
