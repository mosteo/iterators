package body AAA.Iterators is

   -----------------
   -- As_Iterator --
   -----------------

   function As_Iterator (This : in out Holder) return Iterator_Reference is
     (Ptr => Iterator'Class (This.Reference.Element.all)'Access);

   --------------
   -- Upstream --
   --------------

   function Upstream (This : in out Iterator'Class) return Iterator_Reference is
     (This.Up.As_Iterator);

   package Elem_Holders is new AAA.Containers.Indefinite_Holders (Any_Element);

   ---------
   -- "&" --
   ---------

   function "&" (L, R : Iterator'Class) return Iterator'Class is
   begin
      return Result : Iterator'Class := R do
         Result.Up := L.To_Holder;
      end return;
   end "&";

   ------------
   -- Append --
   ------------

   type Append_Iterator is new Iterator with record
      Extra : Holder; -- A Just iterator
   end record;

   overriding
   function Next (This : in out Append_Iterator) return Reference is
      Prev : constant Reference := This.Upstream.Next;
   begin
      if Prev.Element /= null then
         return (Element => Prev.Element);
      else
         return This.Extra.As_Iterator.Next;
      end if;
   end Next;

   function "&" (L : Iterator'Class; R : Any_Element) return Iterator'Class is
     (Append_Iterator'(Up    => L.To_Holder,
                       Extra => Just (R).To_Holder));

   ------------
   -- Filter --
   ------------

   type Filter_Iterator
     (Tester : access function (Element : Any_Element) return Boolean)
   is new Iterator with null record;

   overriding
   function Next (This : in out Filter_Iterator) return Reference is
   begin
      loop
         declare
            Ref : constant Reference := This.Upstream.Next;
         begin
            if Ref.Element = null or else This.Tester (Ref) then
               return Result : constant Reference := (Element => Ref.Element);
            else
               -- Skip non-complying value
               null;
            end if;
         end;
      end loop;
   end Next;

   function Filter
     (I      : Iterator'Class;
      Tester : access function (Element : Any_Element) return Boolean)
      return Iterator'Class is
     (raise Program_Error);

   ----------
   -- Just --
   ----------

   type Just_Iterator is new Iterator with record
      Element : Elem_Holders.Holder;
      Given   : Boolean := False;
   end record;

   overriding
   function Next (This : in out Just_Iterator) return Reference is
   begin
      if not This.Given then
         return Ref : constant Reference :=
           (Element => This.Element.Reference.Element)
         do
            This.Given := True;
         end return;
      else
         return No_Element;
      end if;
   end Next;

   function Just (Element : Any_Element) return Iterator'Class is
     (Just_Iterator'(Up      => <>,
                     Element => Elem_Holders.To_Holder (Element),
                     Given   => <>));

end AAA.Iterators;
