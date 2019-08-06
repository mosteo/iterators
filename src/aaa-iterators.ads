with AAA.Containers.Indefinite_Holders;

generic
   type Any_Element (<>) is private;
package AAA.Iterators is

   type Reference (Element : access Any_Element) is limited null record with
     Implicit_Dereference => Element;

   type Const_Ref (Element : access constant Any_Element) is limited null record with
     Implicit_Dereference => Element;

   type Cursor (<>) is tagged private;

--     function Get (This : Cursor) return Const_Ref;
--
--     function Ref (This : Cursor) return Reference;

   type Iterator is abstract tagged private;

   function Next (This : in out Iterator) return Reference is abstract;

   function "&" (L, R : Iterator'Class) return Iterator'Class;

   ---------------
   -- Operators --
   ---------------

   function "&" (L : Iterator'Class; R : Any_Element) return Iterator'Class;

   function Filter
     (I      : Iterator'Class;
      Tester : access function (Element : Any_Element) return Boolean)
      return Iterator'Class;

   -------------
   -- Sources --
   -------------

   function Just (Element : Any_Element) return Iterator'Class;

private

   type Cursor (Read_Only : Boolean) is tagged null record;

   function No_Element return Reference is (Element => null);

   type Proto_Iterator is tagged null record;

   package Holders is new AAA.Containers.Indefinite_Holders
     (Proto_Iterator'Class);

   type Holder is new Holders.Holder with null record;

   type Iterator is abstract new Proto_Iterator with record
      Up : Holder;
   end record;

   type Iterator_Reference (Ptr : access Iterator'Class) is
   limited null record with
     Implicit_Dereference => Ptr;

   function As_Iterator (This : in out Holder) return Iterator_Reference;

   function Upstream (This : in out Iterator'Class) return Iterator_Reference;

end AAA.Iterators;
