with AAA.Containers.Indefinite_Holders;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Iterator_Interfaces;

generic
   type Any_Element (<>) is private;
package Iterators.Root with Preelaborate is

   subtype Elements is Any_Element;
   --  Work around visibility bug.

   ----------------
   -- References --
   ----------------

   type Reference (Element : access Any_Element) is limited null record with
     Implicit_Dereference => Element;

   type Const_Ref (Element : access constant Any_Element) is limited null record with
     Implicit_Dereference => Element;

   ------------
   -- Cursor --
   ------------

   type Cursor is tagged private;

   function Has_Element (This : Cursor) return Boolean;

   function Is_Empty (This : Cursor) return Boolean is (not This.Has_Element);

   function Element (This : Cursor) return Any_Element;

   function Get (This : Cursor) return Const_Ref;

   function Ref (This : Cursor) return Reference;

   function Val (This : Cursor) return Any_Element;

   function New_Cursor (Element : aliased in out Any_Element) return Cursor;

   function New_Const_Cursor (Element : aliased Any_Element) return Cursor;

   function New_Empty_Cursor return Cursor;

   package Ada_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   --------------
   -- Iterable --
   --------------

   type Iterator;

   type Iterable is limited interface;
   --  Anything that can be turned into an iterator. This helps in reducing
   --  prototype duplications with functional/imperative notations, and with
   --  some operators that take iterators as arguments.

   function To_Iterator (This : Iterable) return Iterator'Class is abstract;

   --------------
   -- Iterator --
   --------------

   type Iterator is abstract new Iterable with null record with
     Constant_Indexing => Get_Const_Ref,
     Variable_Indexing => Get_Var_Ref,
     Default_Iterator => Iterate,
     Iterator_Element => Any_Element;

   function Next (This : in out Iterator) return Cursor'Class is abstract;
   --  Get next element, if any

   function To_Iterator (This : Iterator) return Iterator'Class is
     (Iterator'Class (This)) with Inline;
   --  Identity

   type Iterator_Reference (Ptr : access Iterator'Class) is limited null record
     with Implicit_Dereference => Ptr;

   ----------
   -- List --
   ----------

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Any_Element);

   subtype List is Lists.List;

   ------------------------
   -- Standard Iteration --
   ------------------------

   --  Support for "of" notation:

   function Iterate (This : aliased Iterator'Class)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class;

   function Get_Const_Ref (This : aliased Iterator'Class;
                           Pos  : Cursor'Class) return Const_Ref;

   function Get_Var_Ref (This : aliased in out Iterator'Class;
                         Pos  : Cursor'Class) return Reference;

   pragma Warnings (Off);
   function Iter (This : aliased Iterator'Class)
                  return access Iterator'Class is
     (new Iterator'Class'(This));
   pragma Warnings (On);
   --  Workaround to allow somewhat simple "of" iteration on sequences: you
   --  can qualify the chain expression and take the Iter to have a RW copy.
   --  "of", however, is buggy and using not the proper returned value.
   --  This in turn forces to create a copy here and return it, causing a leak.
   --  TODO: REMOVE LEAK, REPORT BUG, FIND ANOTHER WORKAROUND.

   -----------
   -- Debug --
   -----------

   procedure Print_Tag (This : Iterator) is null;
   --  Declared abstract here so operators can redispatch across types
   function Print_Tag (This : Iterator'Class) return Iterator'Class;

   -------------
   -- Holders --
   -------------

   package Holders is new AAA.Containers.Indefinite_Holders (Iterator'Class);

   type Holder is new Holders.Holder with null record;

   function As_Iterator (This : in out Holder) return Iterator_Reference;

   package Elem_Holders is new AAA.Containers.Indefinite_Holders (Any_Element);
   --  Used by some child packages.

   --------------------
   -- Iterable trait --
   --------------------

   --  This interface here allows later simpler notation for imperative
   --  interfaces.

--     type Iterable is limited interface;

--     function Iterate (This : Iterable) return Iterator'Class is abstract;

private

   type Element_Ptr       is access all Any_Element;
   type Element_Const_Ptr is access constant Any_Element;

   type Cursor_Data (Read_Only : Boolean := False) is record
      case Read_Only is
         when True  => Const_Ptr : Element_Const_Ptr;
         when False => Ptr       : Element_Ptr;
      end case;
   end record;
   --  It seems having having constraints in the cursor makes things
   --  innecessarily complicated for "of" notation, since we cannot
   --  ensure that a cursor in the LHS has always the same constraints.
   --  As tagged types cannot have discriminant defaults, this is a workaround.

   type Cursor is tagged record
      Data : Cursor_Data;
   end record;

end Iterators.Root;
