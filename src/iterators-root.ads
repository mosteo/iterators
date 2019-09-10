with AAA.Containers.Indefinite_Holders;

with Ada.Iterator_Interfaces;

generic
   type Any_Element (<>) is private;
package Iterators.Root with Preelaborate is

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

   type Cursor (<>) is tagged private;

   function Has_Element (This : Cursor) return Boolean;

   function Is_Empty (This : Cursor) return Boolean is (not This.Has_Element);

   function Get (This : Cursor) return Const_Ref;

   function Ref (This : Cursor) return Reference;

   function New_Cursor (Element : aliased in out Any_Element) return Cursor;

   function New_Const_Cursor (Element : aliased Any_Element) return Cursor;

   function New_Empty_Cursor return Cursor;

   package Ada_Iterator_Interfaces is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   --------------
   -- Iterator --
   --------------

   type Iterator is abstract tagged private with
     Constant_Indexing => Get_Const_Ref,
     Variable_Indexing => Get_Var_Ref,
     Default_Iterator => Iterate,
     Iterator_Element => Any_Element;

   function Next (This : in out Iterator) return Cursor'Class is abstract;

   function "&" (L, R : Iterator'Class) return Iterator'Class;
   --  Basic concatenator of iterators.

   ------------------------
   -- Standard Iteration --
   ------------------------

   --  Support for "of" notation:

   function Iterate (This : aliased Iterator)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class;

   function Get_Const_Ref (This : aliased Iterator'Class;
                           Pos  : Cursor'Class) return Const_Ref;

   function Get_Var_Ref (This : aliased in out Iterator'Class;
                         Pos  : Cursor'Class) return Reference;

   ---------------
   -- Operators --
   ---------------

   function "&" (L : Iterator'Class; R : Any_Element) return Iterator'Class;
   --  In-place aggregator of individual elements.

   function Append (Element : Any_Element) return Iterator'Class;
   --  Explicit variant of previous "&" operator.

   function Copy return Iterator'Class;
   --  Copies the preceding iterator, so it is not consumed by subsequent
   --  operators.

   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Iterator'Class;
   --  Let only pass elements accepted by the function argument.

   function No_Op return Iterator'Class;
   --  Does nothing.

   -------------
   -- Sources --
   -------------

   function Just (Element : Any_Element) return Iterator'Class;
   --  Convert an element into an iterator.
   --  See also "&" and the Iterators.From.* packages.

private

   type Element_Ptr       is access all Any_Element;
   type Element_Const_Ptr is access constant Any_Element;

   package Elem_Holders is new AAA.Containers.Indefinite_Holders (Any_Element);
   --  Used by some child packages.

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

   type Proto_Iterator is tagged null record;
   --  Needed to be able to store the upstream iterator without moving
   --  everything to a separate file.

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

end Iterators.Root;
