with GNAT.IO; use GNAT.IO;

package body Iterators.Root is

   ------------------------------------
   -- ADA STANDARD ITERATION SUPPORT --
   ------------------------------------

   package Ada_Iterators is

      type Ada_Iterator is new Ada_Iterator_Interfaces.Forward_Iterator with record
         Base : access Iterator'Class;
      end record;

      overriding function First (This : Ada_Iterator) return Cursor is
        (Cursor (This.Base.Next));

      overriding function Next (This       : Ada_Iterator;
                                Pos_Unused : Cursor) return Cursor is
         (Cursor (This.Base.Next));

   end Ada_Iterators;

   -------------
   -- Iterate --
   -------------

   function Iterate (This : aliased Iterator)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class is
     (Ada_Iterators.Ada_Iterator'(Base => This'Unrestricted_Access));
   --  We need to store a RW pointer, so we must "cheat" here. If Ada iterators
   --  supported several signatures for ro/rw iterate this would not be needed.
   --  Even standard gnat containers use Unrestricted_Access for their
   --  iterators.

   -------------------
   -- Get_Const_Ref --
   -------------------

   function Get_Const_Ref (This : aliased Iterator'Class;
                           Pos  : Cursor'Class) return Const_Ref is (Pos.Get);

   -----------------
   -- Get_Var_Ref --
   -----------------

   function Get_Var_Ref (This : aliased in out Iterator'Class;
                         Pos  : Cursor'Class) return Reference is (Pos.Ref);

   --------------------
   -- REST OF THINGS --
   --------------------

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Cursor) return Boolean is
     (This.Get.Element /= null);

   ---------
   -- Get --
   ---------

   function Get (This : Cursor) return Const_Ref is
     (Element => (if This.Read_Only
                  then This.Const_Ptr
                  else This.Ptr));

   ---------
   -- Ref --
   ---------

   function Ref (This : Cursor) return Reference is
     (Element => (if This.Read_Only
                  then raise Constraint_Error with "Iterator is read-only"
                  else This.Ptr));

   ----------------
   -- New_Cursor --
   ----------------

   function New_Cursor (Element : aliased in out Any_Element) return Cursor is
     (Cursor'(Read_Only => False,
              Ptr       => Element'Access));

   ----------------------
   -- New_Const_Cursor --
   ----------------------

   function New_Const_Cursor (Element : aliased Any_Element) return Cursor is
     (Cursor'(Read_Only => True,
              Const_Ptr  => Element'Access));

   ----------------------
   -- New_Empty_Cursor --
   ----------------------

   function New_Empty_Cursor return Cursor is
     (Cursor'(Read_Only => False,
              Ptr       => null));
   --  This is RW cursor because it is common to both RO/RW iterators. This is
   --  OK because nobody should try to access the element of an empty cursor
   --  anyway (and that will result in an access check failed).

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
         if Result.Up.Is_Empty then
            Result.Up := L.To_Holder;
         else
            Result.Up :=
              To_Holder (L & Iterator'Class (Result.Up.Reference.Element.all));
         end if;
      end return;
   end "&";

   ------------
   -- Append --
   ------------

   type Append_Iterator is new Iterator with record
      Extra : Holder; -- A Just iterator
   end record;

   overriding
   function Next (This : in out Append_Iterator) return Cursor'Class is
      Prev : constant Cursor'Class := This.Upstream.Next;
   begin
      if Prev.Has_Element then
         return Prev;
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
   function Next (This : in out Filter_Iterator) return Cursor'Class is
   begin
      loop
         declare
            Pos : constant Cursor'Class := This.Upstream.Next;
         begin
            if Pos.Is_Empty or else This.Tester (Pos.Get) then
               return Pos;
            else
               -- Skip non-complying value
               null;
            end if;
         end;
      end loop;
   end Next;

   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
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
   function Next (This : in out Just_Iterator) return Cursor'Class is
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

   function Just (Element : Any_Element) return Iterator'Class is
     (Just_Iterator'(Up      => <>,
                     Element => Elem_Holders.To_Holder (Element),
                     Given   => <>));

   -----------
   -- No_Op --
   -----------

   type No_Op_Iterator is new Iterator with null record;

   overriding
   function Next (This : in out No_Op_Iterator) return Cursor'Class is
   begin
      Put_Line ("ASDF");
      return This.Upstream.Next;
   end Next;

   function No_Op return Iterator'Class is
   begin
      return No_Op_Iterator'(Up => <>);
   end No_Op;

end Iterators.Root;
