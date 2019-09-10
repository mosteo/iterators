--  with GNAT.IO; use GNAT.IO;

with Iterators.Root.Impl_Append;
with Iterators.Root.Impl_Copy;
with Iterators.Root.Impl_Filter;
with Iterators.Root.Impl_Just;
with Iterators.Root.Impl_No_Op;

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
     (Element => (if This.Data.Read_Only
                  then This.Data.Const_Ptr
                  else This.Data.Ptr));

   ---------
   -- Ref --
   ---------

   function Ref (This : Cursor) return Reference is
     (Element => (if This.Data.Read_Only
                  then raise Constraint_Error with "Iterator is read-only"
                  else This.Data.Ptr));

   ----------------
   -- New_Cursor --
   ----------------

   function New_Cursor (Element : aliased in out Any_Element) return Cursor is
     (Data => (Read_Only => False,
               Ptr       => Element'Access));

   ----------------------
   -- New_Const_Cursor --
   ----------------------

   function New_Const_Cursor (Element : aliased Any_Element) return Cursor is
     (Data => (Read_Only => True,
               Const_Ptr => Element'Access));

   ----------------------
   -- New_Empty_Cursor --
   ----------------------

   function New_Empty_Cursor return Cursor is
     (Data => (Read_Only => True,
               Const_Ptr => null));

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

   -----------------
   --  OPERATORS  --
   -----------------

   package  Append_Instance is new Impl_Append;
   function Append (Element : Any_Element) return Iterator'Class
                    renames Append_Instance.Create;
   function "&" (L : Iterator'Class; R : Any_Element) return Iterator'Class
                 renames Append_Instance."&";

   package  Filter_Instance is new Impl_Filter;
   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Iterator'Class is (Filter_Instance.Create (Tester));

   package  Just_Instance is new Impl_Just;
   function Just (Element : Any_Element) return Iterator'Class
                  renames Just_Instance.Create;

   package  Copy_Instance is new Impl_Copy;
   function Copy return Iterator'Class renames Copy_Instance.Create;

   package  No_Op_Instance is new Impl_No_Op;
   function No_Op return Iterator'Class renames No_Op_Instance.Create;

end Iterators.Root;
