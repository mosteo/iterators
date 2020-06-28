package body Iterators.Root is

   -----------------
   -- As_Iterator --
   -----------------

   function As_Iterator (This : in out Holder) return Iterator_Reference is
     (Ptr => Iterator'Class (This.Reference.Element.all)'Access);

   ------------------------------------
   -- ADA STANDARD ITERATION SUPPORT --
   ------------------------------------

   package Ada_Iterators is

      type Ada_Iterator is new Ada_Iterator_Interfaces.Forward_Iterator with record
         Base : Holder;
      end record;

      overriding function First (This : Ada_Iterator) return Cursor is
        (Cursor (This.Base.Unchecked_Reference.Next));

      overriding function Next (This       : Ada_Iterator;
                                Pos_Unused : Cursor) return Cursor is
        (Cursor (This.Base.Unchecked_Reference.Next));

   end Ada_Iterators;

   -------------
   -- Iterate --
   -------------

   function Iterate (This : aliased Iterator'Class)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class is
     (Ada_Iterators.Ada_Iterator'(Base => This.To_Holder));

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

   -------------
   -- Element --
   -------------

   function Element (This : Cursor) return Any_Element is
     (This.Get.Element.all);

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

   ---------
   -- Val --
   ---------

   function Val (This : Cursor) return Any_Element is (This.Get.Element.all);

   ----------------
   -- New_Cursor --
   ----------------

   function New_Cursor (Element : aliased in out Any_Element) return Cursor is
     (Data => (Read_Only => False,
               Ptr       => Element'Unchecked_Access));

   ----------------------
   -- New_Const_Cursor --
   ----------------------

   function New_Const_Cursor (Element : aliased Any_Element) return Cursor is
     (Data => (Read_Only => True,
               Const_Ptr => Element'Unchecked_Access));

   ----------------------
   -- New_Empty_Cursor --
   ----------------------

   function New_Empty_Cursor return Cursor is
     (Data => (Read_Only => True,
               Const_Ptr => null));

   ---------------
   -- Print_Tag --
   ---------------

   function Print_Tag (This : Iterator'Class) return Iterator'Class is
   begin
      This.Print_Tag;
      return This;
   end Print_Tag;

   --------------------
   -- LIST ITERATORS --
   --------------------

   type List_Iterator is new Iterator with record
      Cont  : access List;
      Const : Boolean;
      Pos   : Lists.Cursor;
   end record;

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out List_Iterator) return Cursor'Class
   is

      -----------------
      -- Next_Cursor --
      -----------------

      function Next_Cursor return Cursor'Class
      is (if This.Const then
             New_Const_Cursor (This.Cont.Constant_Reference (This.Pos))
          else
             New_Cursor (This.Cont.Reference (This.Pos).Element.all));

   begin
      if not Lists.Has_Element (This.Pos) then
         return New_Empty_Cursor;
      end if;

      return C : constant Cursor'Class := Next_Cursor do
         if Lists.Has_Element (This.Pos) then
            Lists.Next (This.Pos);
         end if;
      end return;
   end Next;

   function Const_Iter (This : List) return Iterator'Class
   is (List_Iterator'(Iterator with
                      Cont => This'Unrestricted_Access,
                      Const => True,
                      Pos   => This.First));

   function Iter (This : List) return Iterator'Class
   is (List_Iterator'(Iterator with
                      Cont => This'Unrestricted_Access,
                      Const => False,
                      Pos   => This.First));

end Iterators.Root;
