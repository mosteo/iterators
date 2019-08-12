package body Iterators.Root.Sequences is

   -------------
   -- Collect --
   -------------

   function Collect return List is (Lists.Empty_List with null record);

   ---------
   -- "&" --
   ---------

   function "&" (L : Iterator'Class;
                 R : List)
                 return List
   is
      Result : List           := R;
      RW_It  : Iterator'Class := L;
   begin
      loop
         declare
            Pos : constant Cursor'Class := RW_It.Next;
         begin
            exit when Pos.Is_Empty;
            Result.Append (Pos.Get);
         end;
      end loop;

      return Result;
   end "&";

   ----------
   -- Iter --
   ----------

   type List_Iterator is new Iterator with record
      List : access Sequences.List'Class;
      Pos  : Lists.Cursor;
   end record;

   overriding
   function Next (This : in out List_Iterator) return Cursor'Class is
   begin
      if Lists.Has_Element (This.Pos) then
         return Pos : constant Cursor :=
           New_Cursor (This.List.Reference (This.Pos))
         do
            This.Pos := Lists.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Iter (L : aliased in out List'Class) return Iterator'Class is
   begin
      return List_Iterator'(Up   => <>,
                            List => L'Access,
                            Pos  => L.First);
   end Iter;

   ----------------
   -- Const_Iter --
   ----------------

   type List_Const_Iterator is new Iterator with record
      List : access constant Sequences.List'Class;
      Pos  : Lists.Cursor;
   end record;

   overriding
   function Next (This : in out List_Const_Iterator) return Cursor'Class is
   begin
      if Lists.Has_Element (This.Pos) then
         return Pos : constant Cursor :=
           New_Const_Cursor (This.List.Constant_Reference (This.Pos))
         do
            This.Pos := Lists.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (L : aliased List'Class) return Iterator'Class is
   begin
      return List_Const_Iterator'(Up   => <>,
                                  List => L'Access,
                                  Pos  => L.First);
   end Const_Iter;


end Iterators.Root.Sequences;
