with Ada.Unchecked_Conversion;

package body Iterators.Root.Adapters is

   type Elem_Ptr_Src is access all Containers.Element;
   type Elem_Ptr_Dst is access all Any_Element;

   function Same is new Ada.Unchecked_Conversion
     (Elem_Ptr_Src, Elem_Ptr_Dst);

   -------------------
   -- To_Const_Iter --
   -------------------

   type Const_Iterator is new Iterator with record
      Col : access constant Containers.Container;
      Pos : Containers.Cursor;
   end record;

   overriding
   function Next (This : in out Const_Iterator) return Cursor'Class is
   begin
      if Containers.Has_Element (This.Pos) then
         return Pos : constant Cursor := New_Const_Cursor (Same (Containers.Constant_Reference (This.Col.all, This.Pos)).all)
         do
            This.Pos := Containers.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (C : aliased Container) return Iterator'Class is
     (Const_Iterator'(Up  => <>,
                      Col => C'Access,
                      Pos => Containers.First (C)));

   -------------
   -- To_Iter --
   -------------

   type Var_Iterator is new Iterator with record
      Col : access Containers.Container;
      Pos : Containers.Cursor;
   end record;

   overriding
   function Next (This : in out Var_Iterator) return Cursor'Class is
   begin
      if Containers.Has_Element (This.Pos) then
         return Pos : constant Cursor := New_Cursor (Same (Containers.Reference (This.Col.all, This.Pos)).all)
         do
            This.Pos := Containers.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Iter (C : aliased in out Container) return Iterator'Class is
     (Var_Iterator'(Up  => <>,
                    Col => C'Access,
                    Pos => Containers.First (C)));

end Iterators.Root.Adapters;
