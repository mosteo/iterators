with Ada.Unchecked_Conversion;

package body Iterators.Root.Adapters is

   -------------
   -- Collect --
   -------------

   function Collect return Container is
      Empty : Container with Warnings => Off;
      --  Hoping that this will perform the expected default initialization...
   begin
      return Empty;
   end Collect;

   ---------
   -- "&" --
   ---------

   function "&" (L : Iterator'Class;
                 R : Container)
                 return Container
   is
      Result : Container      := R;
      RW_It  : Iterator'Class := L;
   begin
      loop
         declare
            Pos : constant Cursor'Class := RW_It.Next;
         begin
            exit when Pos.Is_Empty;
            Containers.Append (Result, Pos.Get, 1);
         end;
      end loop;

      return Result;
   end "&";

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
         return Pos : constant Cursor :=
           New_Const_Cursor (Containers.Constant_Reference (This.Col.all, This.Pos).Element.all)
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
         return Pos : constant Cursor :=
           New_Cursor (Containers.Reference (This.Col.all, This.Pos).Element.all)
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
