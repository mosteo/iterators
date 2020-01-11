package body Iterators.Generators.Containers is

   use Root;

   -------------------
   -- To_Const_Iter --
   -------------------

   type Const_Iterator is new Iterator with record
      Col : access constant Containers.Container;
      Pos : Container_Traits.Cursor;
   end record;

   overriding
   function Next (This : in out Const_Iterator) return Cursor'Class is
   begin
      if Container_Traits.Has_Element (This.Pos) then
         return Pos : constant Cursor :=
           New_Const_Cursor (Container_Traits.Constant_Reference (This.Col.all, This.Pos).Element.all)
         do
            This.Pos := Container_Traits.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (C : aliased Container) return Iterator'Class is
     (Const_Iterator'(Col => C'Access,
                      Pos => Container_Traits.First (C)));

   -------------
   -- To_Iter --
   -------------

   type Var_Iterator is new Iterator with record
      Col : access Containers.Container;
      Pos : Container_Traits.Cursor;
   end record;

   overriding
   function Next (This : in out Var_Iterator) return Cursor'Class is
   begin
      if Container_Traits.Has_Element (This.Pos) then
         return Pos : constant Cursor :=
           New_Cursor (Container_Traits.Reference (This.Col.all, This.Pos).Element.all)
         do
            This.Pos := Container_Traits.Next (This.Pos);
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Iter (C : aliased in out Container) return Iterator'Class is
     (Var_Iterator'(Col => C'Access,
                    Pos => Container_Traits.First (C)));

end Iterators.Generators.Containers;
