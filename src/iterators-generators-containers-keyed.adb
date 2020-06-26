package body Iterators.Generators.Containers.Keyed is

   package Keyed renames Keyed_Iterators;

   -------------------
   -- To_Const_Iter --
   -------------------

   type Const_Iterator is new Iterator with record
      Col  : access constant Containers.Container;
      Pos  : Containers.Cursor;
      Pair : aliased Keyed.Pairs.Pairs;
   end record;

   overriding
   function Next (This : in out Const_Iterator) return Keyed.Iterators.Cursor'Class;

   procedure Fill_Pair (This : in out Const_Iterator) is
   begin
      if Containers.Has_Element (This.Pos) then
         This.Pair := Keyed.Pairs.New_Pair
           (Key => Keyed_Containers.Key (This.Pos),
            Pos => Root.New_Const_Cursor
              (Containers.Constant_Reference (This.Col.all, This.Pos).Element.all));
      end if;
   end Fill_Pair;

   overriding
   function Next (This : in out Const_Iterator) return Keyed.Iterators.Cursor'Class is
      --  Note: Iterators.Cursor is a keyed cursor!
   begin
      if Containers.Has_Element (This.Pos) then
         Fill_Pair (This);

         return Pos : constant Keyed.Iterators.Cursor :=
           Keyed.Iterators.New_Const_Cursor (This.Pair)
         do
            This.Pos := Containers.Next (This.Pos);
         end return;
      else
         return Keyed.Iterators.New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (C : aliased Container) return Iterator'Class is
     (Const_Iterator'(Col  => C'Unchecked_Access,
                      Pos  => Containers.First (C),
                      Pair => <>));

   -------------
   -- To_Iter --
   -------------

   type Var_Iterator is new Iterator with record
      Col  : access Containers.Container;
      Pos  : Containers.Cursor;
      Pair : aliased Keyed.Pairs.Pairs;
   end record;

   overriding
   function Next (This : in out Var_Iterator) return Keyed.Iterators.Cursor'Class;

   procedure Fill_Pair (This : in out Var_Iterator) is
   begin
      if Containers.Has_Element (This.Pos) then
         This.Pair := Keyed.Pairs.New_Pair
           (Key => Keyed_Containers.Key (This.Pos),
            Pos => Root.New_Cursor
              (Containers.Reference (This.Col.all, This.Pos).Element.all));
      end if;
   end Fill_Pair;

   overriding
   function Next (This : in out Var_Iterator) return Keyed.Iterators.Cursor'Class is
   begin
      if Containers.Has_Element (This.Pos) then
         Fill_Pair (This);

         return Pos : constant Keyed.Iterators.Cursor :=
           Keyed.Iterators.New_Cursor (This.Pair)
         do
            This.Pos := Containers.Next (This.Pos);
         end return;
      else
         return Keyed.Iterators.New_Empty_Cursor;
      end if;
   end Next;

   function Iter (C : aliased in out Container) return Iterator'Class is
     (Var_Iterator'(Col  => C'Unchecked_Access,
                    Pos  => Containers.First (C),
                    Pair => <>));

end Iterators.Generators.Containers.Keyed;
