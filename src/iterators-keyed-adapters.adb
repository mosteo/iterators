with Ada.Unchecked_Conversion;

package body Iterators.Keyed.Adapters is

--     -------------
--     -- Collect --
--     -------------
--
--     function Collect return Container is
--        Empty : Container with Warnings => Off;
--        --  Hoping that this will perform the expected default initialization...
--     begin
--        return Empty;
--     end Collect;
--
--     -------------
--     -- Collect --
--     -------------
--
--     function Collect (It : Iterator'Class) return Container is (It & Collect);
--
--     ---------
--     -- "&" --
--     ---------
--
--     function "&" (L : Iterator'Class;
--                   R : Container)
--                   return Container is
--     begin
--        return Result : Container := R do
--           for E of L loop
--              Containers.Append (Result, E, 1);
--           end loop;
--        end return;
--     end "&";

   -------------------
   -- To_Const_Iter --
   -------------------

   type Const_Iterator is new Iterator with record
      Col  : access constant Containers.Container;
      Pos  : Containers.Cursor;
      Pair : aliased Keyed.Pairs.Pairs;
   end record;

   overriding
   function Next (This : in out Const_Iterator) return Iterators.Cursor'Class;

   procedure Fill_Pair (This : in out Const_Iterator) is
   begin
      if Containers.Has_Element (This.Pos) then
         This.Pair := Pairs.New_Pair
           (Key => Keyed_Containers.Key (This.Pos),
            Pos => Unkeyed.New_Const_Cursor
              (Containers.Constant_Reference (This.Col.all, This.Pos).Element.all));
      end if;
   end Fill_Pair;

   overriding
   function Next (This : in out Const_Iterator) return Iterators.Cursor'Class is
      --  Note: Iterators.Cursor is a keyed cursor!
   begin
      if Containers.Has_Element (This.Pos) then
         Fill_Pair (This);

         return Pos : constant Iterators.Cursor := Iterators.New_Const_Cursor (This.Pair)
         do
            This.Pos := Containers.Next (This.Pos);
         end return;
      else
         return Iterators.New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (C : aliased Container) return Iterator'Class is
     (Const_Iterator'(Col  => C'Access,
                      Pos  => Containers.First (C),
                      Pair => <>));

--     -------------
--     -- To_Iter --
--     -------------
--
--     type Var_Iterator is new Iterator with record
--        Col : access Containers.Container;
--        Pos : Containers.Cursor;
--     end record;
--
--     overriding
--     function Next (This : in out Var_Iterator) return Cursor'Class is
--     begin
--        if Containers.Has_Element (This.Pos) then
--           return Pos : constant Cursor :=
--             New_Cursor (Containers.Reference (This.Col.all, This.Pos).Element.all)
--           do
--              This.Pos := Containers.Next (This.Pos);
--           end return;
--        else
--           return New_Empty_Cursor;
--        end if;
--     end Next;
--
--     function Iter (C : aliased in out Container) return Iterator'Class is
--       (Var_Iterator'(Col => C'Access,
--                      Pos => Containers.First (C)));

end Iterators.Keyed.Adapters;
