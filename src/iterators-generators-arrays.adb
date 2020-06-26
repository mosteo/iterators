package body Iterators.Generators.Arrays is

   use Root;

   ----------------
   -- Const_Iter --
   ----------------

   type Const_Iterator is new Iterator with record
      Col   : access constant Element_Array;
      Pos   : Any_Index;
      Ended : Boolean;
   end record;

   overriding
   function Next (This : in out Const_Iterator) return Cursor'Class is
   begin
      if not This.Ended and then This.Pos <= This.Col'Last then
         return Pos : constant Cursor :=
           New_Const_Cursor (This.Col (This.Pos))
         do
            if This.Pos = This.Col'Last then
               This.Ended := True;
            else
               This.Pos := Any_Index'Succ (This.Pos);
            end if;
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Const_Iter (C : aliased Element_Array) return Root.Iterator'Class
   is (Const_Iterator'(Col   => C'Unchecked_Access,
                       Pos   => C'First,
                       Ended => C'Length = 0));

   ----------
   -- Iter --
   ----------

   type Var_Iterator is new Iterator with record
      Col   : access Element_Array;
      Pos   : Any_Index;
      Ended : Boolean;
   end record;

   overriding
   function Next (This : in out Var_Iterator) return Cursor'Class is
   begin
      if not This.Ended and then This.Pos <= This.Col'Last then
         return Pos : constant Cursor :=
           New_Cursor (This.Col (This.Pos))
         do
            if This.Pos = This.Col'Last then
               This.Ended := True;
            else
               This.Pos := Any_Index'Succ (This.Pos);
            end if;
         end return;
      else
         return New_Empty_Cursor;
      end if;
   end Next;

   function Iter (C : aliased in out Element_Array) return Root.Iterator'Class
   is (Var_Iterator'(Col   => C'Unchecked_Access,
                     Pos   => C'First,
                     Ended => C'Length = 0));

end Iterators.Generators.Arrays;
