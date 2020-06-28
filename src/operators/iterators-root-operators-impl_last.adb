package body Iterators.Root.Operators.Impl_Last is

   type Operator is new Operators.Operator with null record;

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out Operator) return Cursor'Class is
      Prev : Cursor'Class := This.Upstream.Next;
   begin
      loop
         declare
            Curr : constant Cursor'Class := (if Prev.Has_Element
                                             then This.Upstream.Next
                                             else New_Empty_Cursor);
         begin
            if not Curr.Has_Element then
               return Prev;
            else
               Prev := Curr;
            end if;
         end;
      end loop;
   end Next;

   ------------
   -- Create --
   ------------

   function Create return Operators.Operator'Class
   is (Operator'(Operators.Operator with null record));

end Iterators.Root.Operators.Impl_Last;
