package body Iterators.Root.Operators.Impl_Take is

   type Operator is new Operators.Operator with record
      Remaining : Natural;
   end record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      if This.Remaining = 0 then
         return New_Empty_Cursor;
      else
         return C : constant Cursor'Class := This.Upstream.Next do
            if C.Is_Empty then
               This.Remaining := 0;
            else
               This.Remaining := This.Remaining - 1;
            end if;
         end return;
      end if;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (At_Most : Natural) return Operators.Operator'Class is
     (Operator'(Operators.Operator with
                Remaining => At_Most));

end Iterators.Root.Operators.Impl_Take;
