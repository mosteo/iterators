package body Iterators.Root.Operators.Impl_Filter is

   type Operator is new Operators.Operator with record
      Tester : Testers;
   end record;

   overriding
   function Next (This : in out Operator) return Cursor'Class is
   begin
      loop
         declare
            Pos : constant Cursor'Class := This.Upstream.Next;
         begin
            if Pos.Is_Empty or else This.Tester (Pos.Get) then
               return Pos;
            else
               -- Skip non-complying value
               null;
            end if;
         end;
      end loop;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (Tester : Testers) return Operators.Operator'Class is
     (Operator'(Operators.Operator with
                Tester => Tester));

end Iterators.Root.Operators.Impl_Filter;
