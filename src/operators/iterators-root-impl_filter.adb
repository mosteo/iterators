package body Iterators.Root.Impl_Filter is

   type Operator is new Root.Operator with record
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

   function Create (Tester : Testers) return Root.Operator'Class is
     (Operator'(Up     => <>,
                Tester => Tester));

end Iterators.Root.Impl_Filter;
