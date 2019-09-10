package body Iterators.Root.Impl_Filter is

   type Iterator is new Root.Iterator with record
      Tester : Testers;
   end record;

   overriding
   function Next (This : in out Iterator) return Cursor'Class is
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

   function Create
     (Tester : Testers)
      return Root.Iterator'Class is
     (Iterator'(Up     => <>,
                Tester => Tester));

end Iterators.Root.Impl_Filter;
