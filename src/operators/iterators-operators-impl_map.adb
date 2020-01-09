package body Iterators.Operators.Impl_Map is

   type Operator is new Operators.Operator with record
      Map  : Mapper;
      Elem : Into.Elem_Holders.Holder; -- The mapped element
   end record;

   overriding
   function Next (This : in out Operator) return Into.Cursor'Class is
   begin
      loop
         declare
            Pos : constant From.Cursor'Class := This.Upstream.Next;
         begin
            if Pos.Is_Empty then
               return Into.New_Empty_Cursor;
            else
               This.Elem.Hold (This.Map (Pos.Get));
               return Into.New_Cursor (This.Elem.Reference);
            end if;
         end;
      end loop;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (Map : not null Mapper) return Operators.Operator'Class is
     (Operator'(Operators.Operator with
                Map  => Map,
                Elem => <>));

end Iterators.Operators.Impl_Map;
