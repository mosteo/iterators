package body Iterators.Operators.Impl_Scan is

   type Operator is new Operators.Operator with record
      Scanner : Scanner_Fn;
      Prev    : Into.Elem_Holders.Holder; -- The running scan element
   end record;

   overriding
   function Next (This : in out Operator) return Into.Cursor'Class is
      Pos : constant From.Cursor'Class := This.Upstream.Next;
   begin
      if Pos.Is_Empty then
         return Into.New_Empty_Cursor;
      else
         This.Prev.Hold (This.Scanner (This.Prev.Get, Pos.Get));
         return Into.New_Const_Cursor (This.Prev.Reference);
      end if;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (Initial : Into.Any_Element;
                    Scanner : not null Scanner_Fn)
                    return Operators.Operator'Class
   is (Operator'(Operators.Operator with
                 Scanner => Scanner,
                 Prev    => Into.Elem_Holders.To_Holder (Initial)));

end Iterators.Operators.Impl_Scan;
