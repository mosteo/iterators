package body Iterators.Root.Operators.Impl_Reduce with Preelaborate is

   function Reduce (It      : Iterator'Class;
                    Reducer : Root.Operators.Reducer)
                    return Any_Element
   is
      Value : Elem_Holders.Holder := Reducer.Initial;
   begin
      for Elem of It loop
         Value.Hold (Reducer.Reduce_Fn (Value.Get, Elem));
      end loop;

      return Value.Get;
   end Reduce;

end Iterators.Root.Operators.Impl_Reduce;
