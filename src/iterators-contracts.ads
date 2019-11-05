package Iterators.Contracts with Pure is

   type Iterator is interface;

   --  A linkable is anything that can appear in the rhs of "&" to transform
   --  the sequence of the lhs iterator. Operators return a new iterator,
   --  whereas Reducers consume the iterator sequence to return a value.

   type Linkable is interface;

   procedure Concatenate (This : in out Linkable; Parent : Iterator'Class)
   is abstract;

   --  A Reducer is a particular linkable that consumes the iterator set as its
   --  parent, returning itself as the reduction. E.g.; a List can start as an
   --  empty reducer to be returned as a filled list.

   type Reducer is interface;

   function Reduce (This   : Reducer;
                    Parent : Iterator'Class) return Reducer is abstract;
   --  Reduce implementations must override this function to provide the actual
   --  reduction.

end Iterators.Contracts;
