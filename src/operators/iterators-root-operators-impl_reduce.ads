private generic
package Iterators.Root.Operators.Impl_Reduce with Preelaborate is

   function Reduce (It      : Iterator'Class;
                    Reducer : Root.Operators.Reducer)
                    return Any_Element;

end Iterators.Root.Operators.Impl_Reduce;
