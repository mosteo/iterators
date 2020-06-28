private generic
package Iterators.Meta.Impl_Window with Preelaborate is

   function Create (Size : Positive;
                    Skip : Positive)
                    return Meta_Operator'Class
     with Pre => Skip <= Size;
   --  Skip > Size is doable but not yet implemented

end Iterators.Meta.Impl_Window;
