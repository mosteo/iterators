private generic
package Iterators.Meta.Impl_Flatmap with Preelaborate is

   --  function Create (Map : not null access
   --                     function (Iter : Base_Root.Iterator'Class)
   --                               return Base_Root.Any_Element)
   --                   return Meta2iter.Operator'Class;

   function Create (Map : Base_Root.Iterator'Class := Base_Operators.No_Op)
                    return Meta2iter.Operator'Class;

end Iterators.Meta.Impl_Flatmap;
