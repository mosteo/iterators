private generic
package Iterators.Operators.Impl_Map with Preelaborate is

   type Mapper is access
     function (E : From.Any_Element) return Into.Any_Element;

   function Create (Map : not null Mapper) return Operator'Class;

end Iterators.Operators.Impl_Map;
