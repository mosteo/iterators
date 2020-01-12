private generic
package Iterators.Operators.Impl_Flat_Map with Preelaborate is

   type Mapper is access
     function (E : From.Any_Element) return Into.Iterable'Class;

   function Create (Map : not null Mapper) return Operator'Class;

end Iterators.Operators.Impl_Flat_Map;
