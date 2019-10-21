private generic
package Iterators.Transform.Impl_Map with Preelaborate is

   function Create
     (Map : not null access
        function (E : From.Any_Element) return Into.Any_Element)
      return Operator'Class;

end Iterators.Transform.Impl_Map;
