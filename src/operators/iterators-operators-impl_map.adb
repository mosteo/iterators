package body Iterators.Operators.Impl_Map is

   function Create
     (Map : not null access
        function (E : From.Any_Element) return Into.Any_Element)
      return Operator'Class is
      (raise Unimplemented);

end Iterators.Operators.Impl_Map;
