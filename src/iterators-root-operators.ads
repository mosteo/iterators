with Iterators.Operators;

generic
package Iterators.Root.Operators with Preelaborate is

   package Operators is new Iterators.Operators (Root, Root);

   ---------------
   -- Operators --
   ---------------

   function Map (Map : not null access
                   function (E : Any_Element) return Any_Element)
                 return Operators.Operator'Class renames Operators.Map;

end Iterators.Root.Operators;
