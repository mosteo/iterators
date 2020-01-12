private generic
package Iterators.Root.Operators.Impl_Append with Preelaborate is

   function Create (Element : Any_Element) return Operator'Class;

   function Append (L : Iterator'Class; R : Any_Element) return Iterator'Class;
   --  To be used as "&"

end Iterators.Root.Operators.Impl_Append;
