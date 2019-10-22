private generic
package Iterators.Root.Operators.Impl_Filter with Preelaborate is

   type Testers is access function (Element : Any_Element) return Boolean;
   --  Explicit because of accessibility checks.

   function Create (Tester : Testers) return Operator'Class;

end Iterators.Root.Operators.Impl_Filter;
