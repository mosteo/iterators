private generic
package Iterators.Operators.Impl_Scan with Preelaborate is

   type Scanner_Fn is access function (L : Into.Any_Element;
                                       R : From.Any_Element)
                                       return Into.Any_Element;

   function Create (Initial : Into.Any_Element;
                    Scanner : not null Scanner_Fn)
                    return Operator'Class;

end Iterators.Operators.Impl_Scan;
