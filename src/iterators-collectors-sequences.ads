with Iterators.Root;
with Iterators.Traits.Containers.Appendable;

generic
   with package Root is new Iterators.Root (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Root.Any_Element, others => <>);
   with package Appendable is new Containers.Appendable (<>);
package Iterators.Collectors.Sequences with Preelaborate is

   --  Collect into a container that respects the appending order.

   package Containers_Bis renames Containers;
   --  Visibility bug workaround.

   function "&" (L : Root.Iterator'Class;
                 R : Containers.Container)
                    return Containers.Container;
   --  To be used with Collect.
   --  Alternatively, if R is not empty, L and then R will be collected.

   function Collect return Containers.Container;
   --  Returns an empty container, to have a unique signature for the previous
   --  "&" operator.
   --  Trivia: since we need to qualify for "of" to work in a loop (e.g., for
   --  I of A & B loop won't work because of the &), we can safely overload the
   --  Collect subprogram.

   function Collect (It : Root.Iterator'Class) return Containers.Container;
   --  Alternate regular function version.

end Iterators.Collectors.Sequences;
