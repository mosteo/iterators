with Iterators.Root;
with Iterators.Traits.Containers.Appendable;

generic
   with package Root is new Iterators.Root (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Root.Any_Element, others => <>);
   with package Appendable is new Containers.Appendable (<>);
package Iterators.Collectors.Sequences with Preelaborate is

   subtype Container is Containers.Container;
   subtype Iterator is Root.Iterator;

   --  Collect into a container that respects the appending order.

   function Collect return Container;
   --  Returns an empty container, to have a unique signature for the previous
   --  "&" operator.
   --  Trivia: since we need to qualify for "of" to work in a loop (e.g., for
   --  I of A & B loop won't work because of the &), we can safely overload the
   --  Collect subprogram.

   function Collect (L : Iterator'Class;
                     R : Container)
                     return Container;
   --  To be used as "&".
   --  Alternatively, if R is not empty, L and then R will be collected.

   function Collect (It : Iterator'Class) return Container;
   --  Alternate regular function version.

   package Linking is

      function "&" (L : Iterator'Class;
                    R : Container)
                    return Container renames Collect;

   end Linking;

end Iterators.Collectors.Sequences;
