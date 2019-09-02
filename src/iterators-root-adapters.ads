with Iterators.Traits.Containers;

generic
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Any_Element,
      others  => <>);
package Iterators.Root.Adapters with Preelaborate is

   subtype Container is Containers.Container;

   function Const_Iter (C : aliased Container) return Iterator'Class;

   function Iter (C : aliased in out Container) return Iterator'Class;

   function Collect return Container;
   --  Returns an empty container, to have a unique signature for the following
   --  "&" operator.
   --  Trivia: since we need to qualify for "of" to work in a loop (e.g., for
   --  I of A & B loop won't work because of the &), we can safely overload the
   --  Collect subprogram.

   function "&" (L : Iterator'Class;
                 R : Container)
                 return Container;
   --  To be used with Collect

end Iterators.Root.Adapters;
