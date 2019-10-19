with Iterators.Traits.Containers.Keyed;

generic
   with package Containers is new Traits.Containers
     (Element_Type => Keyed.Unkeyed.Any_Element,
      others       => <>);
   with package Keyed_Containers is new Containers.Keyed
     (Keys => Keys,
      Key  => <>);
package Iterators.Keyed.Adapters with Preelaborate is

   subtype Container is Containers.Container;

   function Const_Iter (C : aliased Container) return Iterator'Class;

--     function Iter (C : aliased in out Container) return Iterator'Class;
--
--     function "&" (L : Iterator'Class;
--                   R : Container)
--                      return Container;
--     --  To be used with Collect.
--     --  Alternatively, if R is not empty, items out of L will be appended.
--
--     function Collect return Container;
--     --  Returns an empty container, to have a unique signature for the previous
--     --  "&" operator.
--     --  Trivia: since we need to qualify for "of" to work in a loop (e.g., for
--     --  I of A & B loop won't work because of the &), we can safely overload the
--     --  Collect subprogram.
--
--     function Collect (It : Iterator'Class) return Container;
--     --  Alternate regular function version.

end Iterators.Keyed.Adapters;
