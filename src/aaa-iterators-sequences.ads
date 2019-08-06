with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
package AAA.Iterators.Sequences is

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Any_Element);

   subtype List is Lists.List;

   function Collect return List;
   --  Trivia: since we need to qualify for "of" to work in a loop
   --  (e.g., for I of A & B loop won't work because of the &), we can safely
   --  overload the Collect subprogram.

   function "&" (L : Iterator'Class;
                 R : List)
                 return List;

end AAA.Iterators.Sequences;
