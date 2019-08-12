with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
package Iterators.Root.Sequences is

   -----------
   -- Lists --
   -----------

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Any_Element);

   type List is new Lists.List with null record;

   function Const_Iter (L : aliased List'Class) return Iterator'Class;

   function Iter (L : aliased in out List'Class) return Iterator'Class;

   function Collect return List;
   --  Trivia: since we need to qualify for "of" to work in a loop
   --  (e.g., for I of A & B loop won't work because of the &), we can safely
   --  overload the Collect subprogram.

   function "&" (L : Iterator'Class;
                 R : List)
                 return List;
   --  To be used with Collect

end Iterators.Root.Sequences;
