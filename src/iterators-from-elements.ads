with Iterators.Root.Operators;

generic
   type Any_Element (<>) is private;
package Iterators.From.Elements with Preelaborate is

   --  When instantiating from an element type, we get full control of further
   --  instances and we can also mix container types.

   package Iterators is new Standard.Iterators.Root (Any_Element);
   package Iters renames Iterators;

   package Operators is new Iterators.Operators;
   package Op renames Operators;

--     package Collectors renames List_Iterators.Collectors;
--     package Col renames Collectors;
--
--     package Generators renames List_Iterators.Generators;
--     package Gen renames Generators;
--
--     package Linkers renames List_Iterators.Linkers;

end Iterators.From.Elements;
