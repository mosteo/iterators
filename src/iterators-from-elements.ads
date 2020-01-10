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

   package Linking renames Operators.Linking;

   --  Re-expose common user types:
   subtype Iterator is Iterators.Iterator;
   subtype Operator is Operators.Operator;
   subtype Sequence is Operators.Sequence;

--     package Collectors renames List_Iterators.Collectors;
--     package Col renames Collectors;
--
--     package Generators renames List_Iterators.Generators;
--     package Gen renames Generators;

end Iterators.From.Elements;
