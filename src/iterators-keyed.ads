with Iterators.Pairs;
with Iterators.Root.Operators;

generic
   with package Unkeyed is new Root (<>);
   type Keys (<>) is private;
package Iterators.Keyed with Preelaborate is

   --  This package cannot be a child of Root because instantiating a root
   --  from a child is an error (package instantiated within itself).

   package Pairs is new Iterators.Pairs (Unkeyed, Keys);
   --  Provides the new type (Pairs) that forms the keyed sequence.
   --  These pairs are a stored key and a cursor from the unkeyed sequence.

   package Iterators is new Root (Pairs.Pairs);
   --  Provides the actual iterators for Pairs.

   package Operators is new Iterators.Operators;
   package Op renames Operators;
   --  Provides type-preserving keyed operators.

   --  Some types are re-exposed here for the benefit of clients and children:

   subtype Any_Element is Iterators.Elements;
   subtype Iterable is Iterators.Iterable;
   subtype Iterator is Iterators.Iterator;
   subtype Operator is Operators.Operator;

end Iterators.Keyed;
