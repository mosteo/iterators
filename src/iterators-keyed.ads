with Iterators.Pairs;
with Iterators.Root;

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

   --  Some types are re-exposed here for the benefit of clients and children:

   subtype Iterator is Iterators.Iterator;

   --  TODO: re-expose Iterators.* here?

end Iterators.Keyed;
