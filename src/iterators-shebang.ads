with Iterators.From.Elements.Lists;

generic
   type Any_Element (<>) is private;
package Iterators.Shebang with Preelaborate is

   --  As the name implies, when you instance this one you get all related
   --  instantiations, including all container flavors and arrays. Most likely
   --  overkill, bad for compilation times and generally bad for the planet.
   --  Still, the simplest way to start with minimum instantiations and uses.

   package Elems is new From.Elements (Any_Element);

   package Iterators renames Elems.Iterators;
   package Iters     renames Iterators;

   package Operators renames Elems.Operators;
   package Op        renames Operators;

   package Lists is new Elems.Lists;

end Iterators.Shebang;
