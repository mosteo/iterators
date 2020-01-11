with Iterators.Generators.Containers.Keyed;
with Iterators.Keyed;
with Iterators.Traits.Containers.Keyed;

generic
   with package Unkeyed_Generators is new Iterators.Generators.Containers (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Unkeyed_Generators.Root.Any_Element,
      others       => <>);
   with package Keyed_Containers is new Containers.Keyed (<>);
package Iterators.From.Keyed with Preelaborate is

   --  Grouped instantiations for Keyed Iterators/Generators/Collectors etc.

   package Iterators is new Standard.Iterators.Keyed
     (Unkeyed => Unkeyed_Generators.Root,
      Keys    => Keyed_Containers.Keys);
   --  Provides iterators over pairs key+value.

   subtype Iterator is Iterators.Iterator;

   package Generators is new Unkeyed_Generators.Keyed
     (Iterators,
      Containers,
      Keyed_Containers);
   package Gen renames Generators;
   --  Provides conversion from keyed container into keyed iterator.

   package Operators renames Iterators.Operators;
   package Op renames Operators;
   --  Provides type-preserving keyed operators.

end Iterators.From.Keyed;
