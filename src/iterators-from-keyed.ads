with Iterators.Generators.Keyed;
with Iterators.Keyed;
with Iterators.Traits.Containers.Keyed;

generic
   with package Unkeyed_Generators is new Iterators.Generators (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Unkeyed_Generators.Root.Any_Element,
      others       => <>);
   with package Keyed_Containers is new Containers.Keyed (<>);
package Iterators.From.Keyed with Preelaborate is

   package Iterators is new Standard.Iterators.Keyed
     (Unkeyed => Unkeyed_Generators.Root,
      Keys    => Keyed_Containers.Keys);

   package Generators is new Unkeyed_Generators.Keyed
     (Iterators,
      Containers,
      Keyed_Containers);
   --  PROBLEMATIC INSTANCE

end Iterators.From.Keyed;
