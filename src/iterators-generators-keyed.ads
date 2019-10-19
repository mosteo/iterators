with Iterators.Keyed;
with Iterators.Traits.Containers.Keyed;

generic
   with package Containers is new Traits.Containers
     (Element_Type => Generators.Root.Any_Element,
      others       => <>);
   with package Keyed_Iterators is new Iterators.Keyed
     (Unkeyed => Generators.Root,
      Keys    => <>);
   with package Keyed_Containers is new Containers.Keyed
     (Keys => Keyed_Iterators.Keys,
      Key  => <>);
package Iterators.Generators.Keyed with Preelaborate is

   subtype Container is Containers.Container;

   subtype Iterator is Keyed_Iterators.Iterator;

   function Const_Iter (C : aliased Container) return Iterator'Class;

   function Iter (C : aliased in out Container) return Iterator'Class;

end Iterators.Generators.Keyed;
