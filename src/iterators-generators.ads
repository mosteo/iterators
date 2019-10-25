with Iterators.Root;
with Iterators.Traits.Containers;

generic
   with package Root is new Standard.Iterators.Root (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Root.Any_Element,
      others       => <>);
package Iterators.Generators with Preelaborate is

end Iterators.Generators;
