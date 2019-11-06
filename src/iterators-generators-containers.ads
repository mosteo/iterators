with Iterators.Root;
with Iterators.Traits.Containers;

generic
   with package Root is new Standard.Iterators.Root (<>);
   with package Container_Traits is new Iterators.Traits.Containers
     (Element_Type => Root.Any_Element,
      others       => <>);
package Iterators.Generators.Containers with Preelaborate is

   subtype Container is Container_Traits.Container;

   function Const_Iter (C : aliased Container) return Root.Iterator'Class;

   function Iter (C : aliased in out Container) return Root.Iterator'Class;

end Iterators.Generators.Containers;
