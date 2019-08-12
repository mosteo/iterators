with AAA.Traits.Containers;

generic
   with package Containers is new AAA.Traits.Containers (<>);
package Iterators.Root.Adapters with Preelaborate is

   subtype Container is Containers.Container;

   function Const_Iter (C : aliased Container) return Iterator'Class;

   function Iter (C : aliased in out Container) return Iterator'Class;

end Iterators.Root.Adapters;
