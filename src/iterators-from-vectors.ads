with Ada.Containers.Vectors;

with Iterators.Root.Adapters;
with Iterators.Traits.Containers;

generic
   with package Ada_Containers is new Ada.Containers.Vectors (<>);
package Iterators.From.Vectors with Preelaborate is

   subtype Container is Ada_Containers.Vector;

   package Core is new Iterators.Root (Ada_Containers.Element_Type);

   package Container_Traits is new Traits.Containers
     (Container                   => Container,
      Element_Type                => Ada_Containers.Element_Type,
      Append                      => Ada_Containers.Append,
      Cursor                      => Ada_Containers.Cursor,
      First                       => Ada_Containers.First,
      Next                        => Ada_Containers.Next,
      Has_Element                 => Ada_Containers.Has_Element,
      Reference_Type              => Ada_Containers.Reference_Type,
      Reference                   => Ada_Containers.Reference,
      Constant_Reference_Type     => Ada_Containers.Constant_Reference_Type,
      Constant_Reference          => Ada_Containers.Constant_Reference);

   package Adapters is new Core.Adapters (Container_Traits);

   function Const_Iter (C : aliased Container) return Core.Iterator'Class
                        renames Adapters.Const_Iter;

   function Iter (C : aliased in out Container) return Core.Iterator'Class
                  renames Adapters.Iter;

   function Collect return Container renames Adapters.Collect;

   function "&" (L : Core.Iterator'Class;
                 R : Container)
                 return Container renames Adapters."&";

end Iterators.From.Vectors;
