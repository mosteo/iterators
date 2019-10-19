with Ada.Containers.Vectors;

with Iterators.Keyed.Adapters;
with Iterators.Root.Adapters;
with Iterators.Traits.Containers.Keyed;

generic
   with package Ada_Containers is new Ada.Containers.Vectors (<>);
package Iterators.From.Vectors with Preelaborate is

   subtype Container is Ada_Containers.Vector;
   subtype Elements  is Ada_Containers.Element_Type;

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

   package Keyed_Traits is new Container_Traits.Keyed
     (Keys => Ada_Containers.Index_Type,
      Key  => Ada_Containers.To_Index);

   package Iterators is new Standard.Iterators.Root (Elements);
   --  This package provides the regular sources, operators, and sinks.

   package Containers is new Iterators.Adapters (Container_Traits);
   --  This package provides the container adapters

   package Keyed_Iterators is New Standard.Iterators.Keyed
     (Iterators,
      Ada_Containers.Index_Type);
   --  Provides iterators over pairs (key + root element)

   package Keyed_Containers is new Keyed_Iterators.Adapters
     (Container_Traits,
      Keyed_Traits);
   --  Provides the keyed container adapters: make a keyed sequence from an Ada
   --  standard container

end Iterators.From.Vectors;
