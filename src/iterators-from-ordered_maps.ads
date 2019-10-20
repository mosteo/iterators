with Ada.Containers.Ordered_Maps;

with Iterators.Collectors.Mappings;
with Iterators.Generators.Keyed;
with Iterators.Keyed;
with Iterators.Linkers.Mappings;
with Iterators.Root;
with Iterators.Traits.Containers.Keyed;

generic
   with package Ada_Containers is new Ada.Containers.Ordered_Maps (<>);
package Iterators.From.Ordered_Maps with Preelaborate is

   subtype Container is Ada_Containers.Map;
   subtype Elements  is Ada_Containers.Element_Type;

   package Container_Traits is new Traits.Containers
     (Container                   => Container,
      Element_Type                => Ada_Containers.Element_Type,
      Cursor                      => Ada_Containers.Cursor,
      First                       => Ada_Containers.First,
      Next                        => Ada_Containers.Next,
      Has_Element                 => Ada_Containers.Has_Element,
      Reference_Type              => Ada_Containers.Reference_Type,
      Reference                   => Ada_Containers.Reference,
      Constant_Reference_Type     => Ada_Containers.Constant_Reference_Type,
      Constant_Reference          => Ada_Containers.Constant_Reference);

   package Keyed_Traits is new Container_Traits.Keyed
     (Keys    => Ada_Containers.Key_Type,
      Key     => Ada_Containers.Key,
      Insert  => Ada_Containers.Insert,
      Include => Ada_Containers.Include);

   package Iterators is new Standard.Iterators.Root (Elements);
   --  This package provides the regular sources, operators, and sinks.

   subtype Iterator is Iterators.Iterator;
   subtype Cursor   is Iterators.Cursor;

   package Keyed_Iterators is new Standard.Iterators.Keyed
     (Iterators,
      Keys => Ada_Containers.Key_Type);
   --  Provides iterators over pairs key+value.

   subtype Keyed_Iterator is Keyed_Iterators.Iterator;

   package Keyed_Collectors is new Standard.Iterators.Collectors.Mappings
     (Keyed_Iterators,
      Container_Traits,
      Keyed_Traits);
   --  Provides collection back into the same container type.

   package Generators is new Standard.Iterators.Generators
     (Iterators,
      Container_Traits);
   --  Provides conversion from container into iterator.

   package Keyed_Generators is new Generators.Keyed
     (Container_Traits,
      Keyed_Iterators,
      Keyed_Traits);
   --  Provides conversion from keyed container into keyed iterator.

   package Linkers is new Standard.Iterators.Linkers.Mappings
     (Keyed_Iterators,
      Keyed_Collectors);

end Iterators.From.Ordered_Maps;
