with Ada.Containers.Ordered_Maps;

with Iterators.Collectors.Mappings;
with Iterators.From.Keyed;
with Iterators.Generators.Containers;
with Iterators.Linkers.Mappings;
with Iterators.Root.Operators;
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

   package Generators is new Standard.Iterators.Generators.Containers
     (Iterators,
      Container_Traits);
   package Gen renames Generators;
   --  Provides conversion from container into iterator.

   package Keyed is new From.Keyed
     (Unkeyed_Generators => Generators,
      Containers         => Container_Traits,
      Keyed_Containers   => Keyed_Traits);
   --  Provides the Keyed alternatives: generators, operators, collectors.

   package Collectors is new Standard.Iterators.Collectors.Mappings
     (Keyed.Iterators,
      Container_Traits,
      Keyed_Traits);
   package Col renames Collectors;
   --  Provides collection back into the same container type.

   package Operators is new Iterators.Operators;
   package Op renames Operators; -- shortcut
   --  Provides type-preserving operators.

   package Linkers is new Standard.Iterators.Linkers.Mappings
     (Iterators,
      Keyed.Iterators,
      Operators,
      Collectors);

end Iterators.From.Ordered_Maps;
