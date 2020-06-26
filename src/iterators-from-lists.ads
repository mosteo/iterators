with Ada.Containers.Doubly_Linked_Lists;

with Iterators.Collectors.Sequences;
with Iterators.Generators.Containers;
with Iterators.Linkers.Sequences;
with Iterators.Root.Operators;
with Iterators.Traits.Containers.Appendable;

generic
   with package Ada_Containers is new Ada.Containers.Doubly_Linked_Lists (<>);
package Iterators.From.Lists with Preelaborate is

   subtype Container is Ada_Containers.List;
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

   package Appendable_Traits is new Container_Traits.Appendable
     (Ada_Containers.Append);

   package Iterators is new Standard.Iterators.Root (Elements);
   --  This package provides the regular sources, operators, and sinks.

   subtype Iterator is Iterators.Iterator;
   subtype Cursor   is Iterators.Cursor;

   package Collectors is new Standard.Iterators.Collectors.Sequences
     (Iterators,
      Container_Traits,
      Appendable_Traits);
   package Col renames Collectors;
   --  Provides collection back into the same container type.

   package Generators is new Standard.Iterators.Generators.Containers
     (Iterators,
      Container_Traits);
   package Gen renames Generators;
   --  Provides conversion from container into iterator.

   package Operators is new Iterators.Operators;
   package Op renames Operators; -- shortcut
   --  Provides type-preserving operators.

   package Linkers is new Standard.Iterators.Linkers.Sequences
     (Iterators,
      Operators,
      Collectors);

end Iterators.From.Lists;
