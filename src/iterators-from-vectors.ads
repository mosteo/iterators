with Ada.Containers.Vectors;

with Iterators.Collectors.Sequences;
with Iterators.From.Keyed;
with Iterators.Generators.Containers;
with Iterators.Linkers.Sequences;
with Iterators.Root.Operators;
with Iterators.Traits.Containers.Appendable;
with Iterators.Traits.Containers.Keyed;

generic
   with package Ada_Containers is new Ada.Containers.Vectors (<>);
package Iterators.From.Vectors with Preelaborate is

   subtype Container is Ada_Containers.Vector;
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

   procedure Cant_Insert_Or_Include (C : in out Container;
                                     I : Ada_Containers.Index_Type;
                                     E : Ada_Containers.Element_Type);
   --  Vectors do not support reassembly using indexes because there is no safe
   --  way to distinguish an insertion of an inclusion. To avoid data loss, for
   --  that kind of use case a Map is recommended. This operation always raises.

   package Keyed_Traits is new Container_Traits.Keyed
     (Keys    => Ada_Containers.Index_Type,
      Key     => Ada_Containers.To_Index,
      Insert  => Cant_Insert_Or_Include,
      Include => Cant_Insert_Or_Include);

   package Iterators is new Standard.Iterators.Root (Elements);
   --  This package provides the regular sources, operators, and sinks.

   subtype Iterator is Iterators.Iterator;
   subtype Cursor   is Iterators.Cursor;

   package Collectors is new Standard.Iterators.Collectors.Sequences
     (Iterators,
      Container_Traits,
      Appendable_Traits);
   --  Provides collection back into the same container type.
   package Col renames Collectors;

   package Generators is new Standard.Iterators.Generators.Containers
     (Iterators,
      Container_Traits);
   --  Provides conversion from container into iterator.
   package Gen renames Generators;

   package Keyed is new From.Keyed
     (Unkeyed_Generators => Generators,
      Containers         => Container_Traits,
      Keyed_Containers   => Keyed_Traits);
   --  Provides the Keyed alternatives: generators, operators, collectors.

   package Operators is new Iterators.Operators;
   package Op renames Operators; -- shortcut
   --  Provides type-preserving operators.

   package Linkers is new Standard.Iterators.Linkers.Sequences
     (Iterators,
      Operators,
      Collectors);

end Iterators.From.Vectors;
