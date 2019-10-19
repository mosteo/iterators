with Ada.Containers.Vectors;

with Iterators.Collectors.Sequences;
with Iterators.Generators.Keyed;
with Iterators.Keyed;
with Iterators.Root;
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

   package Keyed_Traits is new Container_Traits.Keyed
     (Keys => Ada_Containers.Index_Type,
      Key  => Ada_Containers.To_Index);

   package Iterators is new Standard.Iterators.Root (Elements);
   --  This package provides the regular sources, operators, and sinks.

   package Collectors is new Standard.Iterators.Collectors.Sequences
     (Iterators,
      Container_Traits,
      Appendable_Traits);
   --  Provides collection back into the same container type.

   package Generators is new Standard.Iterators.Generators
     (Iterators,
      Container_Traits);
   --  Provides conversion from container into iterator.

   package Keyed_Iterators is New Standard.Iterators.Keyed
     (Iterators,
      Ada_Containers.Index_Type);
   --  Provides iterators over pairs (key + root element).

   package Keyed_Generators is new Generators.Keyed
     (Container_Traits,
      Keyed_Iterators,
      Keyed_Traits);
   --  Provides conversion from keyed container into keyed iterator.

end Iterators.From.Vectors;
