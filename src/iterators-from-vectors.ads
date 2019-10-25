with Ada.Containers.Vectors;

with Iterators.From.Keyed;
with Iterators.Generators;
with Iterators.Root;
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

   package Keyed_Traits is new Container_Traits.Keyed
     (Keys    => Ada_Containers.Index_Type,
      Key     => Ada_Containers.To_Index);

   package Iterators is new Standard.Iterators.Root (Elements);

   package Generators is new Standard.Iterators.Generators
     (Iterators,
      Container_Traits);

   package Keyed is new From.Keyed
     (Unkeyed_Generators => Generators,
      Containers         => Container_Traits,
      Keyed_Containers   => Keyed_Traits);

end Iterators.From.Vectors;
