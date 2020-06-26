with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Iterators.Collectors.Sequences;
with Iterators.Generators.Containers;
with Iterators.Traits.Containers.Appendable;

generic
package Iterators.From.Elements.Lists with Preelaborate is

   package Ada_Containers is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Any_Element);

   subtype Container is Ada_Containers.List;

   --  Bug work-arounding:

   type Reference_Type
     (Element : not null access Any_Element) is
     new Ada_Containers.Reference_Type (Element => Element);

   function Reference
     (C : aliased in out Container;
      P : Ada_Containers.Cursor) return Reference_Type is
     (Reference_Type
        (Ada_Containers.Reference_Type'
             (Ada_Containers.Reference (C, P))));

   type Constant_Reference_Type
     (Element : not null access constant Any_Element) Is
   new Ada_Containers.Constant_Reference_Type (Element => Element);

   function Constant_Reference
     (C : aliased Container;
      P : Ada_Containers.Cursor) return Constant_Reference_Type is
     (Constant_Reference_Type
        (Ada_Containers.Constant_Reference_Type'
             (Ada_Containers.Constant_Reference (C, P))));

   --  Resume normal operation

   package Container_Traits is new Traits.Containers
     (Container                   => Container,
      Element_Type                => Any_Element,
      Cursor                      => Ada_Containers.Cursor,
      First                       => Ada_Containers.First,
      Next                        => Ada_Containers.Next,
      Has_Element                 => Ada_Containers.Has_Element,
      Reference_Type              => Reference_Type,
      Reference                   => Reference,
      Constant_Reference_Type     => Constant_Reference_Type,
      Constant_Reference          => Constant_Reference);

   package Appendable_Traits is new Container_Traits.Appendable
     (Ada_Containers.Append);

   package Collectors is new Standard.Iterators.Collectors.Sequences
     (Iterators,
      Container_Traits,
      Appendable_Traits);
   package Col renames Collectors;

   package Generators is new Standard.Iterators.Generators.Containers
     (Iterators,
      Container_Traits);
   package Gen renames Generators;

   package Linking renames Collectors.Linking;

end Iterators.From.Elements.Lists;
