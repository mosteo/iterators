with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Iterators.Traits.Containers;

generic
package Iterators.From.Elements.Lists is

   --  Provides plain lists

   package Ada_Containers is New
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Any_Element);

   subtype Container is Ada_Containers.List;
--     subtype Elements is Ada_Containers.Element_Type;

--     package Container_Traits is new Traits.Containers
--       (Container                   => Container,
--        Element_Type                => Elements,
--        Cursor                      => Ada_Containers.Cursor,
--        First                       => Ada_Containers.First,
--        Next                        => Ada_Containers.Next,
--        Has_Element                 => Ada_Containers.Has_Element,
--        Reference_Type              => Ada_Containers.Reference_Type,
--        Reference                   => Ada_Containers.Reference,
--        Constant_Reference_Type     => Ada_Containers.Constant_Reference_Type,
--        Constant_Reference          => Ada_Containers.Constant_Reference);
--
--     package Appendable_Traits is new Container_Traits.Appendable
--       (Ada_Containers.Append);

end Iterators.From.Elements.Lists;
