with AAA.Traits.Containers;

with Ada.Containers.Vectors;

with Iterators.Root.Adapters;

generic
   with package Ada_Containers is new Ada.Containers.Vectors (<>);
package Iterators.From.Vectors with Preelaborate is

   subtype Container is Ada_Containers.Vector;

   package Core is new Iterators.Root (Ada_Containers.Element_Type);

   function Const_Iter (C : aliased Container) return Core.Iterator'Class;

   function Iter (C : aliased in out Container) return Core.Iterator'Class;

private

   function Ref  (C   : aliased in out Container;
                  Pos : Ada_Containers.Cursor)
                  return not null access Ada_Containers.Element_Type is
      (C.Reference (Pos).Element);

   function CRef (C   : aliased Container;
                  Pos : Ada_Containers.Cursor)
                  return not null access constant Ada_Containers.Element_Type is
      (C.Constant_Reference (Pos).Element);

   package Container_Traits is new AAA.Traits.Containers
     (Container                   => Container,
      Element                     => Ada_Containers.Element_Type,
      Cursor                      => Ada_Containers.Cursor,
      First                       => Ada_Containers.First,
      Next                        => Ada_Containers.Next,
      Has_Element                 => Ada_Containers.Has_Element,
      Reference                   => Ref,
      Constant_Reference          => CRef);

   package Adapters is new Core.Adapters (Container_Traits);

   function Const_Iter (C : aliased Container) return Core.Iterator'Class
     renames Adapters.Const_Iter;

   function Iter (C : aliased in out Container) return Core.Iterator'Class
     renames Adapters.Iter;

end Iterators.From.Vectors;
