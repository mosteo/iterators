with AAA.Traits.Containers;

with Ada.Containers.Vectors;

with Iterators.Root.Adapters;

generic
   with package Ada_Vectors is new Ada.Containers.Vectors (<>);
package Iterators.From.Vectors with Preelaborate is

   package Core is new Iterators.Root (Ada_Vectors.Element_Type);

   function Ref  (C : aliased in out Ada_Vectors.Vector; Pos : Ada_Vectors.Cursor)
                  return not null access Ada_Vectors.Element_Type is
      (C.Reference (Pos).Element);

   function CRef (C : aliased Ada_Vectors.Vector; Pos : Ada_Vectors.Cursor)
                  return not null access constant Ada_Vectors.Element_Type is
      (C.Constant_Reference (Pos).Element);

   package Container_Traits is new AAA.Traits.Containers
     (Container                   => Ada_Vectors.Vector,
      Element                     => Ada_Vectors.Element_Type,
      Cursor                      => Ada_Vectors.Cursor,
      First                       => Ada_Vectors.First,
      Next                        => Ada_Vectors.Next,
      Has_Element                 => Ada_Vectors.Has_Element,
      Reference                   => Ref,
      Constant_Reference          => CRef);

   package Adapters is new Core.Adapters (Container_Traits);

end Iterators.From.Vectors;
