with Ada.Containers;

generic
   pragma Warnings (Off); -- For the unreferenced entities
   type Container is private;
   type Element_Type (<>) is private;

   with procedure Append (C : in out Container; E : Element_Type; Count : Ada.Containers.Count_Type);

   type Cursor is private;
   with function First (C : Container) return Cursor;
   with function Next  (Pos : Cursor) return Cursor;
   with function Has_Element (Pos : Cursor) return Boolean;

   type Reference_Type (Element : not null access Element_Type) is limited private;
   with function Reference (Col : aliased in out Container; Pos : Cursor) return Reference_Type;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is limited private;
   with function Constant_Reference (Col : aliased Container; Pos : Cursor) return Constant_Reference_Type;
   pragma Warnings (On);
package Iterators.Traits.Containers with Preelaborate is

   --  Reuse the gist of standard containers

end Iterators.Traits.Containers;
