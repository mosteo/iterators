generic
   type Container is tagged private;
   type Element_Type (<>) is private;

   type Cursor is private;
   with function First (C : Container) return Cursor;
   with function Next  (Pos : Cursor) return Cursor;
   with function Has_Element (Pos : Cursor) return Boolean;

   type Reference_Type (Element : not null access Element_Type) is limited private;
   with function Reference (Col : aliased in out Container; Pos : Cursor) return Reference_Type;

   type Constant_Reference_Type (Element : not null access constant Element_Type) is limited private;
   with function Constant_Reference (Col : aliased Container; Pos : Cursor) return Constant_Reference_Type;
package Iterators.Traits.Containers with Preelaborate is

   subtype Cursor_Bug is Cursor;
   --  Sometimes (not in this bugcase), using this renaming instead of the
   --  original in dependent packages works.

end Iterators.Traits.Containers;
