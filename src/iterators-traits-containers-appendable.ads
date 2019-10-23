with Ada.Containers;

generic
   with procedure Append (C     : in out Container;
                          E     : Element_Type;
                          Count : Ada.Containers.Count_Type) with Warnings => Off;
package Iterators.Traits.Containers.Appendable with Preelaborate is

   --  For containers that support the Append operation:
   --  Doubly_Linked_Lists
   --  Vectors

   procedure Copy (Dst : in out Container;
                   Src :        Container);

end Iterators.Traits.Containers.Appendable;
