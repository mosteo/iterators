with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Ada.Text_IO;

with Iterators.Std;

private with Ada.Strings.Unbounded;

package Iterators.Text_IO is

   --  Read a file as a sequence of lines

   function Lines (File_Name : String)
                   return Std.Strings.Iterator'Class;

   --  But also make available a regular Ada iterator. Note that all that
   --  follows is strictly unnecessary as "of" works for the library iterator;
   --  but for the JSA example we need an actual Ada 2012 plain iterator.

   type Ada_Cursor is limited private;

   function Has_Element (Cursor : Ada_Cursor) return Boolean;

   package Ada_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Ada_Cursor, Has_Element);

   type Ada_Iterator (<>) is new
     Ada.Finalization.Limited_Controlled
     and Ada_Iterator_Interfaces.Forward_Iterator
   with private with
     Constant_Indexing  => Element,
     Default_Iterator   => Iterate,
     Iterator_Element   => String;

   function Get_Lines (File_Name : String) return Ada_Iterator;
   --  Different name to avoid ambiguous iteration domain in the pure Ada case

   function Element (This : aliased Ada_Iterator'Class;
                     Pos  : Ada_Cursor) return String;

   function Iterate (This : aliased Ada_Iterator)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class;

private

   type Ada_Iterator (Length : Natural) is new
     Ada.Finalization.Limited_Controlled
     and Ada_Iterator_Interfaces.Forward_Iterator with
      record
         Name : String (1 .. Length);
         File : Ada.Text_IO.File_Type;
      end record;

   overriding procedure Finalize (This : in out Ada_Iterator);

   overriding function First (This : Ada_Iterator) return Ada_Cursor;

   overriding function Next (This     : Ada_Iterator;
                             Position : Ada_Cursor)
                             return Ada_Cursor;

   --  Using constraints in iterator cursors is full of pitfalls...

   use Ada.Strings.Unbounded;

   type Ada_Cursor is limited record
      EOF  : Boolean;
      Line : Unbounded_String;
   end record;

end Iterators.Text_IO;
