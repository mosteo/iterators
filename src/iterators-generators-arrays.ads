with Iterators.Root;

generic
   type Any_Index is (<>);
   type Any_Element is private;
   with package Root is new Iterators.Root (Any_Element => Any_Element);
package Iterators.Generators.Arrays with Preelaborate is

   --  To avoid duplication of generic profile, and allow use with anonymous
   --  arrays, we require users to cast their arrays to one of the named type
   --  herein:

   type Element_Array is array (Any_Index range <>) of aliased Any_Element;

   function Const_Iter (C : aliased Element_Array) return Root.Iterator'Class;

   function Iter (C : aliased in out Element_Array) return Root.Iterator'Class;

end Iterators.Generators.Arrays;
