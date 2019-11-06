generic
   type Any_Element is private;
   type Any_Index is (<>);
package Iterators.Generators.Arrays is

   --  To avoid duplication of generic profile, and allow use with anonymous
   --  arrays, we require users to cast their arrays to one of the named type
   --  herein:

   type Element_Array is array (Any_Index range <>) of Any_Element;

end Iterators.Generators.Arrays;
