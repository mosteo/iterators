with Iterators.From.Elements;
with Iterators.Generators.Arrays;
with Iterators.Operators;
with Iterators.Std;

package Iterators.Demo with Preelaborate is

   package Float_Iters is new From.Elements (Float);

   package Float2str is new Operators (Float_Iters.Iterators,
                                       Std.Strings.Iterators);
   package Str2float is new Operators (Std.Strings.Iterators,
                                       Float_Iters.Iterators);

   package Float_Array_Iters is new Iterators.Generators.Arrays
     (Any_Index   => Positive,
      Any_Element => Float,
      Root        => Float_Iters.Iterators);


   function "+" (L, R : Float) return Float is (Standard."+" (L, R));

   function Image (F : Float) return String is (Float'Image (F));
   function Value (S : String) return Float is (Float'Value (S));

   function Is_Positive (F : Float) return Boolean is (F >= 0.0);

end Iterators.Demo;
