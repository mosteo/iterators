with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

with Iterators.From.Elements;
with Iterators.From.Ordered_Maps;
with Iterators.From.Vectors;
with Iterators.Operators;

package Iterators.Tests with Preelaborate is

   --  Supporting instantiations:

   package Ints    is new From.Elements (Integer);
   package Strings is new From.Elements (String);
   package Int2str is new Operators (Ints.Iterators, Strings.Iterators);
   package Str2int is new Operators (Strings.Iterators, Ints.Iterators);

   package Int_Maps      is new Ada.Containers.Ordered_Maps (Positive, Integer);
   package Int_Map_Iters is new From.Ordered_Maps (Int_Maps);

   package Int_Vectors   is new Ada.Containers.Vectors (Positive, Integer);
   package Int_Vec_Iters is new From.Vectors (Int_Vectors);

   --  Supporting subprograms:

   function Double (I : Integer) return Integer is (I * 2);

   function Image (I : Integer) return String is (Integer'Image (I));

   function Is_Odd (I : Integer) return Boolean is (I mod 2 = 1);

   function Value (S : String) return Integer is (Integer'Value (S));

end Iterators.Tests;
