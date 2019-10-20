with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

pragma Warnings (Off);
with Iterators.From.Lists; -- force compilation
pragma Warnings (On);

with Iterators.From.Ordered_Maps;
with Iterators.From.Vectors;

package Iterators.Tests with Preelaborate is

   package Int_Maps is new Ada.Containers.Ordered_Maps (Positive, Integer);
   package Int_Map_Iters is new From.Ordered_Maps (Int_Maps);

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Int_Vec_Iters is new From.Vectors (Int_Vectors);

end Iterators.Tests;
