with Ada.Containers.Vectors;

with Iterators.From.Vectors;

package Iterators.Tests with Preelaborate is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Int_Iters is new From.Vectors (Int_Vectors);

end Iterators.Tests;
