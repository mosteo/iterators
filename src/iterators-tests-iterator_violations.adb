--  Situations in which accessibility cannot prevent misuse of iterators.

with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Iterators.From.Vectors;

procedure Iterators.Tests.Iterator_Violations is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Int_Iters is new From.Vectors (Int_Vectors);
   use Int_Iters;
   use Int_Iters.Core;

   --------------
   -- Dangling --
   --------------

   function Dangling return Iterator'Class is
      L : Container;
   begin
      L.Append (1);
      L.Append (2);
      L.Append (3);
      return Iter (L);
   end Dangling;

begin
   --  Will bomb because the original list is out of scope
   for Int of Container'(Dangling & Collect) loop
      Put_Line (Int'Img);
   end loop;

   Put_Line ("OK");
end Iterators.Tests.Iterator_Violations;
