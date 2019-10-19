--  Situations in which accessibility cannot prevent misuse of iterators.

with Ada.Text_IO; use Ada.Text_IO;

procedure Iterators.Tests.Violations is

   use Int_Iters.Iterators;
   use Int_Iters.Collectors;
   use Int_Iters.Generators;

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
end Iterators.Tests.Violations;
