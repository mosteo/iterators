--  Situations in which accessibility cannot prevent misuse of iterators.

with Ada.Text_IO; use Ada.Text_IO;

procedure Iterators.Tests.Violations is

   package Ints renames Int_Vec_Iters;
   use Ints.Linkers;

   --------------
   -- Dangling --
   --------------

   function Dangling return Ints.Iterator'Class is
      L : Ints.Container;
   begin
      L.Append (1);
      L.Append (2);
      L.Append (3);
      return Ints.Gen.Iter (L);
   end Dangling;

begin
   --  Will bomb because the original list is out of scope
   for Int of Ints.Container'(Dangling & Ints.Col.Collect) loop
      Put_Line (Int'Img);
   end loop;

   Put_Line ("OK");
end Iterators.Tests.Violations;
