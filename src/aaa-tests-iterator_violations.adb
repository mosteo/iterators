--  Situations in which accessibility cannot prevent misuse of iterators.

with AAA.Iterators.Sequences;

with Ada.Text_IO; use Ada.Text_IO;

procedure AAA.Tests.Iterator_Violations is

   package Int_Iterators is new AAA.Iterators (Integer); use Int_Iterators;
   package Int_Sequences is new Int_Iterators.Sequences; use Int_Sequences;

   --------------
   -- Dangling --
   --------------

   function Dangling return Iterator'Class is
      L : List;
   begin
      L.Append (1);
      L.Append (2);
      L.Append (3);
      return L.Iter;
   end Dangling;

begin
   --  Will bomb because the original list is out of scope
   for Int of List'(Dangling & Collect) loop
      Put_Line (Int'Img);
   end loop;

   Put_Line ("OK");
end AAA.Tests.Iterator_Violations;
