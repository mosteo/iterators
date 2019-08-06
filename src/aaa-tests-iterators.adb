with AAA.Iterators.Sequences;

with Ada.Text_IO; use Ada.Text_IO;

procedure AAA.Tests.Iterators is

   package Int_Iterators is new AAA.Iterators (Integer); use Int_Iterators;
   package Int_Sequences is new Int_Iterators.Sequences; use Int_Sequences;

   Seq : Iterator'Class := Just (1) & 2 & 3;

begin
   --  Check manual iteration
   declare
      Count : Natural := 0;
   begin
      loop
         declare
            Ref : constant Reference := Seq.Next;
         begin
            exit when Ref.Element = null;
            Count := Count + 1;
            pragma Assert (Count = Ref);
         end;
      end loop;
   end;

   --  Check composition
   declare
      L : constant Int_Sequences.List :=
            Just (1) & 2 & 3
            & Collect;
   begin
      pragma Assert (L.First_Element = 1 and then L.Last_Element = 3);
   end;

   --  Test "of" with collected list
   for I of List'(Seq & Collect) loop
      null;
   end loop;

   Put_Line ("OK");

end AAA.Tests.Iterators;
