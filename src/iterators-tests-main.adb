with Iterators.Root.Sequences;

with Ada.Text_IO; use Ada.Text_IO;

procedure Iterators.Tests.Main is

   package Int_Iterators is new Iterators.Root (Integer); use Int_Iterators;
   package Int_Sequences is new Int_Iterators.Sequences; use Int_Sequences;

begin
   --  Check manual iteration
   declare
      Count : Natural := 0;
      Seq   : Iterator'Class := Just (1) & 2 & 3;
   begin
      loop
         declare
            Pos : constant Cursor'Class := Seq.Next;
         begin
            exit when not Pos.Has_Element;
            Count := Count + 1;
            pragma Assert (Count = Pos.Get);
         end;
      end loop;

      --  Test "of" with collected list
      for I of List'(Seq & Collect) loop
         null;
      end loop;
   end;

   --  Check composition
   declare
      L : constant Int_Sequences.List := Just (1) & 2 & 3 & Collect;
   begin
      pragma Assert (L.First_Element = 1 and then L.Last_Element = 3);
   end;

   --  Test readonliness
   declare
      It : Iterator'Class := Just (1);
   begin
      It.Next.Ref := 2; -- Must fail with a Constraint_Error
      raise Program_Error with "Should not be reached";
   exception
      when Constraint_Error => null;
   end;

   --  Test iteration over plain list
   declare
      Count : Natural := 0;
      L : Int_Sequences.List := Just (1) & 2 & 3 & Collect;
      I : Iterator'Class     := L.Iter;
   begin
      loop
         declare
            Pos : constant Cursor'Class := I.Next;
         begin
            exit when Pos.Is_Empty;
            Count := Count + 1;
            pragma Assert (Count = Pos.Get);
         end;
      end loop;

      --  Test modification through iterators:
      declare
         I : Iterator'Class := L.Iter;
      begin
         I.Next.Ref := 4;
         pragma Assert (L.First_Element = 4);
      end;
   end;

   --  Constant iteration over plain list
   declare
      Count : Natural := 0;
      L : constant Int_Sequences.List := Just (1) & 2 & 3 & Collect;
      I : Iterator'Class     := L.Const_Iter;
   begin
      loop
         declare
            Pos : constant Cursor'Class := I.Next;
         begin
            exit when Pos.Is_Empty;
            Count := Count + 1;
            pragma Assert (Count = Pos.Get);
         end;
      end loop;
   end;

   Put_Line ("OK");

end Iterators.Tests.Main;
