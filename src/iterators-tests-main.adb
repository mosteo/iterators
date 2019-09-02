with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;

with Iterators.From.Vectors;

procedure Iterators.Tests.Main is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Int_Iters is new From.Vectors (Int_Vectors);
   use Int_Iters;
   use Int_Iters.Core;

   Seq : Iterator'Class := Just (1) & 2 & 3;

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
      for I of Container'(Seq & Collect) loop
         null;
      end loop;
   end;

   --  Check composition
   declare
      L : constant Container := Just (1) & 2 & 3 & Collect;
   begin
      pragma Assert (L.First_Element = 1 and then L.Last_Element = 3);
   end;

   --  Test readonliness
--     declare
--        It : Iterator'Class := Just (1);
--     begin
--        It.Next.Ref := 2; -- Must fail with a Constraint_Error
--        raise Program_Error with "Should not be reached";
--     exception
--        when Constraint_Error => null;
--     end;

   --  Test iteration over plain list
   declare
      Count : Natural := 0;
      L : Container  := Just (1) & 2 & 3 & Collect;
      I : Iterator'Class     := Iter (L);
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
         I : Iterator'Class := Iter (L);
      begin
         I.Next.Ref := 4;
         pragma Assert (L.First_Element = 4);
      end;
   end;

   --  Constant iteration over plain list
   declare
      Count : Natural := 0;
      L : constant Container := Just (1) & 2 & 3 & Collect;
      I : Iterator'Class     := Const_Iter (L);
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

   --  Variable iteration with Ada "of" notation
   declare
      Seq   : Iterator'Class := Just (1) & 2 & 3;
   begin
      for I of Seq loop
         Put_Line (I'Img);
         I := I + 1;
      end loop;
   end;

   --  Constant iteration with Ada "of" notation
   for I of Iterator'Class'(Just (1) & 2 & 3) loop
      Put_Line (I'Img);
   end loop;

   Put_Line ("OK");

end Iterators.Tests.Main;
