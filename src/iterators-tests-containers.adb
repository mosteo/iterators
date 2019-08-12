with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Iterators.From.Vectors;

procedure Iterators.Tests.Containers is

   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Intvecits is new From.Vectors (Int_Vectors);

   V : Int_Vectors.Vector;

begin
   V.Append (1);
   V.Append (2);
   V.Append (3);

   --  Constant manual iteration
   declare
      It : Intvecits.Core.Iterator'Class := Intvecits.Adapters.Const_Iter (V);
      Count : Positive := 1;
   begin
      loop
         declare
            Pos : constant Intvecits.Core.Cursor'Class := It.Next;
         begin
            exit when Pos.Is_Empty;
            pragma Assert (Pos.Get = Count);
            Count := Count + 1;
         end;
      end loop;
   end;

   --  Variable iteration
   declare
      It : Intvecits.Core.Iterator'Class := Intvecits.Adapters.Iter (V);
      Count : Positive := 1;
   begin
      loop
         declare
            Pos : constant Intvecits.Core.Cursor'Class := It.Next;
         begin
            exit when Pos.Is_Empty;
            pragma Assert (Pos.Ref = Count);
            Pos.Ref := Pos.Ref + 1;
            Count := Count + 1;
            pragma Assert (Pos.Ref = Count);
         end;
      end loop;
   end;

   Put_Line ("OK");
end Iterators.Tests.Containers;
