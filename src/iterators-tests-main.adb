with Ada.Text_IO; use Ada.Text_IO;

procedure Iterators.Tests.Main is

   use type Ada.Containers.Count_Type;

   use Int_Iters;
   use Int_Iters.Core;

   Seq : constant Iterator'Class := Just (1) & 2 & 3;
   Vec : constant Container :=
           Seq
           & Copy
           & Collect;

   --  Element collection
   pragma Assert (Vec.First_Element = 1 and then Vec.Last_Element = 3);

   --  Individual tests

   procedure Manual_Constant_Iteration is
      Count : Natural := 0;
      Seq   : Iterator'Class := Const_Iter (Vec);
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
   end Manual_Constant_Iteration;

   procedure Manual_Variable_Iteration is
      Count : Natural        := 0;
      Vec   : Container      := Main.Vec; -- Use RW copy
      Seq1  : Iterator'Class := Iter (Vec);       -- 1st pass, modifying
      Seq2  : Iterator'Class := Const_Iter (Vec); -- 2nd pass, verifying
   begin
      --  1st pass, modifying
      loop
         declare
            Pos : constant Cursor'Class := Seq1.Next;
         begin
            exit when not Pos.Has_Element;
            Count := Count + 1;
            pragma Assert (Count = Pos.Get);
            Pos.Ref := -Pos.Ref;
            pragma Assert (Count = -Pos.Get);
         end;
      end loop;

      --  2nd pass, verifying
      Count := 0;
      loop
         declare
            Pos : constant Cursor'Class := Seq2.Next;
         begin
            exit when not Pos.Has_Element;
            Count := Count + 1;
            pragma Assert (Count = -Pos.Get);
         end;
      end loop;
   end Manual_Variable_Iteration;

   procedure Constant_Of_Iteration is
      Count : Natural := 0;
   begin
      for I of Const_Iter (Vec) loop
         Count := Count + 1;
         pragma Assert (Count = I);
      end loop;
   end Constant_Of_Iteration;

   procedure Variable_Of_Iteration is
      Count : Natural           := 0;
      Vec   : aliased Container := Main.Vec;
      It    : Iterator'Class    := Iter (Vec);
      --  Unfortunately, for Ada to allow variable iteration, the expression
      --  after "of" must be a variable; so we need an otherwise superfluous
      --  iterator as intermediate variable. This can be avoided by using the
      --  own Iterator modification operators (e.g., Map).
   begin
      for I of It loop
         I := -I;
      end loop;

      for I of Vec loop
         Count := Count + 1;
         pragma Assert (Count = -I);
      end loop;

      --  Alternatively, using the read-write cursor returned by Iter allows
      --  skipping the intermediate Iterator variable, although it's uglier:
      for C in Iter (Vec).Iterate loop
         C.Ref := -C.Ref;
      end loop;
   end Variable_Of_Iteration;

   procedure Op_Copy is
      Seq : constant Iterator'Class := Main.Seq;
   begin
      pragma Assert (Container'(Seq & Copy & Collect).Length = 3);
      --  Does not consume Seq because of Copy.

      pragma Assert (Container'(Seq & Collect).Length = 3);
      --  Consumes Seq.

      pragma Assert (Container'(Seq & Collect).Length = 0);
      --  Because Seq is already consumed.
   end Op_Copy;

   procedure Op_Filter is
      function Is_Even (I : Integer) return Boolean is (I mod 2 = 0);
   begin
      pragma Assert
        (Container'
           (Const_Iter (Vec)
            & Filter (Is_Even'Access)
            & Collect)
         .Length = 1);
   end Op_Filter;

begin
   Manual_Constant_Iteration;
   Manual_Variable_Iteration;

   Constant_Of_Iteration;
   Variable_Of_Iteration;

   Op_Copy;
   Op_Filter;

   Put_Line ("OK");

end Iterators.Tests.Main;
