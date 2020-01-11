procedure Iterators.Tests.Vectors is

   use type Ada.Containers.Count_Type;

   --  Regular sequences
   package Ints renames Int_Vec_Iters;
   use Ints.Linkers;

   Seq : constant Ints.Iterator'Class := Ints.Op.Just (1) & 2 & 3;
   Vec : constant Ints.Container :=
           Seq
           & Ints.Col.Collect;

   --  Element collection
   pragma Assert (Vec.First_Element = 1 and then Vec.Last_Element = 3);

   --  Individual tests

   -------------------------------
   -- Manual_Constant_Iteration --
   -------------------------------

   procedure Manual_Constant_Iteration is
      Count : Natural := 0;
      Seq   : Ints.Iterator'Class := Ints.Generators.Const_Iter (Vec);
   begin
      loop
         declare
            Pos : constant Ints.Cursor'Class := Seq.Next;
         begin
            exit when not Pos.Has_Element;
            Count := Count + 1;
            pragma Assert (Count = Pos.Get);
         end;
      end loop;
   end Manual_Constant_Iteration;

   -------------------------------
   -- Manual_Variable_Iteration --
   -------------------------------

   procedure Manual_Variable_Iteration is
      Count : Natural             := 0;
      Vec   : Ints.Container      := Vectors.Vec; -- Use RW copy
      Seq1  : Ints.Iterator'Class := Ints.Generators.Iter (Vec);       -- 1st pass, modifying
      Seq2  : Ints.Iterator'Class := Ints.Generators.Const_Iter (Vec); -- 2nd pass, verifying
   begin
      --  1st pass, modifying
      loop
         declare
            Pos : constant Ints.Cursor'Class := Seq1.Next;
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
            Pos : constant Ints.Cursor'Class := Seq2.Next;
         begin
            exit when not Pos.Has_Element;
            Count := Count + 1;
            pragma Assert (Count = -Pos.Get);
         end;
      end loop;
   end Manual_Variable_Iteration;

   ---------------------------
   -- Constant_Of_Iteration --
   ---------------------------

   procedure Constant_Of_Iteration is
      Count : Natural := 0;
   begin
      for I of Ints.Generators.Const_Iter (Vec) loop
         Count := Count + 1;
         pragma Assert (Count = I);
      end loop;
   end Constant_Of_Iteration;

   ---------------------------
   -- Variable_Of_Iteration --
   ---------------------------

   procedure Variable_Of_Iteration is
      Count : Natural                := 0;
      Vec   : aliased Ints.Container := Vectors.Vec;
      It    : Ints.Iterator'Class    := Ints.Generators.Iter (Vec); -- PAPER
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
      for C in Ints.Generators.Iter (Vec).Iterate loop
         C.Ref := -C.Ref;
      end loop;
   end Variable_Of_Iteration;

   -------------
   -- Op_Copy --
   -------------

   procedure Op_Copy is
   begin
      pragma Assert (Ints.Container'(
                     Seq
                     & Ints.Op.Copy
                     & Ints.Col.Collect).Length = 3);

      pragma Assert (Ints.Container'(
                     Seq
                     & Ints.Op.Copy
                     & Ints.Op.Copy
                     & Ints.Col.Collect).Length = 3);
   end Op_Copy;

   ---------------
   -- Op_Filter --
   ---------------

   procedure Op_Filter is
      function Is_Even (I : Integer) return Boolean is (I mod 2 = 0);
   begin
      pragma Assert
        (Ints.Container'
           (Ints.Generators.Const_Iter (Vec)
            & Ints.Op.Filter (Is_Even'Access)
            & Ints.Col.Collect)
         .Length = 1);
   end Op_Filter;

   ------------------------------
   -- Keyed_Constant_Iteration --
   ------------------------------

   procedure Keyed_Constant_Iteration is
   begin
      for Pair of Ints.Keyed.Gen.Const_Iter (Vec) loop
--           Ada.Text_IO.Put_Line (Pair.Key'Img);
--           Ada.Text_IO.Put_Line (Pair.Pos.Element'Img);
         pragma Assert (Pair.Key = Pair.Pos.Get);
      end loop;
   end Keyed_Constant_Iteration;

   ------------------------------
   -- Keyed_Variable_Iteration --
   ------------------------------

   procedure Keyed_Variable_Iteration is
      Vec : Ints.Container := Vectors.Vec;
   begin
      for Pair of Ints.Keyed.Gen.Iter (Vec) loop
         Pair.Pos.Ref := Pair.Pos.Ref + 1;
         pragma Assert (Pair.Key = Pair.Pos.Get - 1);
      end loop;
   end Keyed_Variable_Iteration;

begin

   Manual_Constant_Iteration;
   Manual_Variable_Iteration;

   Constant_Of_Iteration;
   Variable_Of_Iteration;

   Op_Copy;
   Op_Filter;

   Keyed_Constant_Iteration;
   Keyed_Variable_Iteration;
end Iterators.Tests.Vectors;
