with Ada.Text_IO;

procedure Iterators.Tests.Main is

   use type Ada.Containers.Count_Type;

   --  Regular sequences
   use Int_Vec_Iters.Linkers;
   package Ints renames Int_Vec_Iters;

   --  Keyed sequences
   package KG renames Int_Vec_Iters.Keyed_Generators;

   Seq : constant Ints.Iterator'Class := Ints.Iterators.Just (1) & 2 & 3;
   Vec : constant Ints.Container :=
           Seq
           & Ints.Iterators.Copy
           & Ints.Collectors.Collect;

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
      Vec   : Ints.Container      := Main.Vec; -- Use RW copy
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
      Vec   : aliased Ints.Container := Main.Vec;
      It    : Ints.Iterator'Class    := Ints.Generators.Iter (Vec);
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
      Seq : constant Ints.Iterator'Class := Main.Seq;
   begin
      pragma Assert (Seq
                     & Ints.Iterators.Copy
                     & Ints.Iterators.Count = 3);
      --  Does not consume Seq because of Copy.

      pragma Assert (Seq & Ints.Iterators.Count = 3);
      --  Consumes Seq.

      pragma Assert (Seq & Ints.Iterators.Count = 0);
      --  Because Seq was just consumed.

      pragma Assert (Ints.Container'(Seq & Ints.Collectors.Collect).Length = 0);
      --  Alternate way, testing Collect
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
            & Ints.Iterators.Filter (Is_Even'Access)
            & Ints.Collectors.Collect)
         .Length = 1);
   end Op_Filter;

   ------------------------------
   -- Keyed_Constant_Iteration --
   ------------------------------

   procedure Keyed_Constant_Iteration is
   begin
      for Pair of KG.Const_Iter (Vec) loop
--           Ada.Text_IO.Put_Line (Pair.Key'Img);
--           Ada.Text_IO.Put_Line (Pair.Pos.Element'Img);
         pragma Assert (Pair.Key = Pair.Pos.Get);
      end loop;
   end Keyed_Constant_Iteration;

   ------------------------------
   -- Keyed_Variable_Iteration --
   ------------------------------

   procedure Keyed_Variable_Iteration is
      Vec : Ints.Container := Main.Vec;
   begin
      for Pair of KG.Iter (Vec) loop
         Pair.Pos.Ref := Pair.Pos.Ref + 1;
         pragma Assert (Pair.Key = Pair.Pos.Get - 1);
      end loop;
   end Keyed_Variable_Iteration;

   --------------------
   -- Map_Collection --
   --------------------

   procedure Map_Collection is
      Map : Int_Maps.Map;
      K, V : Integer;
   begin
      Map.Insert (1, 2);
      Map.Insert (2, 4);
      Map.Insert (3, 6);

      --  Verify plain iteration
      V := 2;
      for V2 of Int_Map_Iters.Generators.Const_Iter (Map) loop
         pragma Assert (V = V2);
         V := V + 2;
      end loop;

      --  Verify key/value pairs
      K := 1; V := 2;
      for Pair of Int_Map_Iters.Keyed_Generators.Const_Iter (Map) loop
         pragma Assert (K = Pair.Key);
         pragma Assert (V = Pair.Val);
         K := K + 1;
         V := K * 2;
      end loop;

      --  Verify collection into plain list
--        V := 2;
--        for V2 of Int_Map_Iters.Iterators.List'
--          (Int_Map_Iters.Keyed_Generators.Const_Iter (Map)
--           & Strip -- TODO: implement this as part of the type transformation operators
--           & Int_Map_Iters.Iterators.Collect)
--        loop
--           null;
--        end loop;

      --  Verify reassembly as map
      declare
         Map2 : constant Int_Maps.Map :=
                  Int_Map_Iters.Keyed_Collectors.Collect
                    (Int_Map_Iters.Keyed_Generators.Const_Iter (Map));
         use Ada.Containers;
      begin
         pragma Assert (Map.Length = Map2.Length);
         for Pos in Map.Iterate loop
            pragma Assert (Map (Pos) = Map2 (Int_Maps.Key (Pos)));
         end loop;
      end;
   end Map_Collection;

begin
   Manual_Constant_Iteration;
   Manual_Variable_Iteration;

   Constant_Of_Iteration;
   Variable_Of_Iteration;

   Op_Copy;
   Op_Filter;

   Keyed_Constant_Iteration;
   Keyed_Variable_Iteration;

   Map_Collection;

   Ada.Text_IO.Put_Line ("OK");

end Iterators.Tests.Main;
