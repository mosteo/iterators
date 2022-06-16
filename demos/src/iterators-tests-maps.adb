procedure Iterators.Tests.Maps is

   package Ints renames Int_Map_Iters;
   use Ints.Linkers;

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
      for Pair of Ints.Keyed.Gen.Const_Iter (Map) loop
         pragma Assert (K = Pair.Key);
         pragma Assert (V = Pair.Val);
         K := K + 1;
         V := K * 2;
      end loop;

      --  Verify collection into plain list
      V := 2;
      for V2 of Ints.Iterators.List'
        (Ints.Gen.Const_Iter (Map)
         & Ints.Op.Collect)
      loop
         null;
      end loop;

      --  Verify stripping
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
                  Ints.Keyed.Gen.Const_Iter (Map)
                  & Ints.Col.Collect;
         use Ada.Containers;
      begin
         pragma Assert (Map.Length = Map2.Length);
         for Pos in Map.Iterate loop
            pragma Assert (Map (Pos) = Map2 (Int_Maps.Key (Pos)));
         end loop;
      end;
   end Map_Collection;

begin
   Map_Collection;
end Iterators.Tests.Maps;
