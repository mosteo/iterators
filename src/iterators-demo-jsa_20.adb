with Ada.Numerics.Float_Random;

with Iterators.Std;
with Iterators.Text_IO;

procedure Iterators.Demo.JSA_20 is

   --  All necessary instantiations are in Iterators.Demo

   Rnd : Ada.Numerics.Float_Random.Generator;

   use Float_Iters.Linking;

   Data : aliased Float_Array_Iters.Element_Array :=
            (1 .. 1000 => Ada.Numerics.Float_Random.Random (Rnd));

   -------------
   -- Average --
   -------------

   procedure Average is
      Average : Float := 0.0;
   begin
      --  Manually compute an average

      for I in Data'Range loop
         Average := Average + Data (I);
      end loop;
      Average := Average / Float (Data'Length);

      --  same average using Reduce

      pragma Assert (Average =
                     (Float_Array_Iters.Const_Iter (Data)
                        & Float_Iters.Op.Reduce (0.0, "+"'Access))
                     / Float (Data'Length));
   end Average;

   -------------
   -- Top_Ten --
   -------------

   procedure Top_Ten is
      function Is_Candidate (Datum : Float) return Boolean is (Datum > 0.9);
      procedure Set_Winner (Datum : in out Float) is null;
   begin

      --  Plain loop

      for Element of Float_Iters.Iter -- Workaround for "of" bug -- see Iter decl
        (Float_Array_Iters.Iter (Data)
         & Float_Iters.Op.Filter (Is_Candidate'Access)
         & Float_Iters.Op.Take (10)).all
      loop
         Set_Winner (Element);
      end loop;

      --  For_Each loop

      Float_Iters.Op.For_Each (Float_Array_Iters.Iter (Data)
                               & Float_Iters.Op.Filter (Is_Candidate'Access)
                               & Float_Iters.Op.Take (10),
                               Set_Winner'Unrestricted_Access);

   end Top_Ten;

   ------------------
   -- File_Average --
   ------------------

   procedure File_Average (Width : Positive := 3) is
      use Std.Strings.Linking;
      use Float2str.Linking;
      use Str2Float.Linking;
      use Float_Iters.Meta.Linking;

      use Float_Iters.Meta;
      use Float_Iters.Op;

      use Float2str;
      use Str2float;

      function Sum (Iter : Float_Iters.Iterator'Class) return Float
      is (Iter & Reduce (0.0, "+"'access));

      function Div (F : Float) return Float is (F / Float (Width));
   begin
      Std.Strings.Op.For_Each
        (Std.Strings.Op.Just ("7") & "8" & "6" & "-1" & "12" & "15" & "-1"
         --  Text_IO.Lines ("file_avg_demo.txt")
         & Map (Value'Access)
         & Filter (Is_Positive'Access)
         & Window (Size => Width, Skip => Width)
         & Flat_Map (Sum'Access)
         & Map (Div'Access)
         & Map (Image'Access),
         GNAT.IO.Put_Line'Access);
   end File_Average;

begin
   Average;
   Top_Ten;
   --  File_Average;

end Iterators.Demo.JSA_20;
