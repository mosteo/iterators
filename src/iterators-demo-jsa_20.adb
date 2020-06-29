with Ada.Numerics.Float_Random;
with Ada.Text_IO;

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

      --  Alternative: For_Each loop

      Float_Iters.Op.For_Each (Float_Array_Iters.Iter (Data)
                               & Float_Iters.Op.Filter (Is_Candidate'Access)
                               & Float_Iters.Op.Take (10),
                               Set_Winner'Unrestricted_Access);

   end Top_Ten;

   ---------------------------
   -- Print_Running_Average --
   ---------------------------

   procedure Print_Running_Average (Width : Positive := 3) is
      --  use Std.Strings.Linking;
      use Float2str.Linking;
      use Str2Float.Linking;
      use Float_Iters.Meta.Linking;

      use Float_Iters.Meta;
      use Float_Iters.Op;
      --  use Std.Strings.Op;

      use Float2str;
      use Str2float;

      function Div (F : Float) return Float is (F / Float (Width));
   begin
      Std.Strings.Op.For_Each
        --  (Just ("7") & "8" & "6" & "-1" & "12" & "15" & "-1" & "18"
        (Text_IO.Lines ("file_avg_demo.txt")  --  For each line
         & Map (Value'Access)                 --  Convert to Float
         & Filter (Is_Positive'Access)        --  Skip "NaN"
         & Window (Size => Width, Skip => 1)  --  Group into new iterator
         & Flat_Map (Scan (0.0, "+"'Access)   --     Sum subiterator
                     & Last)                  --     Take last (total sum)
         & Map (Div'Access)                   --  Divide by window size
         & Map (Image'Access),                --  Back to string
         GNAT.IO.Put_Line'Access);            --  Print running average
   end Print_Running_Average;

   --------------------------------------
   -- Print_Running_Average_Imperative --
   --------------------------------------
   --  Alternative using the Chain type instead of "&" concatenators
   procedure Print_Running_Average_Imperative (Width : Positive := 3) is
      S2F : Str2Float.Chain;
      Flt : Float_Iters.Operators.Chain;
      F2M : Float_Iters.Meta.Chain_To_Meta;
      M2F : Float_Iters.Meta.Chain_From_Meta;
      Sum : Float_Iters.Operators.Chain;
      Avg : Float_Iters.Operators.Chain;
      F2S : Float2Str.Chain;

      use Float_Iters.Op;

      function Div (F : Float) return Float is (F / Float (Width));
   begin
      S2F.Start (Text_IO.Lines ("file_avg_demo.txt"));
      S2F.Map (Value'Access);
      Flt.Resume (S2F);
      Flt.Filter (Is_Positive'Access);
      F2M.Resume (Flt);
      F2M.Window (Size => Width, Skip => 1);
      M2F.Resume (F2M);

      Sum.Start (Scan (0.0, "+"'Access));
      Sum.Continue (Last);
      M2F.Flat_Map (Sum.As_Iterator);

      Avg.Resume (M2F);
      Avg.Map (Div'Access);
      F2S.Resume (Avg);
      F2S.Map (Image'Access);
      F2S.For_Each (GNAT.IO.Put_Line'Access);
   end Print_Running_Average_Imperative;

   -------------------------------------
   -- Print_Running_Average_Classical --
   -------------------------------------
   --  Functionally equivalent classical implementation
   procedure Print_Running_Average_Classical (Width : Positive := 3) is
      use Ada.Text_IO;
      File : File_Type;
      Window : Float_Iters.Iterators.List;
      --  Any list-like type would do; we use this one that's already available
      --  instead of instantiating a new one.
   begin
      Open (File, In_File, "file_avg_demo.txt");

      while not End_Of_File (File) loop
         declare
            Line : constant String := Get_Line (File);
            Num  : constant Float  := Float'Value (Line);
         begin
            --  Skip "NaN"
            if Num >= 0.0 then
               --  Add new sample
               Window.Append (Num);
               --  Prune the window
               if Natural (Window.Length) > Width then
                  Window.Delete_First;
               end if;
               --  New averaged value?
               if Natural (Window.Length) = Width then
                  declare
                     Total : Float := 0.0;
                  begin
                     --  Compute total. This could be done more efficiently
                     --  by keeping a running total. However, that might have
                     --  numerical implications due to unneeded substractions,
                     --  would not be functionally equivalent to the Iterator
                     --  version, and would be less self-evident.
                     for Sample of Window loop
                        Total := Total + Sample;
                     end loop;
                     --  Write the new running average value
                     GNAT.IO.Put_Line (Float'Image (Total / Float (Width)));
                  end;
               end if;
            end if;
         end;
      end loop;

      Close (File);
   end Print_Running_Average_Classical;

begin
   Average;
   Top_Ten;

   GNAT.IO.Put_Line ("Iterators functional version");
   Print_Running_Average;

   GNAT.IO.Put_Line ("Iterators imperative version");
   Print_Running_Average_Imperative;

   GNAT.IO.Put_Line ("Classical no-iterators version");
   Print_Running_Average_Classical;
end Iterators.Demo.JSA_20;
