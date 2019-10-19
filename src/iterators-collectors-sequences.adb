package body Iterators.Collectors.Sequences is

   use Containers;
   use Root;

   -------------
   -- Collect --
   -------------

   function Collect return Container is
      Empty : Container with Warnings => Off;
      --  Hoping that this will perform the expected default initialization...
   begin
      return Empty;
   end Collect;

   -------------
   -- Collect --
   -------------

   function Collect (It : Iterator'Class) return Container is (It & Collect);

   ----------
   -- Copy --
   ----------

   procedure Copy (Dst : in out Container;
                   Src :        Container)
   is
      Pos : Containers.Cursor := First (Src);
   begin
      while Has_Element (Pos) loop
         Appendable.Append (Dst, Constant_Reference (Src, Pos).Element.all, 1);
         Pos := Next (Pos);
      end loop;
   end Copy;

   ---------
   -- "&" --
   ---------

   function "&" (L : Iterator'Class;
                 R : Container)
                 return Container is
   begin
      return Result : Container do
         for E of L loop
            Appendable.Append (Result, E, 1);
         end loop;
         Copy (Result, R);
      end return;
   end "&";

end Iterators.Collectors.Sequences;
