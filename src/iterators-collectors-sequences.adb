package body Iterators.Collectors.Sequences is

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

   function Collect (It : Iterator'Class) return Container is
     (Collect (It, Collect));

   -------------
   -- Collect --
   -------------

   function Collect (L : Iterator'Class;
                     R : Container)
                     return Container is
   begin
      return Result : Container do
         for E of L loop
            Appendable.Append (Result, E, 1);
         end loop;
         Appendable.Copy (Result, R);
      end return;
   end Collect;

end Iterators.Collectors.Sequences;
