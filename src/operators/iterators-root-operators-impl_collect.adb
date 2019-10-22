package body Iterators.Root.Operators.Impl_Collect is

   function Reduce (L : Iterator'Class; R : List) return List is
   begin
      return Result : List do
         for E of L loop
            Result.Append (E);
         end loop;
         for E of R loop
            Result.Append (E);
         end loop;
      end return;
   end Reduce;

end Iterators.Root.Operators.Impl_Collect;
