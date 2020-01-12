package body Iterators.Root.Operators.Impl_Collect is

   function Reduce (L : Iterable'Class; R : List) return List is
   begin
      return Result : List do
         for E of L.To_Iterator loop
            Result.Append (E);
         end loop;
         for E of R.To_Iterator loop
            Result.Append (E);
         end loop;
      end return;
   end Reduce;

end Iterators.Root.Operators.Impl_Collect;
