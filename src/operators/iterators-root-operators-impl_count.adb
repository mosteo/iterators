package body Iterators.Root.Operators.Impl_Count is

   function Reduce (It : Iterator'Class) return Natural is
   begin
      return Count : Natural := 0 do
         for Unused of It loop
            Count := Count + 1;
         end loop;
      end return;
   end Reduce;

end Iterators.Root.Operators.Impl_Count;
