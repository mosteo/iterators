package body Iterators.Transform is

   -------------
   -- Linkers --
   -------------

   package body Linkers is

      ---------
      -- "&" --
      ---------

      function "&" (L : From.Iterator'Class;
                    R : Operator'Class) return Into.Iterator'Class is
      begin
         return Result : Operator'Class := R do
            if Result.Up.Is_Empty then
               Result.Up := To_Holder (L);
            else
               declare
                  Parent : From.Iterator'Class renames Result.Up.Reference.Element.all;
               begin
                  if Parent in From.Operator'Class then
                     Result.Up :=
                       To_Holder (From."&" (L, From.Operator'Class (Parent)));
                  else
                     raise Constraint_Error with
                       "Operator required in RHS of ""&"" concatenator";
                  end if;
               end;
            end if;
         end return;
      end "&";

   end Linkers;

end Iterators.Transform;
