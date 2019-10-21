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
         return RW : Operator'Class := R do
            RW.Set_Upstream (L);
         end return;
      end "&";

   end Linkers;

   ------------------
   -- Set_Upstream --
   ------------------

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class) is
   begin
      if This.Up.Is_Empty then
         This.Up := To_Holder (Upstream);
      else
         declare
            Parent : From.Iterator'Class renames This.Up.Reference.Element.all;
         begin
            if Parent in Operator'Class then
               Operator'Class (Parent).Set_Upstream (Upstream);
            else
               raise Constraint_Error with
                 "Operator required upstream while linking partial chains";
            end if;
         end;
      end if;
   end Set_Upstream;

end Iterators.Transform;
