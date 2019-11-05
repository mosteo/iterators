with Iterators.Contracts;

package body Iterators is

   ---------------------
   -- Dynamic_Linking --
   ---------------------

   package body Dynamic_Linking is

      ---------
      -- "&" --
      ---------

      function "&" (L : Contracts.Iterator'Class;
                    R : Contracts.Linkable'class)
                    return Contracts.Linkable'Class is
      begin
         return Linked : Contracts.Linkable'Class := R do
            Linked.Set_Parent (L);
         end return;
      end "&";

   end Dynamic_Linking;

end Iterators;
