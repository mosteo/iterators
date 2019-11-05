limited with Iterators.Contracts;

package Iterators with Pure is

   package Dynamic_Linking is

      function "&" (L : Contracts.Iterator'Class;
                    R : Contracts.Linkable'class)
                    return Contracts.Linkable'Class;

   end Dynamic_Linking;

private

   Unimplemented : exception;

end Iterators;
