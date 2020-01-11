pragma Warnings (Off);
with GNAT.IO;
pragma Warnings (On);

package Iterators with Preelaborate is

   Iterator_Error : exception;
   --  Any error with usage of iterators

   Unimplemented : exception;

end Iterators;
