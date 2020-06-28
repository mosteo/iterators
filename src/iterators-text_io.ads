with Iterators.Std;

package Iterators.Text_IO is

   --  Read a file as a sequence of lines

   function Lines (File_Name : String)
                   return Std.Strings.Iterator'Class;

end Iterators.Text_IO;
