package body Iterators.Text_IO is

   -----------
   -- Lines --
   -----------

   function Lines (File : Ada.Text_IO.File_Type)
                   return Std.Strings.Iterator'Class
   is (raise Unimplemented);

   -----------
   -- Lines --
   -----------

   function Lines (File_Name : String)
                   return Std.Strings.Iterator'Class
   is (raise Unimplemented);

end Iterators.Text_IO;
