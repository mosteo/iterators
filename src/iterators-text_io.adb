with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body Iterators.Text_IO is

   use Ada.Text_IO;

   type File_Access is access File_Type;
   type String_Access is access String;

   procedure Free is new Ada.Unchecked_Deallocation (File_Type, File_Access);
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   type Iterator is new Std.Strings.Iterator with record
      File : File_Access;
      Line : String_Access;
      --  TODO: these pointers should be refcounted, in case the file is not
      --  read in its entirety.
   end record;

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out Iterator)
                  return Std.Strings.Iterators.Cursor'Class
   is
   begin
      if This.File = null then
         return Std.Strings.Iterators.New_Empty_Cursor;
      elsif End_Of_File (This.File.all) then
         Close (This.File.all);
         Free (This.File);
         Free (This.Line);
         return Std.Strings.Iterators.New_Empty_Cursor;
      else
         Free (This.Line);
         This.Line := new String'(Get_Line (This.File.all));
         return Std.Strings.Iterators.New_Const_Cursor (This.Line.all);
      end if;
   end Next;

   -----------
   -- Lines --
   -----------

   function Lines (File_Name : String)
                   return Std.Strings.Iterator'Class
   is
   begin
      return Iter : Iterator do
         begin
            Iter.File := new File_Type;
            Open (Iter.File.all, In_File, File_Name);
         exception
            when others =>
               if Iter.File /= null then
                  Free (Iter.File);
               end if;
         end;
      end return;
   end Lines;

end Iterators.Text_IO;
