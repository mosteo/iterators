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

   -------------------------
   -- Ada iterator things --
   -------------------------

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Ada_Iterator) is
   begin
      if Is_Open (This.File) then
         Close (This.File);
      end if;
   end Finalize;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Cursor : Ada_Cursor) return Boolean
   is (not Cursor.EOF);

   -----------
   -- Lines --
   -----------

   function Get_Lines (File_Name : String) return Ada_Iterator is
   begin
      return This : Ada_Iterator (File_Name'Length) do
         This.Name := File_Name;
         Open (This.File, In_File, File_Name);
      end return;
   end Get_Lines;

   -----------
   -- First --
   -----------

   overriding function First (This : Ada_Iterator) return Ada_Cursor is
   begin
      if End_Of_File (This.File) then
         return (EOF => True, Line => Null_Unbounded_String);
      else
         declare
            Line : constant String := Get_Line (This.File);
         begin
            return (EOF => False, Line => To_Unbounded_String (Line));
         end;
      end if;
   end First;

   overriding function Next
     (This     : Ada_Iterator;
      Position : Ada_Cursor) return Ada_Cursor
   is (if Position.EOF
       then raise Use_Error with "EOF already reached"
       else This.First);

   -------------
   -- Element --
   -------------

   function Element (This : aliased Ada_Iterator'Class;
                     Pos  : Ada_Cursor) return String
   is (To_String (Pos.Line));

   -------------
   -- Iterate --
   -------------

   function Iterate (This : aliased Ada_Iterator)
                     return Ada_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      This'Unrestricted_Access.Finalize;
      return Get_Lines (This.Name);
   end Iterate;

end Iterators.Text_IO;
