package body Iterators.Meta.Impl_Flatmap is

   type Operator is new Meta2iter.Operator with record
      Initialized : Boolean := False;
      Sub  : Base_Root.Holder; -- The subchain to attach to every upstream
      Iter : Base_Root.Holder; -- The current chain being consumed
   end record;

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out Operator) return Base_Root.Cursor'Class is

      ------------------
      -- Prepare_Iter --
      ------------------

      procedure Prepare_Iter is
         use Base_Operators.Linking;
         C : constant Meta_Root.Cursor'Class := This.Upstream.Next;
      begin
         This.Iter.Clear;
         if C.Has_Element then
            This.Iter.Hold (C.Element
                            & Base_Operators.Operator'Class (This.Sub.Element));
         end if;
      end Prepare_Iter;

   begin
      if not This.Initialized then
         This.Initialized := True;
         Prepare_Iter;
      end if;

      while This.Iter.Is_Valid loop
         declare
            C : constant Base_Root.Cursor'Class := This.Iter.As_Iterator.Next;
         begin
            if C.Has_Element then
               --  GNAT.IO.Put_Line ("regular advance");
               return C;
            else
               --  GNAT.IO.Put_Line ("upstream advance");
               Prepare_Iter;
            end if;
         end;
      end loop;

      return Base_Root.New_Empty_Cursor;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (Map : Base_Root.Iterator'Class := Base_Operators.No_Op)
                    return Meta2iter.Operator'Class
   is (Operator'(Meta2iter.Operator with
                 Sub => Map.To_Holder,
                 others => <>));

   --  function Create (Map : not null access
   --                     function (Iter : Base_Root.Iterator'Class)
   --                               return Base_Root.Any_Element)
   --                   return Meta2iter.Operator'Class
   --  is (raise Unimplemented);

end Iterators.Meta.Impl_Flatmap;
