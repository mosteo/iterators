package body Iterators.Meta.Impl_Window is

   type Operator is new Meta_Operator with record
      Size, Skip : Positive;
      List       : Base_Root.List;
      Iter       : Base_Root.Holder;
      Done       : Boolean := False;
   end record;

   overriding
   function Next (This : in out Operator) return Meta_Root.Cursor'Class is

      function New_List_Iter return Meta_Root.Cursor'Class is
      begin
         This.Iter.Hold (This.List.Const_Iter);
         return Meta_Root.New_Const_Cursor (This.Iter.Get);
      end New_List_Iter;

   begin
      if This.Done then
         return Meta_Root.New_Empty_Cursor;
      end if;

      --  Prune list if just returned:

      if Natural (This.List.Length) = This.Size then
         for I in 1 .. This.Skip loop
            This.List.Delete_First;
         end loop;
      end if;

      --  Now gather new elements until completing the list, and return its
      --  iterator.

      loop
         declare
            Pos : constant Base_Root.Cursor'Class := This.Upstream.Next;
         begin
            if Pos.Is_Empty then
               if This.List.Is_Empty then
                  This.Done := True;
                  return Meta_Root.New_Empty_Cursor;
               else
                  This.Done := True;
                  return New_List_Iter;
               end if;
            else -- There is at least one more element
               This.List.Append (Pos.Get);
               if Natural (This.List.Length) = This.Size then
                  return New_List_Iter;
               end if;
            end if;
         end;
      end loop;
   end Next;

   ------------
   -- Create --
   ------------

   function Create (Size : Positive;
                    Skip : Positive)
                    return Meta_Operator'Class
   is (Operator'(Meta_Operator with
                 Size      => Size,
                 Skip      => Skip,
                 others    => <>));

end Iterators.Meta.Impl_Window;
