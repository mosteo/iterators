package body Iterators.Meta.Impl_Flatmap is

   --  type Operator is new Meta2iter.Operator with record
   --     Done : Boolean := False;
   --     Map  : Mapper;
   --     Iter : Into.Holder;
   --  end record;
   --
   --  ----------
   --  -- Next --
   --  ----------
   --
   --  overriding
   --  function Next (This : in out Operator) return Into.Cursor'Class is
   --
   --     ------------------
   --     -- Prepare_Iter --
   --     ------------------
   --
   --     function Prepare_Iter return Into.Cursor'Class is
   --        C : constant From.Cursor'Class := This.Upstream.Next;
   --     begin
   --        This.Iter.Clear;
   --        if C.Has_Element then
   --           This.Iter := This.Map (C.Element).To_Holder;
   --           return This.Next;
   --           -- Even if current Iter is empty, this ensures we move on to the
   --           -- next value.
   --        else
   --           This.Done := True;
   --           return Into.New_Empty_Cursor;
   --        end if;
   --     end Prepare_Iter;
   --
   --  begin
   --     if This.Iter.Is_Valid then
   --        declare
   --           C : constant Into.Cursor'Class := This.Iter.As_Iterator.Next;
   --        begin
   --           if C.Has_Element then
   --              return C;
   --           else
   --              return Prepare_Iter;
   --           end if;
   --        end;
   --     else
   --        return Prepare_Iter;
   --     end if;
   --  end Next;

   ------------
   -- Create --
   ------------

   function Create (Map : not null access
                      function (Iter : Base_Root.Iterator'Class)
                                return Base_Root.Any_Element)
                    return Meta2iter.Operator'Class
   is (raise Unimplemented);

end Iterators.Meta.Impl_Flatmap;
