package body AAA.Iterators.Sequences is

   -------------
   -- Collect --
   -------------

   function Collect return List is (Lists.Empty_List);

   ---------
   -- "&" --
   ---------

   function "&" (L : Iterator'Class;
                 R : List)
                 return List
   is
      Result : List           := R;
      RW_It  : Iterator'Class := L;
   begin
      loop
         declare
            Pos : constant Cursor'Class := RW_It.Next;
         begin
            exit when Pos.Is_Empty;
            Result.Append (Pos.Get);
         end;
      end loop;

      return Result;
   end "&";

end AAA.Iterators.Sequences;
