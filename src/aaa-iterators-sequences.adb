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
            Ref : constant Iterators.Reference := RW_It.Next;
         begin
            exit when Ref.Element = null;
            Result.Append (Ref);
         end;
      end loop;

      return Result;
   end "&";

end AAA.Iterators.Sequences;
