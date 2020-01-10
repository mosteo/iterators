with Iterators.Root.Operators;

package body Iterators.Collectors.Mappings is

   -------------
   -- Collect --
   -------------

   function Collect (L : Keyed_Iterators.Iterator'Class;
                     R : Containers.Container)
                     return Containers.Container is
      (Collect (L, R, Allow_Replacement => False));

   -------------
   -- Collect --
   -------------

   function Collect return Containers.Container is
   begin
      return Col : Containers.Container;
   end Collect;

   -------------
   -- Collect --
   -------------

   package Operators is new Keyed_Iterators.Iterators.Operators;

   function Collect (L                 : Keyed_Iterators.Iterator'Class;
                     R                 : Containers.Container := Collect;
                     Allow_Replacement : Boolean := False)
                     return Containers.Container is
   begin
      return Result : Containers.Container do

         --  Collect the iterator first:

         for E of Operators.Operators.Concatenate (L, Operators.Copy) loop
            if Allow_Replacement then
               Keyed_Containers.Include (Result, E.Key, E.Val);
            else
               Keyed_Containers.Insert (Result, E.Key, E.Val);
            end if;
         end loop;

         --  And copy any element in the rightmost container:

         declare
            Pos : Containers.Cursor := Containers.First (R);
         begin
            while Containers.Has_Element (Pos) loop
               if Allow_Replacement then
                  Keyed_Containers.Include
                    (Result,
                     Keyed_Containers.Key (Pos),
                     Containers.Constant_Reference (R, Pos).Element.all);
               else
                  Keyed_Containers.Insert
                    (Result,
                     Keyed_Containers.Key (Pos),
                     Containers.Constant_Reference (R, Pos).Element.all);
               end if;

               Pos := Containers.Next (Pos);
            end loop;
         end;

      end return;
   end Collect;

end Iterators.Collectors.Mappings;
