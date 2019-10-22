with Iterators.Operators.Impl_Map;
--  with Iterators.Root.Impl_Collect;
--  with Iterators.Root.Impl_Copy;
--  with Iterators.Root.Impl_Count;
--  with Iterators.Root.Impl_Filter;
--  with Iterators.Root.Impl_Just;
--  with Iterators.Root.Impl_No_Op;

package body Iterators.Operators is

   -----------------
   -- Concatenate --
   -----------------

   function Concatenate (L : From.Iterator'Class;
                         R : Operator'Class) return Into.Iterator'Class is
   begin
      return RW : Operator'Class := R do
         RW.Set_Upstream (L);
      end return;
   end Concatenate;

   ------------------
   -- Set_Upstream --
   ------------------

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class) is
   begin
      if This.Up.Is_Empty then
         This.Up := Upstream.To_Holder;
      else
         declare
            Parent : From.Iterator'Class renames This.Up.Reference.Element.all;
         begin
            if Parent in Operator'Class then
               Operator'Class (Parent).Set_Upstream (Upstream);
            else
               raise Constraint_Error with
                 "Operator required upstream while linking partial chains";
            end if;
         end;
      end if;
   end Set_Upstream;

   --------------
   -- Upstream --
   --------------

   function Upstream (This : in out Operator'Class)
                      return From.Iterator_Reference is
     (This.Up.As_Iterator);

   -------------------------------------------------------------------------
   -- OPERATORS ------------------------------------------------------------
   -------------------------------------------------------------------------

   ---------
   -- Map --
   ---------

   package Map_Instance is new Impl_Map;
   function Map (Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element)
                 return Operator'Class renames Map_Instance.Create;

end Iterators.Operators;
