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

   --------------
   -- Continue --
   --------------

   procedure Continue (This : in out Sequence;
                       Last :        Operator'Class) is
   begin
      if This.First.Is_Empty then
         raise Iterator_Error with
           "Attempt to continue without initial iterator";
      else
         This.Last.Hold
           (Concatenate
              (This.First.Element, Last));
      end if;
   end Continue;

   ---------------
   -- Has_First --
   ---------------

   function Has_First (This : Sequence) return Boolean is
     (This.First.Is_Valid);

   --------------
   -- Has_Last --
   --------------

   function Has_Last (This : Sequence) return Boolean is
     (This.Last.Is_Valid);

   -------------
   -- Iterate --
   -------------

   function Iterate (This : Sequence) return Into.Iterator'Class is
     (This.Last.Element);

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

   -----------
   -- Start --
   -----------

   procedure Start (This  : in out Sequence;
                    First :        From.Iterator'Class) is
   begin
      if This.Last.Is_Valid then
         raise Iterator_Error with
           "Attempting to start an already-started sequence";
      else
         This.First.Hold (First);
      end if;
   end Start;

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
                 return Operator'Class is
     (Map_Instance.Create (Map));

   procedure Map (This : in out Sequence;
                  Map  : not null access
                    function (E : From.Any_Element) return Into.Any_Element) is
   begin
      This.Continue (Map_Instance.Create (Map));
   end Map;

end Iterators.Operators;
