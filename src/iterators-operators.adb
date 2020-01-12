with Iterators.Operators.Impl_Map;

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
                       Last :        Operator'Class)
   is
      RW : Operator'Class := Last;
   begin
      if Last.Up.Is_Empty and then This.First.Is_Valid then
         RW.Up.Hold (This.First.Element);
         This.Last.Hold (RW);
      else
         raise Iterator_Error with
           "Attempting to continue without previous iterator";
      end if;
   end Continue;

   -----------
   -- First --
   -----------

   function First (This : Sequence) return From.Iterator'Class is
     (This.First.Element);

   --------------
   -- Flat_Map --
   --------------

--     function Flat_Map (Map : not null access
--                       function (E : From.Any_Element)
--                                 return Into.Iterable'Class)
--                        return Operator'Class is
--     begin
--     end Flat_Map;
--     procedure Flat_Map (This : in out Sequence;
--                         Prev :        From.Iterable'Class;
--                         Map  : not null access
--                           function (E : From.Any_Element)
--                                     return Into.Iterable'Class)
--     is
--     begin
--     end Flat_Map;

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

   ------------------
   -- Has_Upstream --
   ------------------

   function Has_Upstream (This : Operator'Class) return Boolean is
     (This.Up.Is_Valid);

   -------------
   -- Iterate --
   -------------

   function To_Iterator (This : Sequence) return Into.Iterator'Class is
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
         raise Iterator_Error with "Attemptint to reparent operator";
--           declare
--              Parent : From.Iterator'Class renames This.Up.Reference.Element.all;
--           begin
--              if Parent in Operator'Class then
--              NOTE: this ceased working when Iterator was made Iterable itself
--              Might be workaroundable by overriding in Root.Operators.
--                 Operator'Class (Parent).Set_Upstream (Upstream);
--              else
--                 --  Root of chain reached
--                 This.Up := Upstream.To_Holder;
--              end if;
--           end;
      end if;
   end Set_Upstream;

   -----------
   -- Start --
   -----------

   procedure Start (This  : in out Sequence;
                    First :        From.Iterable'Class) is
   begin
      This.First.Hold (First.To_Iterator);
      This.Last.Clear;
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
                    function (E : From.Any_Element) return Into.Any_Element)
   is
   begin
      This.Continue (Map_Instance.Create (Map));
   end Map;

end Iterators.Operators;
