with Iterators.Operators.Impl_Flat_Map;
with Iterators.Operators.Impl_Map;
with Iterators.Operators.Impl_Scan;

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

   procedure Continue (This : in out Chain;
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

   function First (This : in out Chain) return From.Iterator_Reference is
     (This.First.As_Iterator);

   ---------------
   -- Has_First --
   ---------------

   function Has_First (This : Chain) return Boolean is
     (This.First.Is_Valid);

   --------------
   -- Has_Last --
   --------------

   function Has_Last (This : Chain) return Boolean is
     (This.Last.Is_Valid);

   ------------------
   -- Has_Upstream --
   ------------------

   function Has_Upstream (This : Operator'Class) return Boolean is
     (This.Up.Is_Valid);

   ----------
   -- Last --
   ----------

   function Last (This : in out Chain) return Into.Iterator_Reference is
     (This.Last.As_Iterator);

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out Chain) return Into.Cursor'Class is
     (This.Last.As_Iterator.Next);

   ------------------
   -- Set_Upstream --
   ------------------

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class) is
   begin
      if This.Up.Is_Empty then
         This.Up := Upstream.To_Holder;
      else
         --  raise Iterator_Error with "Attemptint to reparent operator";
         --  MEGANOTE: this below in theory shouldn't work, but it does??????
         --  It seems changes undone in commit b857dc4 allow this to work again.
         declare
            Parent : From.Iterator'Class renames This.Up.Reference.Element.all;
         begin
            if Parent in Operator'Class then
            --  NOTE: this ceased working when Iterator was made Iterator itself
            --  Might be workaroundable by overriding in Root.Operators.
               Operator'Class (Parent).Set_Upstream (Upstream);
            else
               --  Root of chain reached
               This.Up := Upstream.To_Holder;
            end if;
         end;
      end if;
   end Set_Upstream;

   -----------
   -- Start --
   -----------

   procedure Start (This  : in out Chain;
                    First :        From.Iterator'Class) is
   begin
      This.First.Hold (First);
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

   --------------
   -- For_Each --
   --------------

   procedure For_Each
     (This  : in out Chain;
      Apply : access procedure (Element : Into.Any_Element))
   is
   begin
      for Elem of This.Last.Get loop
         Apply (Elem);
      end loop;
   end For_Each;

  --------------
   -- Flat_Map --
   --------------

   package Flat_Map_Instance is new Impl_Flat_Map;
   function Flat_Map (Map : not null access
                     function (E : From.Any_Element)
                               return Into.Iterator'Class)
                      return Operator'Class is
      (Flat_Map_Instance.Create (Map));
   procedure Flat_Map (This : in out Chain;
                       Map  : not null access
                         function (E : From.Any_Element)
                                   return Into.Iterator'Class)
   is
   begin
      This.Continue (Flat_Map (Map));
   end Flat_Map;

   ---------
   -- Map --
   ---------

   package Map_Instance is new Impl_Map;
   function Map (Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element)
                 return Operator'Class is
     (Map_Instance.Create (Map));

   procedure Map (This : in out Chain;
                  Map  : not null access
                    function (E : From.Any_Element) return Into.Any_Element)
   is
   begin
      This.Continue (Operators.Map (Map));
   end Map;

   ----------
   -- Scan --
   ----------

   package Scan_Instance is new Impl_Scan;
   function Scan (Initial : Into.Any_Element;
                  Scan_Fn : not null access function (L : Into.Any_Element;
                                                      R : From.Any_Element)
                                                      return Into.Any_Element)
                  return Operator'Class
   is (Scan_Instance.Create (Initial, Scan_Fn));

end Iterators.Operators;
