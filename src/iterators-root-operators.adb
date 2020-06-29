with Ada.Unchecked_Conversion;

with Iterators.Root.Operators.Impl_Append;
with Iterators.Root.Operators.Impl_Collect;
with Iterators.Root.Operators.Impl_Copy;
with Iterators.Root.Operators.Impl_Count;
with Iterators.Root.Operators.Impl_Empty;
with Iterators.Root.Operators.Impl_Filter;
with Iterators.Root.Operators.Impl_Just;
with Iterators.Root.Operators.Impl_Last;
with Iterators.Root.Operators.Impl_No_Op;
with Iterators.Root.Operators.Impl_Reduce;
with Iterators.Root.Operators.Impl_Take;

package body Iterators.Root.Operators is

   ----------
   -- Next --
   ----------

   overriding
   function Next (This : in out Chain) return Cursor'Class is
     (if This.Has_Last
      then Operators.Chain (This).Next
      elsif This.Has_First
      then This.First.Next
      else raise Iterator_Error with "Uninitialized sequence");

   ----------------
   --  OPERATORS --
   ----------------

   ------------
   -- Append --
   ------------

   package  Append_Instance is new Impl_Append;
   function Append (Element : Any_Element) return Operator'Class
                    renames Append_Instance.Create;
   function Append (L : Iterator'Class; R : Any_Element) return Iterator'Class
                    renames Append_Instance.Append;

   -------------
   -- Collect --
   -------------

   package Collect_Instance is new Impl_Collect;
   function Collect return List is (Lists.Empty_List with null record);
   function Collect (It : Iterator'Class) return List is
      (Collect_Instance.Reduce (It, (Lists.Empty_List with null record)));
   function Collect (L : Iterator'Class; R : List) return List
                     renames Collect_Instance.Reduce;


   --------------
   -- Continue --
   --------------

   overriding
   procedure Continue (This : in out Chain;
                       Last :        Operators.Operator'Class) is
   begin
      if This.Has_Last then
         This.Resume (This); -- Set ourselves as previous upstream
         Operators.Chain (This).Continue (Last); -- And continue from there
      elsif This.Has_First then
         Operators.Chain (This).Continue (Last);
      else
         raise Iterator_Error with
           "Attempting to continue without source iterator";
      end if;
   end Continue;

   ----------
   -- Copy --
   ----------

   package  Copy_Instance is new Impl_Copy;
   function Copy return Operator'Class renames Copy_Instance.Create;

   procedure Copy (This : in out Chain) is
   begin
      This.Continue (Copy);
   end Copy;

   -----------
   -- Count --
   -----------

   package Count_Instance is new Impl_Count;
   function Count return Counter is (null record);
   function Count (L : Iterator'Class; R : Counter) return Natural is (Count (L));
   function Count (It : Iterator'Class) return Natural renames Count_Instance.Reduce;

   -----------
   -- Empty --
   -----------

   package Empty_Instance is new Impl_Empty;
   function Empty return Iterator'Class renames Empty_Instance.Create;

   ------------
   -- Filter --
   ------------

   package  Filter_Instance is new Impl_Filter;
   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Operator'Class is (Filter_Instance.Create (Tester));
   procedure Filter
     (This : in out Chain;
      Tester : access function (Element : Any_Element) return Boolean) is
   begin
      This.Continue (Filter (Tester));
   end Filter;

   --------------
   -- Flat_Map --
   --------------

   procedure Flat_Map (This : in out Chain;
                       Map : not null access
                        function (E : Any_Element)
                                  return Iterator'Class) is
   begin
      Operators.Chain (This).Flat_Map (Map);
   end Flat_Map;

   --------------
   -- For_Each --
   --------------

   procedure For_Each
     (Iter  : Iterator'Class;
      Apply : Const_Apply := null)
   is
   begin
      for Element of Iter loop
         if Apply /= null then
            Apply (Element);
         end if;
      end loop;
   end For_Each;

   --------------
   -- For_Each --
   --------------

   procedure For_Each
     (Iter  : Iterator'Class;
      Apply : Var_Apply := null)
   is
      use Linking;
      RW : Iterator'Class := Iter & No_Op;
   begin
      --  We iterate no matter what, to consume the sequence:
      for Element of RW loop
         if Apply /= null then
            Apply (Element);
         end if;
      end loop;
   end For_Each;

   ----------
   -- Just --
   ----------

   package  Just_Instance is new Impl_Just;
   function Just (Element : Any_Element) return Iterator'Class
                  is (Just_Instance.Create (Element));

   ----------
   -- Last --
   ----------

   package Last_Instance is new Impl_Last;
   function Last return Operator'Class renames Last_Instance.Create;

   ---------
   -- Map --
   ---------

   procedure Map (This : in out Chain;
                  Map  : not null access
                    function (E : Any_Element) return Any_Element) is
   begin
      Operators.Chain (This).Map (Map);
   end Map;

   -----------
   -- No_Op --
   -----------

   package  No_Op_Instance is new Impl_No_Op;
   function No_Op return Operator'Class renames No_Op_Instance.Create;
   procedure No_Op (This : in out Chain) is
   begin
      This.Continue (No_Op);
   end No_Op;

   -------------
   -- Partial --
   -------------

   --  procedure Partial (This  : in out Chain;
   --                     First          Operator'Class)
   --  is
   --  begin
   --  end Partial;

   ------------
   -- Reduce --
   ------------

   package Reduce_Instance is new Impl_Reduce;
   function Reduce (L : Iterator'Class; R : Reducer) return Any_Element
                    renames Reduce_Instance.Reduce;
   function Reduce (Initial   : Any_Element;
                    Reduce_Fn : Reduce_Fn_Access)
                    return Reducer
   is
      --  function Cast is new Ada.Unchecked_Conversion (
   begin
        return Reducer'(Initial   => Elem_Holders.To_Holder (Initial),
                        Reduce_Fn => Reduce_Fn);
   end Reduce;

   ----------
   -- Scan --
   ----------

   function Scan (Initial : Any_Element;
                  Scan_Fn : not null access function (L, R : Any_Element)
                  return Any_Element)
                  return Operator'Class
                  renames Operators.Scan;

   ----------
   -- Take --
   ----------

   package Take_Instance is new Impl_Take;
   function Take (At_Most : Natural) return Operator'Class
                  renames Take_Instance.Create;

   ------------------
   -- Set_Upstream --
   ------------------

   --  This is the version that allows attaching partial chains. This is yet
   --  unsupported since that would entail creating a new operator derived type
   --  here, and that breaks a havoc I don't have time for.

--     procedure Set_Upstream (This     : in out Operator;
--                             Upstream : Iterator'Class) is
--     begin
--        if not This.Has_Upstream then
--           Operators.Operator (This).Set_Upstream (Upstream);
--        else
--           declare
--              Parent : Iterator'Class renames This.Upstream;
--           begin
--              if Parent in Operator'Class then
--                 Operator'Class (Parent).Set_Upstream (Upstream);
--              else
--                 raise Iterator_Error with "Attempting to reparent operator";
--              end if;
--           end;
--        end if;
--     end Set_Upstream;

end Iterators.Root.Operators;
