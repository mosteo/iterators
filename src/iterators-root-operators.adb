with Iterators.Root.Operators.Impl_Append;
with Iterators.Root.Operators.Impl_Collect;
with Iterators.Root.Operators.Impl_Copy;
with Iterators.Root.Operators.Impl_Count;
with Iterators.Root.Operators.Impl_Filter;
with Iterators.Root.Operators.Impl_Just;
with Iterators.Root.Operators.Impl_No_Op;

package body Iterators.Root.Operators is

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
   function Collect return List is (Lists.Empty_List);
   function Collect (It : Iterator'Class) return List is
      (Collect_Instance.Reduce (It, Lists.Empty_List));
   function Collect (L : Iterator'Class; R : List) return List
                     renames Collect_Instance.Reduce;


   --------------
   -- Continue --
   --------------

   overriding
   procedure Continue (This : in out Sequence;
                       Last :        Operators.Operator'Class) is
   begin
      if This.Has_Last then
         Operators.Sequence (This).Continue
           (Operator'Class (Operators.Concatenate (This.To_Iterator, Last)));
      elsif This.Has_First then
         Operators.Sequence (This).Continue
           (Operator'Class (Operators.Concatenate (This.First, Last)));
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

   procedure Copy (This : in out Sequence) is
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

   ------------
   -- Filter --
   ------------

   package  Filter_Instance is new Impl_Filter;
   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Operator'Class is (Filter_Instance.Create (Tester));
   procedure Filter
     (This : in out Sequence;
      Tester : access function (Element : Any_Element) return Boolean) is
   begin
      This.Continue (Filter (Tester));
   end Filter;

   -------------
   -- Iterate --
   -------------

   overriding
   function To_Iterator (This : Sequence) return Iterator'Class is
   begin
      if This.Has_Last then
         return Operators.Sequence (This).To_Iterator;
      elsif This.Has_First then
         return This.First;
      else
         raise Iterator_Error with "Sequence is uninitialized";
      end if;
   end To_Iterator;

   ----------
   -- Just --
   ----------

   package  Just_Instance is new Impl_Just;
   function Just (Element : Any_Element) return Iterator'Class
                  renames Just_Instance.Create;

   ---------
   -- Map --
   ---------

   procedure Map (This : in out Sequence;
                  Map  : not null access
                    function (E : Any_Element) return Any_Element) is
   begin
      This.Map (This, Map);
   end Map;

   -----------
   -- No_Op --
   -----------

   package  No_Op_Instance is new Impl_No_Op;
   function No_Op return Operator'Class renames No_Op_Instance.Create;
   procedure No_Op (This : in out Sequence) is
   begin
      This.Continue (No_Op);
   end No_Op;

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
