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


   ----------
   -- Copy --
   ----------

   package  Copy_Instance is new Impl_Copy;
   function Copy return Operator'Class renames Copy_Instance.Create;

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
--
--     -- Just --
--
   ----------
   -- Just --
   ----------

   package  Just_Instance is new Impl_Just;
   function Just (Element : Any_Element) return Iterator'Class
                  renames Just_Instance.Create;

   -----------
   -- No_Op --
   -----------

   package  No_Op_Instance is new Impl_No_Op;
   function No_Op return Operator'Class renames No_Op_Instance.Create;

end Iterators.Root.Operators;
