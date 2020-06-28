with Iterators.Operators;

generic
package Iterators.Root.Operators with Preelaborate is

   --  This public instance is a necessary evil to re-expose type-changing
   --  operators here:

   package Operators is new Iterators.Operators (Root, Root);

   subtype Operator is Operators.Operator;
   subtype Iterator is Root.Iterator;

   type Chain is new Operators.Chain with null record;

   overriding
   procedure Continue (This : in out Chain;
                       Last :        Operators.Operator'Class);

   overriding
   function Next (This : in out Chain) return Cursor'Class;

   ---------------
   -- Operators --
   ---------------

   -- Operators are passive and do not cause calls to Next, until a reducer or
   -- collector is invoked at the end of a chain of operators.

   function Append (Element : Any_Element) return Operator'Class;
   function Append (L : Iterator'Class; R : Any_Element) return Iterator'Class;
   --  Add an element to the previous iterator sequence.

   function Copy return Operator'Class;
   --  Copies the preceding iterator, so it is not consumed by subsequent
   --  operators.
   procedure Copy (This : in out Chain);

   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Operator'Class;
   --  Let only pass elements accepted by the function argument.
   procedure Filter
     (This : in out Chain;
      Tester : access function (Element : Any_Element) return Boolean);

   function Flat_Map (Map : not null access
                        function (E : Any_Element)
                                  return Iterator'Class)
                      return Operators.Operator'Class
                      renames Operators.Flat_Map;
   procedure Flat_Map (This : in out Chain;
                       Map : not null access
                        function (E : Any_Element)
                                  return Iterator'Class);

   function Map (Map : not null access
                   function (E : Any_Element) return Any_Element)
                 return Operators.Operator'Class
                 renames Operators.Map;

   procedure Map (This : in out Chain;
                  Map  : not null access
                    function (E : Any_Element) return Any_Element);

   function No_Op return Operator'Class;
   --  Does nothing.

   procedure No_Op (This : in out Chain);

   function Take (At_Most : Natural) return Operator'Class;

   --------------
   -- Reducers --
   --------------

   --  Reducers trigger the pulling of elements until the sequence is exhausted.

   function Collect return List;
   function Collect (It : Iterator'Class) return List;
   function Collect (L : Iterator'Class; R : List) return List;

   type Counter (<>) is private;
   function Count return Counter;
   function Count (It : Iterator'Class) return Natural;
   function Count (L : Iterator'Class; R : Counter) return Natural;

   type Reducer (<>) is private;
   type Reduce_Fn_Access is
     access function (L, R : Any_Element) return Any_Element;
   function Reduce (L : Iterator'Class; R : Reducer) return Any_Element;
   function Reduce (Initial   : Any_Element;
                    Reduce_Fn : Reduce_Fn_Access)
                    return Reducer;
   --  Generic reduction, from a starting value and applying Reduce_Fn to each
   --  incoming element.

   -------------
   -- Sources --
   -------------

   function Empty return Iterator'Class;

   function Just (Element : Any_Element) return Iterator'Class;
   --  Convert an element into an iterator for use as start of a sequence.

   ---------------
   -- Terminals --
   ---------------

   type Const_Apply is access procedure (Element : Any_Element);
   type Var_Apply   is access procedure (Element : in out Any_Element);
   --  These named access types allow having two For_Each with the same name.
   --  Otherwise, the "in out" is not enough to avoid the name clash.

   procedure For_Each
     (Iter  : Iterator'Class;
      Apply : Const_Apply := null);

   procedure For_Each
     (Iter  : Iterator'Class;
      Apply : Var_Apply := null);
   --  Although Iter is mode "in", to allow direct application to a returned
   --  chain (e.g., A & B), the actual writability depends on the root iterator
   --  being created with Iter and not Const_Iter. Otherwise, this will be
   --  detected at runtime.

   -------------
   -- Linking --
   -------------

   package Linking is

      --  Expose here the linking operators for Operators and Reducers. This is
      --  an "use"-intended package.

      function "&" (L : Iterator'Class;
                    R : Operators.Operator'Class) return Iterator'Class
                    renames Operators.Linking."&";

      function "&" (L : Iterator'Class;
                    R : Any_Element) return Iterator'Class
              renames Append;

      function "&" (L : Iterator'Class;
                    R : Counter) return Natural
                    renames Count;

      function "&" (L : Iterator'Class;
                    R : List) return List
                    renames Collect;

      function "&" (L : Iterator'Class;
                    R : Reducer) return Any_Element
                    renames Reduce;

   end Linking;

private

   type Counter is null record;

   type Reducer is record
      Initial   : Elem_Holders.Holder;
      Reduce_Fn : Reduce_Fn_Access;
   end record;

end Iterators.Root.Operators;
