with Iterators.Operators;

generic
package Iterators.Root.Operators with Preelaborate is

   --  This public instance is a necessary evil to re-expose type-changing
   --  operators here:

   package Operators is new Iterators.Operators (Root, Root);

   subtype Operator is Operators.Operator;

   ---------------
   -- Reduction --
   ---------------
   --  Defining a reducer type allows cutting down the number of "&" to be
   --  defined.

   type Reducer is interface;

   function Reduce (This : Reducer;
                    From : Iterator'Class) return Reducer is abstract;

   -------------
   -- Linking --
   -------------

   package Linking is

      --  Expose here the linking operators for Operators and Reducers

      function "&" (L : Iterator'Class;
                    R : Operator'Class) return Iterator'Class
                    renames Operators.Linking."&";

--        function "&" (L : Iterator'Class;
--                      R : Reducer'Class) return Reducer'Class;

   end Linking;

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

   function Filter
     (Tester : access function (Element : Any_Element) return Boolean)
      return Operator'Class;
   --  Let only pass elements accepted by the function argument.

   function Map (Map : not null access
                   function (E : Any_Element) return Any_Element)
                 return Operator'Class renames Operators.Map;

   function No_Op return Operator'Class;
   --  Does nothing.

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

   -------------
   -- Sources --
   -------------

   function Just (Element : Any_Element) return Iterator'Class;
   --  Convert an element into an iterator for use as start of a sequence.

private

   type Counter is null record;

end Iterators.Root.Operators;
