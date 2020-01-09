with AAA.Containers.Indefinite_Holders;

with Iterators.Root;

generic
   with package From is new Root (<>);
   with package Into is new Root (<>);
package Iterators.Operators with Preelaborate is

   --  Operators are chainable functions (monads) that take an iterator and
   --  return another iterator. The ones in this package transform the type
   --  they take into the one they generate. Thus, using any of these operators
   --  produces a read-only sequence, since the root container cursor cannot be
   --  preserved.

   --  On the contrary, some operators in Root.Operators take and return same-
   --  typed operators; in that case a read-write sequence can be maintained.

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with private;
   --  Operators that transform from one type into another.

   function Upstream (This : in out Operator'Class) return From.Iterator_Reference;

   function Concatenate (L : From.Iterator'Class;
                         R : Operator'Class) return Into.Iterator'Class;
   --  Function to build a chain of iterator & operators.
   --  Intended to be renamed as "&" in Linkers packages.

   -------------
   -- Linking --
   -------------

   package Linking is

      function "&" (L : From.Iterator'Class;
                    R : Operator'Class) return Into.Iterator'Class
                    renames Concatenate;
      --  Function to build a chain of iterator & operators.

   end Linking;

   --------------
   -- Sequence --
   --------------

   --  The Sequence type is the imperative alternative to "&"; it is a helper
   --  type that stores a sequence of iterator -> operator -> operator ...

   type Sequence is tagged limited private;

   procedure Start (This  : in out Sequence;
                    First :        From.Iterator'Class);
   --  Begin a sequence with First at the root

   procedure Continue (This : in out Sequence;
                       Last :        Operator'Class);
   --  This presumes that This already contains a chain, and Last is going
   --  to become the bottom of the chain, stored in this operator.

   function Iterate (This : Sequence) return Into.Iterator'Class;
   --  Functional view of the sequence

   --  Support functions, not normally needed for use

   function Has_First (This : Sequence) return Boolean;

   function Has_Last (This : Sequence) return Boolean;

   ---------------
   -- Operators --
   ---------------

   function Map (Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element)
                 return Operator'Class;

   procedure Map (This : in out Sequence;
                  Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element);

private

   subtype Upstream_Holder is From.Holder;

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with record
      Up : Upstream_Holder; -- An operator has a mandatory upstream Iterator
   end record;

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class);

   --------------
   -- Sequence --
   --------------

   package Holders is new
     AAA.Containers.Indefinite_Holders (Operator'Class);

   type Sequence is tagged limited record
      First : From.Holder;
      Last  : Into.Holder;
   end record;

end Iterators.Operators;
