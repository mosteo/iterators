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

   function Has_Upstream (This : Operator'Class) return Boolean;

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class);
   --  Replaces upstream going up the chain to the root upstream that has no
   --  parent yet still is an Operator'Class

   function Upstream (This : in out Operator'Class)
                      return From.Iterator_Reference;

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

   -----------
   -- Chain --
   -----------

   --  The Chain type is the imperative alternative to "&"; it is a helper
   --  type that stores a sequence of iterator -> operator -> operator ...

   type Chain is new Into.Iterator with private;

   overriding
   function Next (This : in out Chain) return Into.Cursor'Class;

   procedure Start (This  : in out Chain;
                    First :        From.Iterator'Class);
   --  Begin a sequence with First at the root. If the sequence was already
   --  started it is reset.

   procedure Resume (This : in out Chain;
                     Prev :        From.Iterator'Class) renames Start;

   procedure Continue (This : in out Chain;
                       Last :        Operator'Class);
   --  This presumes that This already contains a chain, and Last is going
   --  to become the bottom of the chain, stored in this operator.

   --  procedure Partial (This  : in out Chain;
   --                     First :        From.Iterator'Class);
   --  Start a partial chain with no source iterator. This partial chain will
   --  have to be somehow assembled into a complete chain to be usable (e.g.,
   --  as argument to Flat_Map).

   procedure For_Each
     (This  : in out Chain;
      Apply : access procedure (Element : Into.Any_Element));

   --  Support functions, not normally needed for use

   function First (This : in out Chain) return From.Iterator_Reference
     with Pre => This.Has_First;

   function Has_First (This : Chain) return Boolean;

   function Has_Last (This : Chain) return Boolean;

   function Last (This : in out Chain) return Into.Iterator_Reference
     with Pre => This.Has_Last;

   function As_Iterator (This : in out Chain) return Into.Iterator_Reference
                         renames Last;

   ---------------
   -- Operators --
   ---------------

   function Flat_Map (Map : not null access
                        function (E : From.Any_Element)
                                  return Into.Iterator'Class)
                      return Operator'Class;
   procedure Flat_Map (This : in out Chain;
                       Map : not null access
                        function (E : From.Any_Element)
                                  return Into.Iterator'Class);

   function Map (Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element)
                 return Operator'Class;
   procedure Map (This : in out Chain;
                  Map : not null access
                    function (E : From.Any_Element) return Into.Any_Element);

   function Scan (Initial : Into.Any_Element;
                  Scan_Fn : not null access function (L : Into.Any_Element;
                                                      R : From.Any_Element)
                                                      return Into.Any_Element)
                  return Operator'Class;

private

   subtype Upstream_Holder is From.Holder;

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with record
      Up : Upstream_Holder; -- An operator has a mandatory upstream Iterator
   end record;

   --------------
   -- Sequence --
   --------------

   package Holders is new
     AAA.Containers.Indefinite_Holders (Operator'Class);

   type Chain is new Into.Iterator with record
      First : From.Holder;
      Last  : Into.Holder;
   end record;

end Iterators.Operators;
