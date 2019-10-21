private with AAA.Containers.Indefinite_Holders;

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

   package Linkers is

      function "&" (L : From.Iterator'Class;
                    R : Operator'Class) return Into.Iterator'Class;

   end Linkers;

   ---------------
   -- Operators --
   ---------------

   function Map (Map : not null access
                   function (E : From.Any_Element) return Into.Any_Element)
                 return Operator'Class;

private

   package Holders is new AAA.Containers.Indefinite_Holders (From.Iterator'Class);

   type Holder is new Holders.Holder with null record;

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with record
      Up : Holder; -- An operator has a mandatory upstream Iterator
   end record;

   procedure Set_Upstream (This     : in out Operator;
                           Upstream : From.Iterator'Class);

end Iterators.Operators;
