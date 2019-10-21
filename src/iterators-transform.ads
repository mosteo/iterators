private with AAA.Containers.Indefinite_Holders;

with Iterators.Root;

generic
   with package From is new Root (<>);
   with package Into is new Root (<>);
package Iterators.Transform with Preelaborate is

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with private;
   --  Operators that transform from one type into another.

   package Linkers is

      function "&" (L : From.Iterator'Class;
                    R : Operator'Class) return Into.Iterator'Class;

   end Linkers;

private

   package Holders is new AAA.Containers.Indefinite_Holders (From.Iterator'Class);

   type Holder is new Holders.Holder with null record;

   --------------
   -- Operator --
   --------------

   type Operator is abstract new Into.Iterator with record
      Up : Holder; -- An operator has a mandatory upstream Iterator
   end record;

end Iterators.Transform;
