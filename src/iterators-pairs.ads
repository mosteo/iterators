with AAA.Containers.Indefinite_Holders;

with Iterators.Root;

generic
   with package Root is new Iterators.Root (<>);
   type Keys (<>) is private;
package Iterators.Pairs with Preelaborate is

   --  Couple a unkeyed Iterator package with a Key type. This allows iterating
   --  over, e.g., a map, keeping the zero-cost reference to the element, at
   --  the expense of storing the key in the iterator.

   type Pairs is tagged private;

   function New_Pair (Key : Keys;
                      Pos : Root.Cursor) return Pairs;

   function Key (Pair : Pairs) return Keys;

   function Pos (Pair : Pairs) return Root.Cursor;

private

   package Key_Holders is new AAA.Containers.Indefinite_Holders (Keys);

   type Pairs is tagged record
      Key : Key_Holders.Holder;
      Pos : Root.Cursor;
   end record;

   --------------
   -- New_Pair --
   --------------

   function New_Pair (Key : Keys;
                      Pos : Root.Cursor) return Pairs is
     (Key_Holders.To_Holder (Key),
      Pos);

   ---------
   -- Key --
   ---------

   function Key (Pair : Pairs) return Keys is (Pair.Key.Element);

   ---------
   -- Pos --
   ---------

   function Pos (Pair : Pairs) return Root.Cursor is (Pair.Pos);

end Iterators.Pairs;
