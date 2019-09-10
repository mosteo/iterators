with AAA.Containers.Indefinite_Holders;

with Iterators.Root;

generic
   with package Root is new Iterators.Root (<>);
   type Keys (<>) is private;
package Iterators.Pairs is

   type Any_Pair (<>) is tagged private;

   function Key (Pair : Any_Pair) return Keys;

   function Element (Pair : Any_Pair) return Root.Cursor;

private

   package Key_Holders is new AAA.Containers.Indefinite_Holders (Keys);

   type Any_Pair is tagged record
      Key     : Key_Holders.Holder;
      Element : Root.Cursor;
   end record;

end Iterators.Pairs;
