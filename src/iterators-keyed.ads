with Iterators.Pairs;
with Iterators.Root;

generic
   with package Unkeyed is new Root (<>);
   type Keys (<>) is private;
package Iterators.Keyed is

   package Pairs is new Iterators.Pairs (Unkeyed, Keys);

   package Iterators is new Root (Pairs.Pair);

private

   type Pair is null record;

end Iterators.Keyed;
