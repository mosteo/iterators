--  with Iterators.Pairs;
with Iterators.Root;

generic
   with package Unkeyed is new Root (<>);
   type Keys (<>) is private;
package Iterators.Keyed with Preelaborate is

end Iterators.Keyed;
