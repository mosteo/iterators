with Iterators.Imperative.Iterators;

generic
   with package From is new Imperative.Iterators (<>);
   with package Into is new Imperative.Iterators (<>);
package Iterators.Imperative.Operators with Preelaborate is

   type Operator is limited new Into.Iterable with private;

   overriding
   function Iterate (This : in out Operator) return Into.Root.Iterator'Class is
     (Into.Iterable'Class (This).Iterate);

   procedure Map (This : in out Operator;
                  Prev : From.Iterable'Class;
                  Map  : not null access
                    function (E : From.Any_Element) return Into.Any_Element)
   is null;

private

   type Operator is limited new Into.Iterable with null record;

end Iterators.Imperative.Operators;
