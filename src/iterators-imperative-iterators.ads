with Iterators.Root;

generic
   with package Root is new Standard.Iterators.Root (<>);
package Iterators.Imperative.Iterators with Preelaborate is

   subtype Any_Element is Root.Any_Element;

   --------------------
   -- Iterable trait --
   --------------------

   type Iterable is limited interface;

   function Iterate (This : Iterable) return Root.Iterator'Class is abstract;

   -------------------------
   -- Imperative iterator --
   -------------------------

   type Iterator is limited new Iterable with private;

   procedure Start (This : in out Iterator;
                    From : Root.Iterator'Class) is null;

   procedure Resume (This : in out Iterator;
                     From : Iterable'Class) is null;

   ---------------
   -- Iteration --
   ---------------

   function Iterate (This : Iterator) return Root.Iterator'Class is
      (raise Unimplemented);
   --  Go back into root iterator class, which accepts "of"

   ---------------
   -- Operators --
   ---------------

   procedure Copy (This : in out Iterator) is null;

   procedure Filter
     (This   : in out Iterator;
      Tester : access function (Element : Any_Element) return Boolean) is null;

   procedure Map (This : in out Iterator;
                  Map  : not null access
                    function (E : Any_Element) return Any_Element) is null;

private

   type Iterator is limited new Iterable with null record;

end Iterators.Imperative.Iterators;
