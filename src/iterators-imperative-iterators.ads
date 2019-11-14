with Iterators.Root;

generic
   with package Root is new Standard.Iterators.Root (<>);
package Iterators.Imperative.Iterators with Preelaborate is

   subtype Any_Element is Root.Any_Element;

   --------------------
   -- Iterable trait --
   --------------------

   type Iterable is limited interface;

   function Iterate (This : in out Iterable) return Root.Iterator'Class is abstract;

   -------------------------
   -- Imperative iterator --
   -------------------------

   type Iterator is limited new Iterable with private;

   procedure Start (This : in out Iterator;
                    From : Root.Iterator'Class);
   --  Requires This to be unused

   procedure Restart (This : in out Iterator;
                      From : Root.Iterator'Class);
   --  Requires This to be already Started, previous From is forgotten

   procedure Resume (This : in out Iterator;
                     From : Iterable'Class) is null;
   --  Although seemingly differen, what we want is simply to continue
   --  consuming the From iterable, which may be a fresh one or not.

   ---------------
   -- Iteration --
   ---------------

   function Iterate (This : in out Iterator) return Root.Iterator'Class;
   --  Go back into root iterator class, which accepts "of"

   ---------------
   -- Operators --
   ---------------

   procedure Copy (This : in out Iterator);

   procedure Filter
     (This   : in out Iterator;
      Tester : access function (Element : Any_Element) return Boolean);

   procedure Map (This : in out Iterator;
                  Map  : not null access
                    function (E : Any_Element) return Any_Element) is null;

   procedure No_Op (This : in out Iterator) is null;

private

   type Iterator is limited new Iterable with record
      Prev : Root.Holder;
   end record;

   procedure Link (This : in out Iterator;
                   Last :        Root.Iterator'Class);

end Iterators.Imperative.Iterators;
