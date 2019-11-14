with Iterators.Root.Operators;

package body Iterators.Imperative.Iterators is

   package Operators is new Root.Operators;

   ----------
   -- Copy --
   ----------

   procedure Copy (This : in out Iterator) is
   begin
      This.Link (Operators.Copy);
   end Copy;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (This   : in out Iterator;
      Tester : access function (Element : Any_Element) return Boolean) is
   begin
      This.Link (Operators.Filter (Tester));
   end Filter;

   -------------
   -- Iterate --
   -------------

   function Iterate (This : in out Iterator) return Root.Iterator'Class is
     (This.Prev.As_Iterator);

   ----------
   -- Link --
   ----------

   procedure Link (This : in out Iterator;
                   Last :        Root.Iterator'Class)
   is
   begin
      if Last not in Operators.Operator'Class then
         raise Iterator_Error with "Attempting to link a non-operator";
      end if;

      declare
         New_Last : constant Root.Iterator'Class :=
                      Operators.Operators.Concatenate
                        (This.Prev.As_Iterator,
                         Operators.Operator'Class (Last));
      begin
         This.Prev.Hold (New_Last);
      end;
   end Link;

   -------------
   -- Restart --
   -------------

   procedure Restart (This : in out Iterator;
                      From : Root.Iterator'Class) is
   begin
      if This.Prev.Is_Empty then
         raise Iterator_Error with
           "Attempting to restart a unused iterator";
      else
         This.Prev.Hold (From);
      end if;
   end Restart;

   -----------
   -- Start --
   -----------

   procedure Start (This : in out Iterator;
                    From : Root.Iterator'Class) is
   begin
      if This.Prev.Is_Valid then
         raise Iterator_Error with
           "Attempting to start an already-started iterator";
      else
         This.Prev.Hold (From);
      end if;
   end Start;

end Iterators.Imperative.Iterators;
