with Iterators.Root.Operators;

package body Iterators.Imperative.Iterators is

   package Operators is new Root.Operators;

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

end Iterators.Imperative.Iterators;
