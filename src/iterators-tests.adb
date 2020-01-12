with Ada.Text_IO;

package body Iterators.Tests is

   procedure Check (It : Ints.Iterators.Iterator'Class;
                    Ok : Number_Array)
     --  Ensure that all items in the iterator match expected values
   is
      use Ada.Text_IO;
      Pos : Positive := Ok'First;
      Dbg : constant Boolean := False;
   begin
      if Dbg then
         Put_Line ("---");
      end if;

      for Int of It loop
         if Dbg then
            Put_Line ("Next:" & Int'Img);
         end if;

         if Pos > Ok'Last then
            raise Constraint_Error with
              "More elements in sequence than expected, index:" & Pos'Img
              & "; Item:" & Int'Img;
         end if;

         if Int /= Ok (Pos) then
            raise Constraint_Error with
              "Expected:" & Ok (Pos)'Img & "; Got:" & Int'Img;
         end if;

         Pos := Pos + 1;
      end loop;

      if Pos /= Ok'Last + 1 then
         raise Constraint_Error with
           "Fewer elements in sequence than expected, index:" & Pos'Img
           & "; Ok'Length =" & Ok'Length'Img;
      end if;
   end Check;

end Iterators.Tests;
