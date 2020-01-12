with Ada.Text_IO;

with Iterators.Tests.Arrays;
with Iterators.Tests.Imperative;
with Iterators.Tests.Maps;
with Iterators.Tests.Operators;
with Iterators.Tests.Vectors;

procedure Iterators.Tests.Main is
begin
   Tests.Arrays;
   Tests.Imperative;
   Tests.Maps;
   Tests.Operators;
   Tests.Vectors;

   Ada.Text_IO.Put_Line ("OK");

end Iterators.Tests.Main;
