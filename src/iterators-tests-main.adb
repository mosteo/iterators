with Ada.Text_IO;

with Iterators.Tests.Imperative;
with Iterators.Tests.Maps;
with Iterators.Tests.Vectors;

procedure Iterators.Tests.Main is
begin
   Tests.Imperative;
   Tests.Maps;
   Tests.Vectors;

   Ada.Text_IO.Put_Line ("OK");

end Iterators.Tests.Main;
