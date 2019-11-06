with Iterators.Generators.Arrays;

procedure Iterators.Tests.Arrays is

--     type Positive_Array is array (Positive range <>) of aliased Integer;

   package Arrs is new Generators.Arrays (Integer, Positive);

   RO : constant array (1 .. 3) of Integer := (others => 777);
   RW : array (1 .. 3) of aliased Integer;

begin
   null;
end Iterators.Tests.Arrays;
