[![Build](https://github.com/mosteo/iterators/workflows/CI%20linux/badge.svg)](https://github.com/mosteo/iterators/actions?workflow=CI+linux)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/iterators.json)](https://alire.ada.dev/crates/iterators.html)

# Iterators
Chainable functional iterators for Ada 2012. Requires GNAT Community 2020 or GNAT FSF 10.

Clone with submodules (`git clone URL --init`). Compile with `gprbuild -p -P iterators_dev`. Compilation takes a long (long) time due
to the heavy use of generics.

Alternatively, build with `alr build`.

Check the tests in `src/iterators-tests-*` for more examples of use.

```Ada
--  Collection:
declare
   Result : constant List :=
              Iter (Some_Collection)
              & Filter (Some_Condition'Access)
              & Map (Some_Transformation'Access)
              & Collect;
begin
   null;
end;

--  Iteration without loops:
For_Each (Iter (Some_Collection)
          & Filter (Some_Condition'Access)
          & Map (Some_Transformation'Access),
          Some_Processing'Access);

--  Equivalent iteration with plain loops:
for Element of Iterator'Class'
               (Iter (Some_Collection)
                & Filter (Some_Condition'Access)
                & Map (Some_Transformation'Access))
loop
   Some_Processing (Element);
end loop

--  Type transformations:
declare
   package Ints is new Iterators.From.Elements (Integer);
   package Strs is new Iterators.From.Elements (String);
   package Int2Str is new Iterators.Operators (Ints.Iterators, 
                                               Strs.Iterators);

   use Ints.Linking, Strs.Linking, Int2Str.Linking; -- Make "&" visible

   Result : constant Ints.Iterators.List :=
              Ints.Iter (Some_Collection_Of_Integers)
              & Ints.Op.Filter (Some_Condition'Access)
              & Int2Str.Op.Map (Some_Integer_String_Transform'Access)
              & Strs.Op.Collect;
begin
   null;
end;
```
