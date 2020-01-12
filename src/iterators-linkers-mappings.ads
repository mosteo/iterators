with Iterators.Collectors.Mappings;
with Iterators.Keyed;
with Iterators.Root.Operators;

generic
   with package Root is new Iterators.Root (<>); -- Bug WA, don't remove
   with package Keyed is new Iterators.Keyed
     (Unkeyed => Root, others => <>);
   with package Operators is new Root.Operators (<>);
   with package Collectors is new Iterators.Collectors.Mappings
     (Keyed_Iterators => Keyed, others => <>);
package Iterators.Linkers.Mappings with Preelaborate is

--     package Root renames Keyed.Unkeyed;

   --  Plain sequences

   function "&" (L : Root.Iterable'Class;
                 R : Root.Any_Element) return Root.Iterable'Class
                 renames Operators.Append;
--
   function "&" (L : Root.Iterable'Class;
                 R : Operators.Counter) return Natural
                 renames Operators.Count;

   function "&" (L : Root.Iterable'Class;
                 R : Root.List) return Root.List
                 renames Operators.Collect;

   function "&" (L : Root.Iterable'Class;
                 R : Operators.Operator'Class) return Root.Iterable'Class
                 renames Operators.Operators.Concatenate;

--     --  Keyed sequences

   function "&" (L : Keyed.Iterable'Class;
                 R : Keyed.Any_Element) return Keyed.Iterable'Class
                 renames Keyed.Operators.Append;

   function "&" (L : Keyed.Iterable'Class;
                 R : Keyed.Operators.Counter) return Natural
                 renames Keyed.Operators.Count;
--
   function "&" (L : Keyed.Iterable'Class;
                 R : Keyed.Iterators.List) return Keyed.Iterators.List
                 renames Keyed.Operators.Collect;

   function "&" (L : Keyed.Iterable'Class;
                 R : Keyed.Operators.Operator'Class)
                 return Keyed.Iterators.Iterable'Class
                 renames Keyed.Operators.Operators.Concatenate;

   --  Collectors

   function "&" (L : Keyed.Iterator'Class;
                 R : Collectors.Containers.Container_Bug)
                 return Collectors.Containers.Container_Bug
                 renames Collectors.Collect;

end Iterators.Linkers.Mappings;
