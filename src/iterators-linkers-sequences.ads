with Iterators.Collectors.Sequences;
with Iterators.Root;

generic
   with package Root is new Iterators.Root (<>);
   with package Collectors is new Iterators.Collectors.Sequences
     (Root => Root, others => <>);
package Iterators.Linkers.Sequences with Preelaborate is

   function "&" (L : Root.Iterator'Class;
                 R : Root.Any_Element) return Root.Iterator'Class
                 renames Root."&";

   function "&" (L : Root.Iterator'Class;
                 R : Root.Counter) return Natural
                 renames Root."&";

   function "&" (L : Root.Iterator'Class;
                 R : Root.List) return Root.List
                 renames Root."&";

   function "&" (L : Root.Iterator'Class;
                 R : Root.Operator'Class) return Root.Iterator'Class
                 renames Root."&";

   function "&" (L : Root.Iterator'Class;
                 R : Collectors.Containers_Bis.Container_Bis)
                 return Collectors.Containers_Bis.Container_Bis
                 renames Collectors."&";

end Iterators.Linkers.Sequences;
