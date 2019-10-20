with Iterators.Collectors.Mappings;
with Iterators.Keyed;

generic
   with package Keyed is new Iterators.Keyed (<>);
   with package Collectors is new Iterators.Collectors.Mappings
     (Keyed_Iterators => Keyed, others => <>);
package Iterators.Linkers.Mappings with Preelaborate is

   package Root renames Keyed.Unkeyed;

   --  Plain sequences

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

   --  Keyed sequences

   function "&" (L : Keyed.Iterator'Class;
                 R : Keyed.Any_Element) return Keyed.Iterator'Class
                 renames Keyed.Iterators."&";

   function "&" (L : Keyed.Iterator'Class;
                 R : Keyed.Iterators.Counter) return Natural
                 renames Keyed.Iterators."&";

   function "&" (L : Keyed.Iterator'Class;
                 R : Keyed.Iterators.List) return Keyed.Iterators.List
                 renames Keyed.Iterators."&";

   function "&" (L : Keyed.Iterator'Class;
                 R : Keyed.Iterators.Operator'Class) return Keyed.Iterators.Iterator'Class
                 renames Keyed.Iterators."&";

   --  Keyed collectors

   function "&" (L : Keyed.Iterator'Class;
                 R : Collectors.Containers.Container_Bis)
                 return Collectors.Containers.Container_Bis
                 renames Collectors."&";

end Iterators.Linkers.Mappings;
