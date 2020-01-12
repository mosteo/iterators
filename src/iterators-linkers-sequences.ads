with Iterators.Collectors.Sequences;
with Iterators.Root.Operators;

generic
   with package Root is new Iterators.Root (<>);
   with package Operators is new Root.Operators (<>);
   with package Collectors is new Iterators.Collectors.Sequences
     (Root   => Root,
      others => <>);
package Iterators.Linkers.Sequences with Preelaborate is

   subtype Container is Collectors.Containers.Container_Bug;
   subtype Iterable is Root.Iterable;
   subtype Iterator is Root.Iterator;
   subtype Operator is Operators.Operator;

   function "&" (L : Iterable'Class;
                 R : Root.Any_Element) return Iterable'Class
                 renames Operators.Append;

   function "&" (L : Iterable'Class;
                 R : Operators.Counter) return Natural
                 renames Operators.Count;

   function "&" (L : Iterable'Class;
                 R : Root.List) return Root.List
                 renames Operators.Collect;

   function "&" (L : Iterable'Class;
                 R : Operator'Class) return Iterable'Class
                 renames Operators.Operators.Concatenate;

   function "&" (L : Iterator'Class;
                 R : Container)
                 return Container
                 renames Collectors.Collect;

end Iterators.Linkers.Sequences;
