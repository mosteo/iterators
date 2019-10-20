with Iterators.Keyed;
with Iterators.Traits.Containers.Keyed;

generic
   with package Keyed_Iterators is new Iterators.Keyed (<>);
   with package Containers is new Iterators.Traits.Containers
     (Element_Type => Keyed_Iterators.Unkeyed.Any_Element, others => <>);
   with package Keyed_Containers is new Containers.Keyed
     (Keys => Keyed_Iterators.Keys, others => <>);
package Iterators.Collectors.Mappings with Preelaborate is

   --  Collect into a container that respects the appending order.

   function "&" (L : Keyed_Iterators.Iterator'Class;
                 R : Containers.Container)
                    return Containers.Container;
   --  To be used with Collect.
   --  Alternatively, if R is not empty, L and then R will be collected.
   --  This version uses "Insert", hence repeated keys will raise.

   function Collect return Containers.Container;
   --  Returns an empty container, to have a unique signature for the previous
   --  "&" operator.

   function Collect (L                 : Keyed_Iterators.Iterator'Class;
                     R                 : Containers.Container := Collect;
                     Allow_Replacement : Boolean := False)
                     return Containers.Container;
   --  Alternate regular function version. Allow_Replacement determines if
   --  Insert/Include is used during collection.

end Iterators.Collectors.Mappings;
