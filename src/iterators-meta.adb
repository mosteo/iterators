with Iterators.Meta.Impl_Flatmap;
with Iterators.Meta.Impl_Window;

package body Iterators.Meta is

   --------------
   -- Flat_Map --
   --------------

   package Flatmap_Instance is new Impl_Flatmap;

   function Flat_Map (Map : not null access
                        function (Iter : Base_Root.Iterator'Class)
                                  return Base_Root.Any_Element)
                      return Meta2iter.Operator'Class
                      renames Flatmap_Instance.Create;

   ------------
   -- Window --
   ------------

   package Window_Instance is new Impl_Window;

   function Window (Size : Positive;
                    Skip : Natural := 0)
                    return Meta_Operator'Class
   is (Window_Instance.Create (Size,
                               (if Skip = 0
                                then Size
                                else Skip)));

end Iterators.Meta;
