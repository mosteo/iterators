with Iterators.Meta.Impl_Flatmap;
with Iterators.Meta.Impl_Window;

package body Iterators.Meta is

   --------------
   -- Flat_Map --
   --------------

   package Flatmap_Instance is new Impl_Flatmap;

   function Flat_Map (Map : Base_Root.Iterator'Class := Base_Operators.No_Op)
                      return Meta2iter.Operator'Class
                      renames Flatmap_Instance.Create;

   procedure Flat_Map (This : in out Chain_From_Meta;
                       Map  : Base_Root.Iterator'Class := Base_Operators.No_Op)
   is
   begin
      This.Continue (Flat_Map (Map));
   end Flat_Map;

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

   procedure Window (This : in out Chain_To_Meta;
                     Size : Positive;
                     Skip : Natural := 0)
   is
   begin
      This.Continue (Window (Size, Skip));
   end Window;

end Iterators.Meta;
