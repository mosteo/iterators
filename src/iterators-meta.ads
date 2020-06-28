with Iterators.Operators;
with Iterators.Root.Operators;

generic
   with package Base_Root is new Iterators.Root (<>);
   with package Base_Operators is new Base_Root.Operators  (<>);
package Iterators.Meta with Preelaborate is

   --  Iterators of iterators. Created by a few specific operators like Buffer,
   --  Window...

   package Meta_Root is new Iterators.Root (Base_Root.Iterator'Class);

   package Iter2meta is new Operators (Base_Root, Meta_Root);
   package Meta2iter is new Operators (Meta_Root, Base_Root);

   subtype Meta_Operator is Iter2meta.Operator;

   function Flat_Map (Map : not null access
                       function (Iter : Base_Root.Iterator'Class)
                                 return Base_Root.Any_Element)
                      return Meta2iter.Operator'Class;
   --  Flatten a sequence of iterators, by applying a function to each
   --  iterator and making the result available as part of a single iterator
   --  of elements. For example, it can flatten a sequence created with Window.

   --  Other possible signatures for this flatmap: instead of function
   --  returning a reduced element from upstream Iter, return a base iterator
   --  (so more elements than one can be obtained from each meta_iter). In this
   --  case, a default value for the function could be provided, which just
   --  lets through all elements.

   function Window (Size : Positive;
                    Skip : Natural := 0)
                    return Meta_Operator'Class;
   --  Group N=Size items and generate an iterator for these items. Move
   --  forward M=Skip items on the original sequence and repeat. When Skip =
   --  0, Size will be used instead.

   package Linking is

      function "&" (L : Base_Root.Iterator'Class;
                    R : Meta_Operator'Class)
                    return Meta_Root.Iterator'Class
                    renames Iter2meta.Concatenate;

      function "&" (L : Meta_Root.Iterator'Class;
                    R : Meta2iter.Operator'Class)
                    return Base_Root.Iterator'Class
                    renames Meta2iter.Concatenate;

   end Linking;

end Iterators.Meta;
