generic
   type Keys (<>) is private;

   with function Key
     (Pos : Containers.Cursor) return Keys with Warnings => Off;

   with procedure Insert
     (Container : in out Containers.Container;
      Key       : Keys;
      New_Item  : Containers.Element_Type_Bug) with Warnings => Off;

   with procedure Include
     (Container : in out Containers.Container;
      Key       : Keys;
      New_Item  : Containers.Element_Type_Bug) with Warnings => Off;
package Iterators.Traits.Containers.Keyed with Preelaborate is

   --  Extension of the Container trait, to represent that elements have an
   --  associated key (e.g., index in vectors, key in maps).

end Iterators.Traits.Containers.Keyed;
