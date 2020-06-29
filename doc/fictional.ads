generic
   type Any_Element (<>) is private;
package Iterators is

   type Iterator is tagged private;

   --  Regular primitives omitted

   generic
      type Target (<>) is private;
   tagged package Op for Iterator is
      --  This new package declaration syntax means that this package can be
      --  implicitly instantiated as a "subprogram" of Operator.

      package Into is new Iterators (Target);
      --  Note that this is illegal in current Ada, because a package cannot
      --  be instantiated within itself. However, since at Into instantiation
      --  time, the parent Iterators must already exist, perhaps should be
      --  doable by a two-pass compiler. Otherwise, these packages with Primitive
      --  aspect might obey different rules or use a different keyword.

      procedure Map () return Into.Iterator'Class;

      package Meta is new Iterators (Iterator'Class);

      procedure Flat_Map (Parent : Meta.Iterator) return Iterator'Class

      procedure Window (Parent : Iterator) return Meta.Iterator'Class;


   end More_Ops

end Ops
