private generic package Iterators.Root.Impl_Filter with Preelaborate is

   type Testers is access function (Element : Any_Element) return Boolean;

   function Create
     (Tester : Testers)
      return Iterator'Class;

end Iterators.Root.Impl_Filter;
