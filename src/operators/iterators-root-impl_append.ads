private generic package Iterators.Root.Impl_Append with Preelaborate is

   function Create (Element : Any_Element) return Iterator'Class;

   function "&" (L : Iterator'Class; R : Any_Element) return Iterator'Class;

end Iterators.Root.Impl_Append;
