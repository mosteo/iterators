with Iterators.Transform;

generic
package Iterators.Root.Preservers with Preelaborate is

   package Operators is new Transform (Root, Root);

end Iterators.Root.Preservers;
