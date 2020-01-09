package Iterators.Collectors with Preelaborate is

   --  Root for facilities to collect iterator elements into a container.

   --  These collectors differ from other implementations in that, to maintain
   --  consistency with the rest of operators, they won't consume the source
   --  iterator, even if there are no intermediate operators (operators always
   --  make a copy of the upstream iterator, so they never consume the source).
   --  The only current possibility to consume for good an iterator is to
   --  manually call Next on it.

end Iterators.Collectors;
