package body Iterators.Traits.Containers.Appendable is

   ----------
   -- Copy --
   ----------

   procedure Copy (Dst : in out Container;
                   Src :        Container)
   is
      Pos : Containers.Cursor := First (Src);
   begin
      while Has_Element (Pos) loop
         Appendable.Append (Dst, Constant_Reference (Src, Pos).Element.all, 1);
         Pos := Next (Pos);
      end loop;
   end Copy;

end Iterators.Traits.Containers.Appendable;
