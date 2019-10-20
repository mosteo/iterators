package body Iterators.From.Vectors is

   ----------------------------
   -- Cant_Insert_Or_Include --
   ----------------------------

   procedure Cant_Insert_Or_Include (C : in out Container;
                                        I : Ada_Containers.Index_Type;
                                     E : Ada_Containers.Element_Type) is
   begin
      raise Constraint_Error with "Operation not allowed for vectors";
   end Cant_Insert_Or_Include;

end Iterators.From.Vectors;
