package body Data_structures is

   function Set_To_Zero return Data_Points is
      Zero_Array : Data_Points;
   begin
      for I in Zero_Array'Range loop
         Zero_Array(I) := 0.0;
      end loop;
      return Zero_Array;
   end Set_To_Zero;

   function Get_X_Axis return Data_Points is
      X_Axis_Array : Data_Points;
      x : Float := 0.0;
      Step : Float := Float (1.0/Buffer_Max_Length);
   begin 
      for I in 0..Integer(Buffer_Max_Length)  loop
         X_Axis_Array(I) := X;
         X := X + Step;
      end loop;
      return X_Axis_Array;
   end Get_X_Axis;

end Data_structures;
