with Ada.Text_IO; use Ada.Text_IO;

package Data_structures is

   Buffer_Max_Length : constant Integer := 200;

   --  type Buffer_Length is range 0 .. Buffer_Max_Length;
   type Data_Points is
     array (0 .. Buffer_Max_Length) of Float;


   function Set_To_Zero return Data_Points;
   function Get_X_Axis return Data_Points;

--
-- Buffer type
--
   type Buffer is record
      Data     : Data_Points;
      IsFull      : Boolean;
   end record;

end Data_structures;
