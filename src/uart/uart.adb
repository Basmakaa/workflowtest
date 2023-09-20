with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.IO_Exceptions;
with Ada.Streams;

package body Uart is

   function Read (
      Number_Of_Samples : Integer;
      Port_Location : GNAT.Serial_Communications.Port_Name
      ) return Readings_Array is

         --  Initialize the variables for the read
         Port : GNAT.Serial_Communications.Serial_Port;
         Buffer : Ada.Streams.Stream_Element_Array (1 .. 1);
         Offset : Ada.Streams.Stream_Element_Offset := 1;

         --  For storing one reading
         Line : String (1 .. 16);
         Line_Index : Natural := Line'First;
         Char : Character;

         --  To count the number of character readings done
         Counter : Integer := 1;

         --  For storing all the readings
         Readings : Uart.Readings_Array (1 .. Number_Of_Samples);
   begin

      --  We initialize the string to eliminate warnings
      for I in Line'Range loop
         Line (I) := '0';
      end loop;

      GNAT.Serial_Communications.Open
        (Port => Port,
         Name => Port_Location);
      GNAT.Serial_Communications.Set
        (Port => Port,
         Rate => GNAT.Serial_Communications.B115200);

      --  Make sure to only start collecting data at start of new line
      loop
         GNAT.Serial_Communications.Read (Port, Buffer, Offset);
         exit when Character'Val (Buffer (1)) = ASCII.LF;
      end loop;

      --  Run until the we gathered the required number of samples
      while Counter < Number_Of_Samples + 1 loop
         begin

            GNAT.Serial_Communications.Read (Port, Buffer, Offset);

            --  Store the reading in the Char variable
            Char := Character'Val (Buffer (1));

            --  If we read the end of the line
            --  and we are not a the beginning of a line
            if Char = ASCII.LF and then Line_Index /= 1 then

               --  We save the reading to an array
               Ada.Float_Text_IO.Get
                 (From => Line (1 .. Line_Index - 1),
                  Item => Readings (Counter),
                  Last => Line_Index);

               --  We reset the line index and increment the counter
               Line_Index := Line'First;
               Counter := Counter + 1;

            --  If we are not a the end of the line
            elsif Char /= ASCII.LF then

               --  We write the current character to
               --  the current index of our line and increment the line
               Line (Line_Index) := Char;
               Line_Index := Line_Index + 1;
            end if;
         exception
            when Ada.IO_Exceptions.Data_Error =>
               Put_Line ("Data error");
               Line_Index := Line_Index - 1;
         end;
      end loop;
      GNAT.Serial_Communications.Close (Port);
      return Readings;
   end Read;
end Uart;
