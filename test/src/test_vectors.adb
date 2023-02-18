with Minimal_Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Vectors
is
   type Index_Type is range 10 .. 20;
   package My_Vectors is new Minimal_Containers.Vectors (Index_Type, Integer);
   use My_Vectors;
   V : Vector (5);
   procedure Report (Message : String) is
   begin
      Put_Line ("Report for " & Message);
      for I of V loop
         Put_Line (I'Image);
      end loop;
      for J in V loop
         Put_Line (J'Image & " =>" & Element (V, J)'Image);
      end loop;
      Put_Line ("--------");
      New_Line;
   end Report;
begin
   for J in -4 .. -1 loop
      Append (V, J);
   end loop;
   Report ("initial setup");
   Append (V, 42);
   Report ("appended 42");
   Delete (V, 11);
   Report ("deleted index 11 (entry #2)");
end Test_Vectors;
