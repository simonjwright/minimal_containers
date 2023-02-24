with Minimal_Containers.Bounded_Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Vectors
is
   type Extended_Index_Type is range 9 .. 21;
   subtype Index_Type is Extended_Index_Type range 10 .. 20;
   package My_Vectors
   is new Minimal_Containers.Bounded_Vectors (Index_Type, Integer);
   use My_Vectors;
   V : Vector (5);
   procedure Report (Message : String) is
      Count : Extended_Index_Type;
   begin
      Put_Line ("Report for " & Message);
      Put_Line ("for .. of");
      Put_Line ("--------");
      for I of V loop
         Put_Line (I'Image);
      end loop;
      Put_Line ("--------");
      Put_Line ("for .. of reverse");
      Put_Line ("--------");
      for I of reverse V loop
         Put_Line (I'Image);
      end loop;
      Put_Line ("--------");
      Put_Line ("for .. in");
      Put_Line ("--------");
      Count := Index_Type'First;
      for J in V.Iterate loop
         Put_Line (Count'Image & " =>" & Element (J)'Image);
         Count := Extended_Index_Type'Succ (Count);
      end loop;
      Put_Line ("--------");
      Put_Line ("for .. in reverse");
      Put_Line ("--------");
      Count := Index_Type (Integer (Length (V)) + 9);
      for J in reverse V.Iterate loop
         Put_Line (Count'Image & " =>" & Element (J)'Image);
         Count := Extended_Index_Type'Pred (Count);
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
