with Containers.Hashed_Maps;
with Ada.Text_IO; use Ada.Text_IO;
procedure Test_Maps is
   function Hash (Key : Natural) return Containers.Hash_Type
   is (Containers.Hash_Type (Key));
   package My_Maps is new Containers.Hashed_Maps
     (Key_Type     => Natural,
      Element_Type => Integer,
      Hash         => Hash);
   use My_Maps;
   M : Map (Capacity => 6, Modulus => 0);
   C : Cursor;
   procedure Report (Message : String) is
   begin
      Put_Line ("Report for " & Message);
      for K of M loop
         Put_Line (K'Image & " => " & Element (M, K)'Image);
      end loop;
      Put_Line ("--------");
      New_Line;
   end Report;
begin
   Report ("initial");
   for J in 0 .. 3 loop
      Insert (Container => M, Key => J, New_Item => -J);
   end loop;
   Report ("setup");
   Insert (Container => M, Key => 42, New_Item => -42);
   Report ("added another");
   C := Find (M, 3);
   Delete (M, Key => 0);
   Delete (M, Position => C);
   Report ("deleted 0 and 3");
end Test_Maps;
