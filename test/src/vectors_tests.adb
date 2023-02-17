with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Containers.Vectors;

package body Vectors_Tests is

   package Tests is
      type T is new Test_Case with null record;
      overriding function Name (C : T) return AUnit.Message_String;
      overriding procedure Register_Tests (C : in out T);
      --  procedure Set_Up (C : in out T);
      --  procedure Tear_Down (C : in out t);
   end Tests;

   use AUnit;

   package body Tests is
      overriding function Name (C : T) return AUnit.Message_String
        is (Format ("Vectors"));

      use Containers; -- for Count_Type

      subtype Index_Type is Integer range 10 .. 14;

      package Vectors_For_Test is new Containers.Vectors
        (Index_Type   => Index_Type,
         Element_Type => Integer);
      use Vectors_For_Test;

      procedure Initial (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
         use type Containers.Count_Type;
      begin
         Assert (Length (V) = 0, "new vector has non-zero length");
      end Initial;

      procedure Add_5 (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in 1 .. 5 loop
            Append (V, J);
            Assert (Length (V) = Count_Type (J), "vector has wrong length");
         end loop;
      end Add_5;

      procedure Too_Many (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 4);
      begin
         for J in 1 .. 5 loop
            Append (V, J);
            Assert (Length (V) = Count_Type (J), "vector has wrong length");
         end loop;
         Assert (False, "should have raised Capacity_Error");
      exception
         when Capacity_Error => null;
      end Too_Many;

      procedure Values (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in 1 .. 5 loop
            Append (V, -J);
            Assert (Length (V) = Count_Type (J), "vector has wrong length");
            --  The index of the fist element is 10
            Assert (Element (V, J + 9) = -J, "element has wrong value (a)");
         end loop;
         for J in V loop
            --  The index of the fist element is 10
            Assert (Element (V, J) = -(J - 9), "element has wrong value (b)");
         end loop;
         declare
            J : Integer := 0;
         begin
            for Value of V loop
               J := J + 1;
               pragma Assert (Value = -J, "element has wrong value (c)");
            end loop;
         end;
      end Values;

      --  XXX more to come!

      overriding procedure Register_Tests (C : in out T)
      is
      begin
         Registration.Register_Routine
           (C, Initial'Access, "initial");
         Registration.Register_Routine
           (C, Add_5'Access, "add 5");
         Registration.Register_Routine
           (C, Too_Many'Access, "add too many");
         Registration.Register_Routine
           (C, Values'Access, "values");
      end Register_Tests;

   end Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Tests.T);

      return Result;
   end Suite;

end Vectors_Tests;
