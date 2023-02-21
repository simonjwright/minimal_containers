with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;

with Ada.Containers;
with Minimal_Containers.Bounded_Hashed_Maps;

package body Maps_Tests is

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
        is (Format ("Maps"));

      use Ada.Containers; -- for Count_Type, Hash_Type, Capacity_Error

      subtype Key_Type is Positive;

      function Hash (Key : Key_Type) return Hash_Type is (Hash_Type (Key));

      package Maps_For_Test is new Minimal_Containers.Bounded_Hashed_Maps
        (Key_Type     => Key_Type,
         Element_Type => Integer,
         Hash         => Hash);
      use Maps_For_Test;

      procedure Initial (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         Assert (Length (M) = 0, "new vector has non-zero length");
      end Initial;

      procedure Add_5 (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, -J);
            Assert (Length (M) = Count_Type (J), "vector has wrong length");
         end loop;
      end Add_5;

      procedure Too_Many (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 4, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, -J);
            Assert (Length (M) = Count_Type (J), "vector has wrong length");
         end loop;
         Assert (False, "should have raised Capacity_Error");
      exception
         when Capacity_Error => null;
      end Too_Many;

      procedure Values (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, -J);
            Assert (Length (M) = Count_Type (J), "vector has wrong length");
            Assert (Element (M, J) = -J, "element has wrong value (a)");
         end loop;
         declare
            Count : Positive := 1;
         begin
            for J in M loop
               Assert (Element (M, J) = -Count, "element has wrong value (b)");
               Count := Count + 1;
            end loop;
         end;
         declare
            J : Integer := 0;
         begin
            for Value of M loop
               J := J + 1;
               pragma Assert (Value = -J, "element has wrong value (c)");
            end loop;
         end;
      end Values;

      procedure Containing (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, -J);
         end loop;
         declare
            Count : Positive := 1;
         begin
            for J in 1 .. 5 loop
               Assert (Contains (M, J), "element not found");
               Count := Count + 1;
            end loop;
         end;
         Assert (not Contains (M, 42), "missing element found");
      end Containing;

      procedure Out_Of_Range (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 4 loop
            Insert (M, J, -J);
         end loop;
         declare
            Unused : Integer := 0;
         begin
            Unused := Element (M, 5);
            Assert (False, "should have raised Constraint_Error");
         exception
            when Constraint_Error => null;
         end;
      end Out_Of_Range;

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
           (C, Values'Access, "values, loops");
         Registration.Register_Routine
           (C, Containing'Access, "contains");
         Registration.Register_Routine
           (C, Out_Of_Range'Access, "out-of-range access");
      end Register_Tests;

   end Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Tests.T);

      return Result;
   end Suite;

end Maps_Tests;
