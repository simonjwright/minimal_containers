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
      overriding function Name (C : T) return AUnit.Message_String is
        (Format ("Maps"));

      use Ada.Containers; -- for Count_Type, Hash_Type, Capacity_Error

      subtype Key_Type is Positive;
      subtype Ch is Character;

      function Hash (Key : Key_Type) return Hash_Type is (Hash_Type (Key));

      package Maps_For_Test is new Minimal_Containers.Bounded_Hashed_Maps
        (Key_Type        => Key_Type,
         Element_Type    => Ch,
         Hash            => Hash,
         Equivalent_Keys => "=");
      use Maps_For_Test;

      procedure Initial (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         Assert (Length (M) = 0, "new map has non-zero length");
         Assert (Is_Empty (M), "new map is not empty");
      end Initial;

      procedure Add_5 (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
            Assert (not Is_Empty (M), "map is empty");
            Assert (Length (M) = Count_Type (J), "map has wrong length");
         end loop;
      end Add_5;

      procedure Clearing (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 4, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
         end loop;
         M.Clear;
         Assert (M.Is_Empty, "cleared map isn't empty");
         Assert (M.Length = 0, "cleared map has non-zero length");
         --  Add another 5
            for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
            Assert (not Is_Empty (M), "map is empty");
            Assert (Length (M) = Count_Type (J), "map has wrong length");
         end loop;
      exception
         when Capacity_Error =>
            null;
      end Clearing;

      procedure Too_Many (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 4, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
            Assert (Length (M) = Count_Type (J), "map has wrong length");
         end loop;
         Assert (False, "should have raised Capacity_Error");
      exception
         when Capacity_Error =>
            null;
      end Too_Many;

      procedure Values (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
            Assert (Length (M) = Count_Type (J), "map has wrong length");
            Assert
              (Element (M, J) = Ch'Val (J), "element has wrong value (a)");
         end loop;
         --  for .. in
            declare
            Count : Positive := 1;
         begin
            for C in M.Iterate loop
               Assert
                 (Element (C) = Ch'Val (Count), "element has wrong value (b)");
               Count := Count + 1;
            end loop;
         end;
         --  for .. of
            declare
            Count : Integer := 1;
         begin
            for C of M loop
               pragma Assert
                 (C = Ch'Val (Count), "element has wrong value (c)");
               Count := Count + 1;
            end loop;
         end;
      end Values;

      procedure Containing (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J));
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
            Insert (M, J, Ch'Val (J));
         end loop;
         declare
            Unused : Ch;
         begin
            Unused := Element (M, 5);
            Assert (False, "should have raised Constraint_Error");
         exception
            when Constraint_Error =>
               null;
         end;
      end Out_Of_Range;

      procedure Tampering (Unused : in out AUnit.Test_Cases.Test_Case'Class) is
         M : Map (Capacity => 5, Modulus => 42);
      begin
         for J in 1 .. 5 loop
            Insert (M, J, Ch'Val (J + Character'Pos ('a') - 1));
         end loop;
         Assert (M.Element (1) = 'a', "wrong first element");
         Assert (M.Element (5) = 'e', "wrong last element");
         begin
            for K in M.Iterate loop
               if Element (K) = 'c' then
                  declare
                     K_Copy : Maps_For_Test.Cursor := K;
                  begin
                     M.Delete (K_Copy);
                  end;
               end if;
            end loop;
            Assert (False, "tampering check should have failed");
         exception
            when Program_Error =>
               null;
         end;
      end Tampering;

      --  XXX more to come!

      overriding procedure Register_Tests (C : in out T) is
      begin
         Registration.Register_Routine (C, Initial'Access, "initial");
         Registration.Register_Routine (C, Add_5'Access, "add 5");
         Registration.Register_Routine (C, Clearing'Access, "clearing");
         Registration.Register_Routine (C, Too_Many'Access, "add too many");
         Registration.Register_Routine (C, Values'Access, "values, loops");
         Registration.Register_Routine (C, Containing'Access, "contains");
         Registration.Register_Routine
           (C, Out_Of_Range'Access, "out-of-range access");
         Registration.Register_Routine
           (C, Tampering'Access, "tampering detected");
      end Register_Tests;

   end Tests;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite :=
        new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Tests.T);

      return Result;
   end Suite;

end Maps_Tests;
