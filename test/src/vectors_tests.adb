with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Assertions;
with Ada.Containers;
with Minimal_Containers.Bounded_Vectors;

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

      use Ada.Containers; -- for Count_Type, Capacity_Error

      type Index_Base is range 0 .. 6;
      subtype Index_Type is Index_Base range 1 .. 5;

      type Element_Type is (K, L, M, N, P);

      Expected_Values : constant array (Index_Type) of Element_Type
        := (1 => K,
            2 => L,
            3 => M,
            4 => N,
            5 => P);

      Index_For : constant array (Element_Type) of Index_Type
        := (K => 1,
            L => 2,
            M => 3,
            N => 4,
            P => 5);

      package Vectors_For_Test is new Minimal_Containers.Bounded_Vectors
        (Index_Type   => Index_Type,
         Element_Type => Element_Type);
      use Vectors_For_Test;

      procedure Initial (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         Assert (Length (V) = 0, "new vector has non-zero length");
      end Initial;

      procedure Add_5 (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Element_Type loop
            Append (V, J);
         end loop;
         Assert (Length (V) = Capacity (V), "vector has wrong length");
      end Add_5;

      procedure Clearing (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
         C : Cursor;
         E : Element_Type;
      begin
         for J in Element_Type loop
            Append (V, J);
         end loop;
         C := V.First;
         V.Clear;
         Assert (V.Length = 0, "vector not cleared");
         E := Element (C);
         Assert (False, "can use cursor on cleared vector");
      exception
         when Constraint_Error =>
            null;
      end Clearing;

      procedure Too_Many (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 4);
      begin
         for J in Element_Type loop
            Append (V, J);
            Assert (Length (V) = Element_Type'Pos (J) + 1,
                    "vector has wrong length");
         end loop;
         Assert (False, "should have raised Capacity_Error");
      exception
         when Capacity_Error => null;
         when Ada.Assertions.Assertion_Error => null;
      end Too_Many;

      procedure Values (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
         Count : Natural;
      begin
         Count := 0;
         for J in Index_Type loop
            Append (V, Element_Type'Val (Count));
            Assert (Length (V) = Count_Type (Count + 1),
                    "vector has wrong length");
            Assert (Element (V, Index_Type (Count + 1))
                      = Element_Type'Val (Count),
                    "element has wrong value (a)");
            Count := Count + 1;
         end loop;
         Count := 0;
         for J in V.Iterate loop
            Assert (Element (Position => J) = Element_Type'Val (Count),
                    "element has wrong value (b)");
            Count := Count + 1;
         end loop;
         declare
            Count : Integer := 0;
         begin
            for Value of V loop
               Assert (Value = Element_Type'Val (Count),
                       "element has wrong value (c)");
               Count := Count + 1;
            end loop;
         end;
      end Values;

      procedure Finding_Index
        (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Index_Type range 1 .. 4 loop
            Append (V, Expected_Values (J));
            Assert (Length (V) = Index_Type'Pos (J),
                    "vector has wrong length");
            Assert (Element (V, J) = Expected_Values (J),
                    "element has wrong value (a)");
         end loop;
         for J in Element_Type range L .. N loop
            Assert (Find_Index (V, J) = Index_For (J),
                    "find_index found wrong index");
         end loop;
         Assert (Find_Index (V, P) = No_Index,
                 "find_index succeeded for missing element");
      end Finding_Index;

      procedure Out_Of_Range (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Element_Type range K .. N loop
            Append (V, J);
         end loop;
         declare
            Unused : Element_Type;
         begin
            Unused := Element (V, 5);
            Assert (False, "should have raised Constraint_Error");
         exception
            when Constraint_Error => null;
         end;
      end Out_Of_Range;

      procedure Tampering_Forward
        (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Element_Type range K .. P loop
            Append (V, J);
         end loop;
         for Cursor in V.Iterate loop
            if Element (Cursor) = M then
               declare
                  Cursor_Copy : Vectors_For_Test.Cursor := Cursor;
               begin
                  V.Delete (Cursor_Copy);
               end;
            end if;
         end loop;
         Assert (False, "tampering check (forward) should have failed");
      exception
         when Program_Error =>
            null;
      end Tampering_Forward;

      procedure Tampering_Reverse
        (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Element_Type range K .. P loop
            Append (V, J);
         end loop;
         for Cursor in reverse V.Iterate loop
            if Element (Cursor) = M then
               declare
                  Cursor_Copy : Vectors_For_Test.Cursor := Cursor;
               begin
                  V.Delete (Cursor_Copy);
               end;
            end if;
         end loop;
         Assert (False, "tampering check (reverse) should have failed");
      exception
         when Program_Error =>
            null;
      end Tampering_Reverse;

      procedure Deleting_In_Loop
        (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
      begin
         for J in Element_Type range K .. P loop
            Append (V, J);
         end loop;
         for Index in reverse V.First_Index .. V.Last_Index loop
            if V.Element (Index) = M then
               V.Delete (Index);
            end if;
         end loop;
         Assert (V.Length = 4, "wrong length");
         Assert (V.Element (2) = L, "wrong element (2)");
         Assert (V.Element (3) = N, "wrong element (3)");
      end Deleting_In_Loop;

      procedure Sorting
        (Unused : in out AUnit.Test_Cases.Test_Case'Class)
      is
         V : Vector (Capacity => 5);
         V_Copy : Vector (Capacity => 5);
         package Sort_Forward
         is new Vectors_For_Test.Generic_Sorting ("<" => "<");
         package Sort_Reverse
         is new Vectors_For_Test.Generic_Sorting ("<" => ">");
      begin
         --  This loop leaves the last element slot unfilled.
         for El in Element_Type range K .. N loop
            Append (V, El);
         end loop;
         V_Copy := V;
         Assert (Sort_Forward.Is_Sorted (V), "should be sorted (a)");
         Assert (not Sort_Reverse.Is_Sorted (V), "should not be sorted (a)");
         Sort_Forward.Sort (V);
         Assert (V = V_Copy, "sorted Vector /= copy (a)");
         Sort_Reverse.Sort (V);
         Assert (V.Element (1) = N, "wrong element (1)");
         Assert (V.Element (2) = M, "wrong element (2)");
         Assert (V.Element (3) = L, "wrong element (3)");
         Assert (V.Element (4) = K, "wrong element (4)");
         Assert (not Sort_Forward.Is_Sorted (V), "should not be sorted (b)");
         Assert (Sort_Reverse.Is_Sorted (V), "should be sorted (b)");
         Sort_Forward.Sort (V);
         Assert (Sort_Forward.Is_Sorted (V), "should be sorted (c)");
         Assert (not Sort_Reverse.Is_Sorted (V), "should not be sorted (c)");
         Assert (V = V_Copy, "sorted Vector /= copy (b)");
      end Sorting;

      overriding procedure Register_Tests (C : in out T)
      is
      begin
         Registration.Register_Routine
           (C, Initial'Access, "initial");
         Registration.Register_Routine
           (C, Add_5'Access, "add 5");
         Registration.Register_Routine
           (C, Clearing'Access, "clearing");
         Registration.Register_Routine
           (C, Too_Many'Access, "add too many");
         Registration.Register_Routine
           (C, Values'Access, "values, loops");
         Registration.Register_Routine
           (C, Finding_Index'Access, "find_index");
         Registration.Register_Routine
           (C, Out_Of_Range'Access, "out-of-range access");
         Registration.Register_Routine
           (C, Tampering_Forward'Access, "tampering (forward) detected");
         Registration.Register_Routine
           (C, Tampering_Reverse'Access, "tampering (reverse) detected");
         Registration.Register_Routine
           (C, Deleting_In_Loop'Access, "deleting in loop");
         Registration.Register_Routine
           (C, Sorting'Access, "sorting");
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
