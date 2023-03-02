--  Minimal_Containers.Bounded_Vectors (body)
--
--  Copyright (C) 2023 Simon Wright <simon@pushface.org>
--
--  Minimal_Containers is free software; you can redistribute it
--  and/or modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 3, or
--  (at your option) any later version.  It is distributed in the hope
--  that it will be useful, but WITHOUT ANY WARRANTY; without even the
--  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
--  PURPOSE.
--
--  As a special exception under Section 7 of GPL version 3, you are
--  granted additional permissions described in the GCC Runtime
--  Library Exception, version 3.1, as published by the Free Software
--  Foundation.
--
--  You should have received a copy of the GNU General Public License
--  and a copy of the GCC Runtime Library Exception along with this
--  program; see the files COPYING3 and COPYING.RUNTIME respectively.
--  If not, see <http://www.gnu.org/licenses/>.

with Ada.Containers.Generic_Array_Sort;

package body Minimal_Containers.Bounded_Vectors
is

   --  Body subprograms  --

   procedure Check_Cursor_Validity (For_The_Container : Vector;
                                    The_Cursor        : Cursor);

   --  Spec subprograms  --

   function Has_Element (Position : Cursor) return Boolean
   is
   begin
      return (if Position = No_Element
              then False
              else Position.Index
                in Index_Type'First .. Position.Container.Last);
   end Has_Element;

   function Capacity (Container : Vector) return Count_Type
     is (Container.Capacity);

   function Length (Container : Vector) return Count_Type
     is (Count_Type (Container.Last - Extended_Index'First));

   function Is_Empty (Container : Vector) return Boolean
     is (Length (Container) = 0);

   function Element (Container : Vector;
                     Index : Index_Type) return Element_Type
     is (if Index <= Container.Last
         then Container.Elements
           (Capacity_Range
              (Index_Type'Pos (Index))
              - Index_Type'Pos (Index_Type'First)
              + 1)
         else raise Constraint_Error with "invalid index in iterator");

   function Element (Position : Cursor) return Element_Type
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "no element for cursor=No_Element";
      end if;
      return Element (Position.Container.all, Position.Index);
   end Element;

   procedure Append (Container : in out Vector; New_Item : Element_Type)
   is
   begin
      if Length (Container) >= Container.Capacity then
         raise Capacity_Error with "appending to full vector";
      end if;
      Container.Last := Extended_Index'Succ (Container.Last);
      Container.Elements (Capacity_Range
                            (Index_Type'Pos (Container.Last))
                            - Index_Type'Pos (Index_Type'First)
                            + 1)
        := New_Item;
      Container.Generation := Container.Generation + 1;
   end Append;

   procedure Delete (Container : in out Vector;
                     Index     :        Extended_Index)
   is
      Idx : constant Count_Type
        := Count_Type (Index - Index_Type'First) + Container.Elements'First;
      Last_Idx : constant Count_Type
        := Count_Type (Container.Last - Index_Type'First) + 1;
   begin
      if not (Index in Index_Type'First .. Container.Last) then
         raise Constraint_Error with "deleting from vector with invalid index";
      end if;
      Container.Elements (Idx .. Last_Idx - 1)
        := Container.Elements (Idx + 1 .. Last_Idx);
      Container.Last := Container.Last - 1;
      Container.Generation := Container.Generation + 1;
   end Delete;

   procedure Delete (Container : in out Vector;
                     Position  : in out Cursor) is
      Old_Index : constant Index_Type := Position.Index;
   begin
      Check_Cursor_Validity (Container, Position);
      Delete (Container, Position.Index);
      if Old_Index >= Container.Last then   -- XXXXXXXXXXXXXXXXXXXXXXXXX
         Position.Index := Container.Last;
      end if;
   end Delete;

   procedure Delete_First (Container : in out Vector)
   is
   begin
      Delete (Container, Index_Type'First);
   end Delete_First;

   procedure Delete_Last (Container : in out Vector)
   is
   begin
      Delete (Container, Container.Last);
   end Delete_Last;

   function First_Index (Container : Vector) return Index_Type
     is (Index_Type'First);

   function First (Container : Vector) return Cursor
     is (if Length (Container) = 0
         then No_Element
         else Cursor'(Container  => Container'Unrestricted_Access,
                      Generation => Container.Generation,
                      Index      => Index_Type'First));

   function First_Element (Container : Vector) return Element_Type
   is (Element (Container, Index_Type'First));

   function Last_Index (Container : Vector) return Extended_Index
     is (if Is_Empty (Container)
         then No_Index
         else Container.Last);

   function Last (Container : Vector) return Cursor
     is (if Is_Empty (Container)
         then No_Element
         else Cursor'(Container  => Container'Unrestricted_Access,
                      Generation => Container.Generation,
                      Index      => Container.Last));

   function Last_Element (Container : Vector) return Element_Type
     is (Element (Container, Last_Index (Container)));

   function Next (Position : Cursor) return Cursor
     is (if Position = No_Element
         then No_Element
         elsif Position.Index
           in Index_Type'First .. Index_Type'Pred (Position.Container.Last)
         then Cursor'(Container  => Position.Container,
                      Generation => Position.Generation,
                      Index      => Index_Type'Succ (Position.Index))
         else No_Element);

   function Previous (Position : Cursor) return Cursor
     is (if Position = No_Element
           or else Is_Empty (Position.Container.all)
           or else Position.Index = Index_Type'First
         then No_Element
         else Cursor'(Container  => Position.Container,
                      Generation => Position.Generation,
                      Index      => Index_Type'Pred (Position.Index)));

   function Find_Index (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
                       return Extended_Index
   is
      Start_Index : constant Capacity_Range
        := Capacity_Range (Index - Index_Type'First + 1);
      Last_Index : constant Capacity_Range
        := Capacity_Range (Container.Last - Index_Type'First + 1);
   begin
      for J in Start_Index .. Last_Index loop
         if Container.Elements (J) = Item then
            return Extended_Index (J + Count_Type (Index_Type'First - 1));
         end if;
      end loop;
      return No_Index;
   end Find_Index;

   --  Sorting  --

   package body Generic_Sorting is

      function Is_Sorted (Container : Vector) return Boolean is
      begin
         if Container.Last <= Index_Type'First then
            return True;
         end if;

         declare
            EA : Elements_Array renames Container.Elements;
            Result : Boolean;
         begin
            Result := True;
            for J in 1 .. Container.Length - 1 loop
               if EA (J + 1) < EA (J) then
                  Result := False;
                  exit;
               end if;
            end loop;

            return Result;
         end;
      end Is_Sorted;

      procedure Sort (Container : in out Vector) is
         procedure Sort is
            new Generic_Array_Sort
             (Index_Type   => Array_Index,
              Element_Type => Element_Type,
              Array_Type   => Elements_Array,
              "<"          => "<");
      begin
         if Container.Last <= Index_Type'First then
            return;
         end if;

         Sort (Container.Elements (1 .. Container.Length));
     end Sort;

   end Generic_Sorting;

   --  Iteration  --

   function Element_For_Iteration (Container : Vector;
                                   Position  : Cursor) return Element_Type
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return Element (Position);
   end Element_For_Iteration;

   function Iterate
     (Container : Vector)
     return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return It : constant Iterator
        := (Container => Container'Unrestricted_Access,
            Index     => Index_Type'First)
      do
         null;
      end return;
   end Iterate;

   overriding function First (Object : Iterator) return Cursor
   is (First (Object.Container.all));

   overriding function Last  (Object : Iterator) return Cursor
   is (Last (Object.Container.all));

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Position = No_Element then
         return No_Element;
      elsif Position.Container /= Object.Container then
         raise Program_Error with "Position designates the wrong Vector";
      elsif Position.Generation /= Object.Container.Generation then
         raise Program_Error with "The Vector has been tampered with";
      else
         return Next (Position);
      end if;
   end Next;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor
   is
   begin
      if Position = No_Element then
         return No_Element;
      elsif Position.Container /= Object.Container then
         raise Program_Error with "Position designates the wrong Vector";
      elsif Position.Generation /= Object.Container.Generation then
         raise Program_Error with "The Vector has been tampered with";
      else
         return Previous (Position);
      end if;
   end Previous;

   --  Body subprogram implementations

   procedure Check_Cursor_Validity (For_The_Container : Vector;
                                    The_Cursor        : Cursor)
   is
   begin
      if The_Cursor.Container /= For_The_Container'Unrestricted_Access then
         raise Constraint_Error with "Cursor for different/no Vector";
      end if;
      if The_Cursor.Generation /= For_The_Container.Generation then
         raise Program_Error with "Vector has been tampered with";
      end if;
   end Check_Cursor_Validity;

end Minimal_Containers.Bounded_Vectors;
