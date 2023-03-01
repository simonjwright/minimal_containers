--  Minimal_Containers.Bounded_Vectors (spec)
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

with Ada.Containers;
with Ada.Iterator_Interfaces;

generic
   type Index_Type is range <>;
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Minimal_Containers.Bounded_Vectors
is

   use Ada.Containers;

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   subtype Capacity_Range
     is Count_Type range 0 .. Index_Type'Pos (Index_Type'Last);

   type Vector (Capacity : Capacity_Range) is tagged private
   with
     Default_Initial_Condition => Is_Empty (Vector),
     Default_Iterator          => Iterate,
     Iterator_Element          => Element_Type,
     Constant_Indexing         => Element_For_Iteration; -- see Ada Gem 128

   type Cursor is private;

   Empty_Vector : constant Vector;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   package Vector_Iterator_Interfaces is new
      Ada.Iterator_Interfaces (Cursor, Has_Element);

   function Capacity (Container : Vector) return Count_Type;

   function Length (Container : Vector) return Count_Type;

   function Is_Empty (Container : Vector) return Boolean;

   function Element (Container : Vector;
                     Index : Index_Type) return Element_Type;

   function Element (Position : Cursor) return Element_Type;

   procedure Append (Container : in out Vector; New_Item : Element_Type)
   with
     Pre  => Length (Container) < Capacity (Container),
     Post => Length (Container) = Length (Container'Old) + 1;

   procedure Delete (Container : in out Vector;
                     Index     : Extended_Index)
   with
     Pre  => Index in Index_Type'First .. Last_Index (Container),
     Post => Length (Container) = Length (Container)'Old - 1;

   procedure Delete (Container : in out Vector;
                     Position  : in out Cursor)
   with
     Pre  => Position /= No_Element,
     Post => Length (Container) = Length (Container)'Old - 1;

   procedure Delete_First (Container : in out Vector)
   with
     Pre => not Is_Empty (Container),
     Post => Length (Container) = Length (Container)'Old - 1;

   procedure Delete_Last (Container : in out Vector)
   with
     Pre => not Is_Empty (Container),
     Post => Length (Container) = Length (Container)'Old - 1;

   function First_Index (Container : Vector) return Index_Type
   with
     Post => First_Index'Result = Index_Type'First;

   function First (Container : Vector) return Cursor;

   function First_Element (Container : Vector) return Element_Type
   with
     Pre => not Is_Empty (Container);

   function Last_Index (Container : Vector) return Extended_Index;

   function Last (Container : Vector) return Cursor;

   function Last_Element (Container : Vector) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   function Find_Index (Container : Vector;
                        Item      : Element_Type;
                        Index     : Index_Type := Index_Type'First)
                       return Extended_Index;

   --  For iteration  --

   function Element_For_Iteration (Container : Vector;
                                   Position  : Cursor) return Element_Type;

   function Iterate
     (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

private

   subtype Array_Index is Capacity_Range range 1 .. Capacity_Range'Last;
   type Elements_Array is array (Array_Index range <>) of aliased Element_Type;

   type Generation_Type is mod 2**32; -- for tampering checks

   type Vector (Capacity : Capacity_Range) is tagged record
      Generation : Generation_Type := 0;
      Last       : Extended_Index  := No_Index;
      Elements   : Elements_Array (1 .. Capacity);
   end record;

   type Vector_Access is access constant Vector with Storage_Size => 0;

   type Cursor is record
      Container  : Vector_Access;
      Generation : Generation_Type := 0;
      Index      : Index_Type      := Index_Type'First;
   end record;

   Empty_Vector : constant Vector := (Capacity => 0, others => <>);

   No_Element : constant Cursor := (Container  => null,
                                    Generation => 0,
                                    Index      => Index_Type'First);

   type Iterator is new Vector_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : Vector_Access;
         Index     : Index_Type'Base;
      end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Last  (Object : Iterator) return Cursor;

   overriding function Next
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   overriding function Previous
     (Object   : Iterator;
      Position : Cursor) return Cursor;

   --  Spec Implementations  --

   --  none

end Minimal_Containers.Bounded_Vectors;
