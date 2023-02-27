--  Minimal_Containers.Bounded_Hashed_Maps (spec)
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

private with Minimal_Containers.Bounded_Vectors;

generic
   type Key_Type is private;
   type Element_Type is private;

   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys (Left, Right : Key_Type) return Boolean;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Minimal_Containers.Bounded_Hashed_Maps
is

   use Ada.Containers;

   type Map (Capacity : Count_Type;
             Modulus  : Ada.Containers.Hash_Type) is tagged private
   with
     Default_Initial_Condition => Is_Empty (Map),
     Default_Iterator          => Iterate,
     Iterator_Element          => Element_Type,
     Constant_Indexing         => Element_For_Iteration; -- see Ada Gem 128

   type Cursor is private;

   Empty_Map : constant Map;

   function Has_Element (Position : Cursor) return Boolean;

   function Element_For_Iteration (Container : Map;
                                   Position  : Cursor) return Element_Type;

   package Map_Iterator_Interfaces
   is new Ada.Iterator_Interfaces (Cursor, Has_Element);

   No_Element : constant Cursor;

   function Capacity (Container : Map) return Count_Type;

   function Length (Container : Map) return Count_Type
   with
     Post => Length'Result in 0 .. Container.Capacity;

   function Is_Empty (Container : Map) return Boolean;

   function Key (Position : Cursor) return Key_Type
     with Pre => Has_Element (Position);

   function Element (Position  : Cursor) return Element_Type
     with Pre => Has_Element (Position);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Delete (Container : in out Map; Key : Key_Type);

   procedure Delete (Container : in out Map; Position  : in out Cursor);

   function First (Container : Map) return Cursor;

   function Next (Position : Cursor) return Cursor;

   function Find (Container : Map; Key : Key_Type) return Cursor;

   function Contains (Container : Map; Key : Key_Type) return Boolean;

   function Element (Container : Map; Key : Key_Type) return Element_Type;

   function Iterate (Container : Map)
                    return Map_Iterator_Interfaces.Forward_Iterator'Class;
private

   --  This Map behaves as though the Hash function always returns 0,
   --  so that there is only one hash bucket.

   package Key_Vectors is new Bounded_Vectors
     (Index_Type => Positive, Element_Type => Key_Type);
   use Key_Vectors;

   package Element_Vectors is new Bounded_Vectors
     (Index_Type => Positive, Element_Type => Element_Type);
   use Element_Vectors;

   type Map
     (Capacity : Count_Type;
      Modulus : Ada.Containers.Hash_Type) is tagged record
         Keys       : Key_Vectors.Vector (Capacity => Capacity);
         Elements   : Element_Vectors.Vector (Capacity => Capacity);
      end record
   with Predicate => Length (Elements) = Length (Keys);

   type Map_Access is access constant Map with Storage_Size => 0;

   type Cursor is record
      Container : Map_Access;
      Index     : Count_Type := Count_Type'First;
   end record;

   Empty_Map : constant Map := (Capacity => 0,
                                Modulus => 0,
                                others => <>);

   No_Element : constant Cursor := (Container => null,
                                    Index     => 0);

   function Capacity (Container : Map) return Count_Type
     is (Container.Capacity);

   type Iterator is new Map_Iterator_Interfaces.Forward_Iterator
      with record
         Container : Map_Access;
      end record;

   overriding function First (Object : Iterator) return Cursor;

   overriding function Next (Object : Iterator; Position : Cursor)
                            return Cursor;

end Minimal_Containers.Bounded_Hashed_Maps;
