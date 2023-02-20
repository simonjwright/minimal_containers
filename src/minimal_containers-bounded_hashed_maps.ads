with Ada.Containers;
private with Minimal_Containers.Bounded_Vectors;
private with System;

generic
   type Key_Type is private;
   type Element_Type is private;

   with function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;
   with function Equivalent_Keys
     (Left  : Key_Type;
      Right : Key_Type) return Boolean is "=";
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Minimal_Containers.Bounded_Hashed_Maps
is

   use Ada.Containers;

   type Map (Capacity : Count_Type;
             Modulus : Ada.Containers.Hash_Type) is private
   with
     Default_Initial_Condition => Is_Empty (Map),
     Iterable => (First       => First,
                  Next        => Next,
                  Has_Element => Has_Element,
                  Element     => Key);

   Empty_Map : constant Map;

   type Cursor is private;

   No_Element : constant Cursor;

   function Capacity (Container : Map) return Count_Type;

   function Length (Container : Map) return Count_Type
   with
     Post   => Length'Result <= Container.Capacity;

   function Is_Empty (Container : Map) return Boolean;

   function Key (Container : Map; Position : Cursor) return Key_Type
     with Pre => Has_Element (Container, Position);

   function Element (Container : Map; Position  : Cursor) return Element_Type
     with Pre => Has_Element (Container, Position);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type;
      Position  : out Cursor;
      Inserted  : out Boolean);

   procedure Insert
     (Container : in out Map;
      Key       : Key_Type;
      New_Item  : Element_Type);

   procedure Delete (Container : in out Map;
                     Key       :        Key_Type);

   procedure Delete (Container : in out Map;
                     Position  : in out Cursor);

   function First (Container : Map) return Cursor;

   function Next (Container : Map; Position : Cursor) return Cursor;

   function Find (Container : Map; Key : Key_Type) return Cursor;

   function Element (Container : Map; Key : Key_Type) return Element_Type;

   function Has_Element (Container : Map; Position : Cursor) return Boolean;

private

   --  This Map behaves as though the Hash function always returns 0,
   --  so that there is only one hash bucket.

   package Key_Vectors is new Bounded_Vectors
     (Index_Type => Positive, Element_Type => Key_Type);
   use Key_Vectors;

   package Element_Vectors is new Bounded_Vectors
     (Index_Type => Positive, Element_Type => Element_Type);
   use Element_Vectors;

   type Generation_Type is mod 2**32; -- for tampering checks

   type Map (Capacity : Count_Type;
             Modulus : Ada.Containers.Hash_Type) is record
      Generation : Generation_Type := 0;
      Keys       : Key_Vectors.Vector (Capacity => Capacity);
      Elements   : Element_Vectors.Vector (Capacity => Capacity);
   end record
   with Predicate => Length (Elements) = Length (Keys);

   Empty_Map : constant Map := (Capacity => 0, Modulus => 0, others => <>);

   type Cursor is record
      Container  : System.Address  := System.Null_Address;
      Generation : Generation_Type := 0;
      Node       : Count_Type      := Count_Type'First;
   end record;

   No_Element : constant Cursor := (Container  => System.Null_Address,
                                    Generation => 0,
                                    Node       => 0);

   function Capacity (Container : Map) return Count_Type
     is (Container.Capacity);

end Minimal_Containers.Bounded_Hashed_Maps;