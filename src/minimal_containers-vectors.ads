generic
   type Index_Type is range <>;
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Minimal_Containers.Vectors
is

   subtype Extended_Index is Index_Type'Base
     range Index_Type'First - 1 ..
     Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   No_Index : constant Extended_Index := Extended_Index'First;

   subtype Capacity_Range
     is Count_Type range 0 .. Index_Type'Pos (Index_Type'Last);

   type Vector (Capacity : Capacity_Range) is private
   with
     --  Default_Initial_Condition => Is_Empty (Vector),
     Iterable => (First       => Iter_First,
                  Has_Element => Iter_Has_Element,
                  Next        => Iter_Next,
                  Element     => Element);
   --  The 4-argument Iterable gives us for .. in and for .. of.

   function Empty_Vector return Vector;

   function Capacity (Container : Vector) return Capacity_Range;

   function Length (Container : Vector) return Capacity_Range;

   function Is_Empty (Container : Vector) return Boolean;

   function Element (Container : Vector;
                     Index : Extended_Index) return Element_Type
   with Pre => Index in Index_Type;

   procedure Append (Container : in out Vector; New_Item : Element_Type)
   with
     Pre  => Length (Container) < Capacity (Container),
     Post => Length (Container) = Length (Container'Old) + 1;

   procedure Delete (Container : in out Vector;
                     Index     :        Extended_Index)
   with
     Pre  => Index in First_Index (Container) .. Last_Index (Container),
     Post => Length (Container) = Length (Container)'Old - 1;

   function First_Index (Container : Vector) return Index_Type
   with
     Post => First_Index'Result = Index_Type'First;

   function Last_Index (Container : Vector) return Extended_Index
   with
     Post => Count_Type (Last_Index'Result - Index_Type'First + 1)
             = Length (Container);

   function Iter_First (Container : Vector) return Extended_Index;

   function Iter_Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean;

   function Iter_Next
     (Container : Vector;
      Position  : Extended_Index) return Extended_Index;

private

   subtype Array_Index is Capacity_Range range 1 .. Capacity_Range'Last;
   type Elements_Array is array (Array_Index range <>) of aliased Element_Type;
   overriding function "=" (L, R : Elements_Array) return Boolean is abstract;
   --  I guess because not all the array elements will be valid.

   type Vector (Capacity : Capacity_Range) is record
      Last     : Extended_Index := No_Index;
      Elements : Elements_Array (1 .. Capacity);
   end record;

   function Empty_Vector return Vector is
     ((Capacity => 0, others => <>));

   function Length (Container : Vector) return Capacity_Range
     is (Capacity_Range (Container.Last - Extended_Index'First));

   function Capacity (Container : Vector) return Capacity_Range
     is (Container.Capacity);

   function Is_Empty (Container : Vector) return Boolean
     is (Length (Container) = 0);

   function Element (Container : Vector;
                     Index : Extended_Index) return Element_Type
     is (Container.Elements
           (Capacity_Range
              (Index_Type'Pos (Index))
              - Index_Type'Pos (Index_Type'First)
              + 1));

   function First_Index (Container : Vector) return Index_Type
     is (Index_Type'First);

   function Last_Index (Container : Vector) return Extended_Index
     is (Container.Last);

   function Iter_First (Container : Vector) return Extended_Index
     is (Index_Type'First);

   function Iter_Has_Element
     (Container : Vector;
      Position  : Extended_Index) return Boolean
     is (Position in Index_Type'First .. Container.Last);

   function Iter_Next
     (Container : Vector;
      Position  : Extended_Index) return Extended_Index
     is (if Position < Container.Last
         then Extended_Index'Succ (Position)
         else No_Index);

end Minimal_Containers.Vectors;
