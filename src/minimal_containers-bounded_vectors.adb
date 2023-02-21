package body Minimal_Containers.Bounded_Vectors
is

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
   end Delete;

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

end Minimal_Containers.Bounded_Vectors;
