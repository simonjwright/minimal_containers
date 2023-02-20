package body Minimal_Containers.Bounded_Hashed_Maps is

   procedure Check_Cursor_Validity (For_The_Container : Map;
                                    The_Cursor : Cursor);

   function Length (Container : Map) return Count_Type
     is (Length (Container.Keys));

   function Is_Empty (Container : Map) return Boolean
     is (Length (Container.Keys) = 0);

   function Key (Container : Map; Position : Cursor) return Key_Type
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return Element (Container.Keys, Positive (Position.Node));
   end Key;

   function Element (Container : Map; Position : Cursor) return Element_Type
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return Element (Container.Elements, Positive (Position.Node));
   end Element;

   procedure Insert
     (Container : in out Map;
      Key       :        Key_Type;
      New_Item  :        Element_Type;
      Position  :    out Cursor;
      Inserted  :    out Boolean)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Insert unimplemented");
      raise Program_Error with "Unimplemented procedure Insert";
   end Insert;

   procedure Insert
     (Container : in out Map; Key : Key_Type; New_Item : Element_Type)
   is
   begin
      if Length (Container) >= Container.Capacity then
         raise Capacity_Error with "map already full";
      end if;
      if Find (Container, Key) /= No_Element then
         raise Constraint_Error with "key already in map";
      end if;
      Append (Container.Keys, Key);
      Append (Container.Elements, New_Item);
      Container.Generation := Container.Generation + 1;
   end Insert;

   procedure Delete (Container : in out Map;
                     Key       :        Key_Type)
   is
      C : Cursor := Find (Container, Key);
   begin
      if C = No_Element then
         raise Constraint_Error with "key not found in map";
      end if;
      Delete (Container, C);
   end Delete;

   procedure Delete (Container : in out Map;
                     Position  : in out Cursor)
   is
   begin
      Check_Cursor_Validity (Container, Position);
      Delete (Container.Keys, Integer (Position.Node));
      Delete (Container.Elements, Integer (Position.Node));
      Container.Generation := Container.Generation + 1;
   end Delete;

   function First (Container : Map) return Cursor
     is (Cursor'(Container  => Container'Address,
                 Generation => Container.Generation,
                 Node       => 1));

   function Next (Container : Map; Position : Cursor) return Cursor
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return (if Position.Node in 1 .. Length (Container)
              then Cursor'(Container  => Container'Address,
                           Generation => Container.Generation,
                           Node       => Count_Type'Succ (Position.Node))
              else No_Element);
   end Next;

   function Find (Container : Map; Key : Key_Type) return Cursor is
   begin
      for J in Container.Keys loop
         if Element (Container.Keys, J) = Key then
            return Cursor'(Container  => Container'Address,
                           Generation => Container.Generation,
                           Node       => Count_Type (J));
         end if;
      end loop;
      return No_Element;
   end Find;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      C : constant Cursor := Find (Container, Key);
   begin
      return Element (Container, C);
   end Element;

   function Has_Element (Container : Map; Position : Cursor) return Boolean
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return Position.Node in 1 .. Length (Container);
   end Has_Element;

   --  Body subprogram implementations

   procedure Check_Cursor_Validity (For_The_Container : Map;
                                    The_Cursor : Cursor)
   is
      use type System.Address;
   begin
      if The_Cursor.Container /= For_The_Container'Address then
         raise Constraint_Error with "cursor for different/no map";
      end if;
      if The_Cursor.Generation /= For_The_Container.Generation then
         raise Program_Error with "map has been tampered with";
      end if;
   end Check_Cursor_Validity;

end Minimal_Containers.Bounded_Hashed_Maps;
