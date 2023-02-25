--  Minimal_Containers.Bounded_Hashed_Maps (body)
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

package body Minimal_Containers.Bounded_Hashed_Maps is

   --  Body subprograms  --

   procedure Check_Cursor_Validity (For_The_Container : Map;
                                    The_Cursor : Cursor);

   --  Spec subprograms  --

   function Has_Element (Position : Cursor) return Boolean
   is
   begin
      return (if Position = No_Element
              then False
              else Position.Node in 1 .. Length (Position.Container.all));
   end Has_Element;

   function Element_For_Iteration (Container : Map;
                                   Position : Cursor) return Element_Type
   is
   begin
      Check_Cursor_Validity (Container, Position);
      return Element (Container.Elements, Positive (Position.Node));
   end Element_For_Iteration;

   function Length (Container : Map) return Count_Type
     is (Length (Container.Keys));

   function Is_Empty (Container : Map) return Boolean
     is (Length (Container.Keys) = 0);

   function Key (Position : Cursor) return Key_Type
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "no key for cursor=No_Element)";
      end if;
      return Element (Position.Container.Keys, Positive (Position.Node));
   end Key;

   function Element (Position : Cursor) return Element_Type
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "no element for cursor=No_Element";
      end if;
      return Element (Position.Container.Elements, Positive (Position.Node));
   end Element;

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

   procedure Delete (Container : in out Map; Key : Key_Type)
   is
      C : Cursor := Find (Container, Key);
   begin
      if C = No_Element then
         raise Constraint_Error with "key not found in map";
      end if;
      Delete (Container, C);
   end Delete;

   procedure Delete (Container : in out Map; Position  : in out Cursor)
   is
   begin
      if Position = No_Element then
         raise Constraint_Error with "can't delete, cursor=No_Element";
      end if;
      Delete (Container.Keys, Integer (Position.Node));
      Delete (Container.Elements, Integer (Position.Node));
      Container.Generation := Container.Generation + 1;
   end Delete;

   function First (Container : Map) return Cursor
     is (Cursor'(Container  => Container'Unrestricted_Access,
                 Generation => Container.Generation,
                 Node       => 1));

   function Next (Position : Cursor) return Cursor
   is
   begin
      if Position = No_Element then
         return No_Element;
      end if;
      return (if Position.Node in 1 .. Length (Position.Container.all) - 1
              then Cursor'(Container  => Position.Container,
                           Generation => Position.Container.Generation,
                           Node       => Count_Type'Succ (Position.Node))
              else No_Element);
   end Next;

   function Find (Container : Map; Key : Key_Type) return Cursor is
      Node : Count_Type := 1;
   begin
      for J in Container.Keys.Iterate loop
         if Key_Vectors.Element (J) = Key then
            return Cursor'(Container  => Container'Unrestricted_Access,
                           Generation => Container.Generation,
                           Node       => Node);
         end if;
         Node := Node + 1;
      end loop;
      return No_Element;
   end Find;

   function Element (Container : Map; Key : Key_Type) return Element_Type is
      C : constant Cursor := Find (Container, Key);
   begin
      if C = No_Element then
         raise Constraint_Error with "key not found in map";
      end if;
      return Element (C);
   end Element;

   function Contains (Container : Map;
                      Key       : Key_Type) return Boolean
   is (Find (Container, Key) /= No_Element);

   function Iterate (Container : Map)
                    return Map_Iterator_Interfaces.Forward_Iterator'Class
   is
   begin
      return It : constant Iterator
        := (Container => Container'Unrestricted_Access)
      do
         null;
      end return;
   end Iterate;

   overriding function First (Object : Iterator) return Cursor
   is (First (Object.Container.all));

   overriding function Next (Object : Iterator; Position : Cursor)
                            return Cursor
   is
   begin
      if Position = No_Element then
         return No_Element;
      elsif Position.Container /= Object.Container then
         raise Program_Error with "Position designates the wrong Map";
      else
         return Next (Position);
      end if;
   end Next;

   --  Body subprogram implementations

   procedure Check_Cursor_Validity (For_The_Container : Map;
                                    The_Cursor : Cursor)
   is
   begin
      if The_Cursor.Container /= For_The_Container'Unrestricted_Access then
         raise Constraint_Error with "cursor for different/no map";
      end if;
      if The_Cursor.Generation /= For_The_Container.Generation then
         raise Program_Error with "map has been tampered with";
      end if;
   end Check_Cursor_Validity;

end Minimal_Containers.Bounded_Hashed_Maps;
