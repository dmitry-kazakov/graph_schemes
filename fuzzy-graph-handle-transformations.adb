--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.                         Luebeck            --
--        Transformations                          Winter, 2005       --
--  Implementation                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Fuzzy.Lecture.Empty;  use Fuzzy.Lecture.Empty;
with System;               use System;

with Generic_Map;

package body Fuzzy.Graph.Handle.Transformations is

   function "<" (Left, Right : Graph_Node_Ptr) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (  Left = null
         or else
            Left.all'Address < Right.all'Address
      )  );
   end "<";

   package Node_To_Node is
      new Generic_Map
          (  Key_Type    => Graph_Node_Ptr,
             Object_Type => Node_Handle
          );
   use Node_To_Node;

   function Create_Plane
            (  Data    : access Node_Modification_Data'Class;
               Node    : Graph_Node'Class;
               Feature : Feature_Object'Class;
               Index   : Positive
            )  return Graph_Node_Ptr is
      pragma Inline (Create_Plane);
      Children : Node_Ptr_Array (1..Node.Cardinality);
      Child    : Graph_Node_Ptr;
      Empty    : Boolean := True;
   begin
      for Node_Index in 1..Node.Cardinality loop
         Child := Get_Child (Node, Node_Index);
         if Child /= null then
            if Get_Feature (Child.all) = Feature.Self then
               Children (Node_Index) := Get_Child (Child.all, Index);
               Empty := Empty and then Children (Node_Index) = null;
            else
               Children (Node_Index) := Child;
               Empty := False;
            end if;
         end if;
      end loop;
      if Empty then
         return null;
      else
         return Create (Data.Factory, Get_Feature (Node).all, Children);
      end if;
   end Create_Plane;

   function Create_Rotated
            (  Data    : access Node_Modification_Data'Class;
               Node    : Graph_Node'Class;
               Feature : Feature_Object'Class
            )  return Graph_Node_Ptr is
      pragma Inline (Create_Rotated);
      Children : Node_Ptr_Array (1..Feature.Cardinality);
   begin
      for Index in Children'Range loop
         Children (Index) := Create_Plane (Data, Node, Feature, Index);
      end loop;
      return Create (Data.Factory, Feature, Children);
   exception
      when others =>
         for Index in Children'Range loop
            Free (Children (Index));
         end loop;
         raise;
   end Create_Rotated;

   procedure Rotate
             (  Data     : in out Node_Modification_Data'Class;
                Node     : in out Graph_Node'Class;
                Feature  : Feature_Object'Class;
                Rotary   : in out Boolean;
                New_Node : in out Graph_Node_Ptr
             )  is
   begin
      New_Node := null;
      if Get_Type (Node) = Tree_Leaf then
         return;
      end if;
      if Get_Feature (Node) = Feature.Self then
         Rotary := True;
         return;
      end if;
      --
      -- All children of the node are rotated first.
      --
      declare
         Old_Children : Map;
         New_Children : Node_Ptr_Array (1..Node.Cardinality);
         Old_Child    : Graph_Node_Ptr;
         New_Child    : Graph_Node_Ptr;
      begin
         for Index in 1..Node.Cardinality loop
            Old_Child := Get_Child (Node, Index);
            if Old_Child /= null then
               if Is_In (Old_Children, Old_Child) then
                  New_Children (Index) :=
                     Ptr (Get (Old_Children, Old_Child));
               else
                  Rotate
                  (  Data,
                     Old_Child.all,
                     Feature,
                     Rotary,
                     New_Child
                  );
                  if New_Child = null then
                     Add (Old_Children, Old_Child, Ref (Old_Child));
                     New_Children (Index) := Old_Child;
                  else
                     Add (Old_Children, Old_Child, Ref (New_Child));
                     New_Children (Index) := New_Child;
                  end if;
               end if;
            end if;
         end loop;
         --
         -- Create the new node if necessary. Rotary is True if at least
         -- one of the children now indeed tests Feature.
         --
         if Rotary then
            Modify
            (  Node,
               Data,
               New_Children,
               False,
               Replacement,
               New_Node
            );
            if New_Node = null then
               New_Node := Create_Rotated (Data'Access, Node, Feature);
            else
               declare
                  Old_Node : Graph_Node_Ptr := New_Node;
               begin
                  New_Node :=
                     Create_Rotated
                     (  Data'Access,
                        Old_Node.all,
                        Feature
                     );
                  if Old_Node.Use_Count = 0 then
                     Free (Old_Node);
                  end if;
               end;
            end if;
         end if;
      end;
   exception
      when others =>
         if New_Node /= null and then New_Node.Use_Count = 0 then
            Free (New_Node);
         end if;
         raise;
   end Rotate;

   procedure Rotate
             (  Node    : in out Node_Handle;
                Feature : Feature_Handle;
                Data    : in out Node_Modification_Data'Class
             )  is
   begin
      if not (Is_Valid (Node) and then Feature.Is_Valid) then
         raise Constraint_Error;
      end if;
      declare
         Rotary   : Boolean := False;
         New_Node : Graph_Node_Ptr;
      begin
         Rotate
         (  Data,
            Ptr (Node).all,
            Ptr (Feature).all,
            Rotary,
            New_Node
         );
         if New_Node /= null then
            Node := Ref (New_Node);
         end if;
      end;
   end Rotate;

   procedure Rotate
             (  Node    : in out Node_Handle;
                Feature : Feature_Handle
             )  is
      Data : Node_Modification_Data (Empty_Set, 0, Negleter'Access);
   begin
      Data.Factory := Get_Factory (Ptr (Node).all);
      Rotate (Node, Feature, Data);
   end Rotate;

end Fuzzy.Graph.Handle.Transformations;
