--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident.Leaf            Luebeck            --
--  Implementation                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Ada.Tags;                   use Ada.Tags;
with Fuzzy.Edit.Intuitionistic;  use Fuzzy.Edit.Intuitionistic;

package body Fuzzy.Graph.Memory_Resident.Leaf is

   Class : constant String := Node_Class & "Leaf";

   procedure Classify
             (  Node   : Leaf_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             )  is
   begin
      Or_At
      (  Data.Result.Possibility,
         Node.Distribution.Possibility,
         Weight
      );
      And_At
      (  Data.Result.Necessity,
         Node.Distribution.Necessity,
         not Weight
      );
      Data.Ready :=
         (  Necessity (Data.Result.Possibility) = Confidence'Last
         and then
            Possibility (Data.Result.Necessity) = Confidence'First
         );
   end Classify;

   procedure Connect
             (  Parent    : in out Leaf_Node;
                Data      : in out Node_Modification_Data'Class;
                Child     : in out Graph_Node'Class;
                Index     : Positive;
                Exclusive : Boolean;
                New_Node  : in out Graph_Node_Ptr
             )  is
   begin
      Raise_Exception
      (  Constraint_Error'Identity,
         "Connecting to a leaf node"
      );
   end Connect;

   function Create
            (  Header       : Header_Handle;
               Distribution : Classification
            )  return Graph_Node_Ptr is
      New_Node : Graph_Node_Ptr;
      Classes  : Feature_Object'Class renames
                    Ptr (Ptr (Header.Handle).Classes).all;
   begin
      if Classes.Cardinality /= Distribution.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of leaf classification"
         );
      end if;
      New_Node := new Leaf_Node (Distribution.Cardinality);
      declare
         Leaf : Leaf_Node renames Leaf_Node (New_Node.all);
      begin
         Leaf.Self         := New_Node;
         Leaf.Distribution := Distribution;
         Attach (New_Node, Header, Classes);
      end;
      return New_Node;
   exception
      when others =>
         if New_Node /= null and then New_Node.Use_Count = 0 then
            Free (New_Node);
         end if;
         raise;
   end Create;

   function Find
            (  Parent : Leaf_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural is
   begin
      return 0;
   end Find;

   function Get_Child (Node : Leaf_Node; Index : Positive)
      return Graph_Node_Ptr is
   begin
      return null;
   end Get_Child;

   function Get_Children_Number
            (  Node      : Leaf_Node;
               Immediate : Boolean := True
            )  return Natural is
   begin
      return 0;
   end Get_Children_Number;

   function Get_Class (Node : Leaf_Node) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Distribution (Node : Leaf_Node) return Classification is
   begin
      return Node.Distribution;
   end Get_Distribution;

   procedure Get_Examples
             (  Node   : Leaf_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Example : constant Positive := Get_Examples_Number (Lesson) + 1;
   begin
      Reset (Viewer.all, 0);
      Put
      (  Lesson,
         Example,
         Ref (Get_Feature (Node)),
         Node.Distribution
      );
      Done (Viewer.all);
   end Get_Examples;

   procedure Get_Referents
             (  Node : Leaf_Node;
                List : in out Deposit_Container'Class
             )  is
   begin
      Add
      (  List,
         To_Deposit_Ptr (Ptr (Node.Layer.Header.Handle)),
         False
      );
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Use_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Referents;

   function Get_Type (Node : Leaf_Node) return Node_Type is
   begin
      return Tree_Leaf;
   end Get_Type;

   function Like
            (  Left  : Leaf_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if (  Leaf_Node'Tag /= Right'Tag
         or else
            Left.Cardinality /= Right.Cardinality
         )
      then
         return False;
      else
         return Left.Distribution = Get_Distribution (Right);
      end if;
   end Like;

   procedure Modify
             (  Node        : in out Leaf_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean;
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             )  is
   begin
      Raise_Exception
      (  Constraint_Error'Identity,
         "Branching a leaf node"
      );
   end Modify;

   procedure Modify
             (  Node         : in out Leaf_Node;
                Data         : in out Node_Modification_Data'Class;
                Distribution : Classification;
                Exclusive    : Boolean;
                Combination  : Node_Modification;
                New_Node     : in out Graph_Node_Ptr
             )  is
      use Fuzzy.Graph.Node_Class_Sets;
      New_Distribution : Classification := Distribution;
   begin
      case Combination is
         when Union =>
            Or_At (New_Distribution, Node.Distribution);
         when Intersection =>
            And_At (New_Distribution, Node.Distribution);
         when Replacement =>
            null;
      end case;
      if New_Distribution /= Node.Distribution then
         if Exclusive then
            declare
               Old_Node  : constant Graph_Node_Ptr := Node.Self;
               Used_Node : Graph_Node_Ptr := Old_Node;
            begin
               Remove (Node.Layer.Nodes, Used_Node);
               Node.Distribution := New_Distribution;
               Insert (Node.Layer.Nodes, Used_Node);
               if Old_Node = Used_Node then
                  Node.Sequence := Get_Sequence_No;
               else
                  New_Node   := Used_Node;
                  Node.Layer := null;
               end if;
            end;
         else
            New_Node := Create (Node.Layer.Header, New_Distribution);
         end if;
      end if;
   end Modify;

   function Precedent
            (  Left  : Leaf_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if Left.Cardinality = Right.Cardinality then
         if Leaf_Node'Tag = Right'Tag then
            return Left.Distribution < Get_Distribution (Right);
         else
            return Leaf_Node'External_Tag < External_Tag (Right'Tag);
         end if;
      else
         return Left.Cardinality < Right.Cardinality;
      end if;
   end Precedent;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Node    : out Deposit_Ptr
             )  is
      Node_Ptr : Graph_Node_Ptr;
      Header   : Graph_Header_Ptr;
   begin
      begin
         Header := To_Graph_Header_Ptr (Get (List, 1));
         if Header = null then
            Raise_Exception
            (  Use_Error'Identity,
               "No graph for restored leaf node"
            );
         end if;
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Use_Error'Identity,
               Exception_Message (Error)
            );
      end;
      declare
         Classes : Feature_Object'Class renames
                      Ptr (Header.Classes).all;
         Distribution : Classification (Classes.Cardinality);
      begin
         Get (Source, Pointer, Distribution);
         Node_Ptr := new Leaf_Node (Classes.Cardinality);
         declare
            Leaf : Leaf_Node renames Leaf_Node (Node_Ptr.all);
         begin
            Leaf.Self         := Node_Ptr;
            Leaf.Distribution := Distribution;
            Attach
            (  Node_Ptr,
               (Ada.Finalization.Controlled with Ref (Header)),
               Classes
            );
            Node := To_Deposit_Ptr (Node_Ptr);
         exception
            when Use_Error =>
               Free (Node_Ptr);
               Raise_Exception
               (  Use_Error'Identity,
                  "Circular dependency encountered"
               );
            when others =>
               Free (Node_Ptr);
               raise;
         end;
         Reset_Modified (Node_Ptr.all);
      end;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Leaf_Node
             )  is
   begin
      Put (Destination, Pointer, Node.Distribution);
   end Store;

   function Tests
            (  Node    : Leaf_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean is
   begin
      if Node.Layer /= null then
         return Equal (Feature.all, Ptr (Node.Layer.Feature).all);
      else
         return False;
      end if;
   end Tests;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Graph.Memory_Resident.Leaf;
