--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident.                Luebeck            --
--        Branch                                   Autumn, 2005       --
--  Interface                                                         --
--                                Last revision :  14:48 30 May 2014  --
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

package Fuzzy.Graph.Memory_Resident.Branch is
   pragma Elaborate_Body (Fuzzy.Graph.Memory_Resident.Branch);
--
-- Create -- Create a branch node
--
--    Header    - Of the graph (a handle to)
--    Feature   - The feature
--    Children  - Of the node
--  [ Weights ] - Of the nodes
--
-- This function creates a branch node testing Feature with arcs leading
-- to Children.  When  the  parameter  Weights  is  specified,  then  it
-- determines the weights of the children. 
--
-- Returns :
--
--    A pointer to the created node
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality, no children
--    Use_Error        - Feature depends on Header
--
   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr;
   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Set
            )  return Graph_Node_Ptr;
private
--
-- Branch_Node -- A node with children
--
   type Branch_Node (Cardinality : Positive) is
      new Memory_Node (Cardinality) with
   record
      Successor : Node_Ptr_Array (1..Cardinality);
   end record;
--
-- Classify -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Classify
             (  Node   : Branch_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             );
--
-- Finalize -- Destructor
--
--    Node - The node
--
-- All children are disconnected
--
   overriding
   procedure Finalize (Node : in out Branch_Node);
--
-- Find -- Overrides Fuzzy.Graph...
--
   overriding
   function Find
            (  Parent : Branch_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural;
--
-- Get_Child -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Child (Node : Branch_Node; Index : Positive)
      return Graph_Node_Ptr;
--
-- Get_Children_Number -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Children_Number
            (  Node      : Branch_Node;
               Immediate : Boolean := True
            )  return Natural;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Node : Branch_Node) return String;
--
-- Get_Distribution -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Distribution (Node : Branch_Node)
      return Classification;
--
-- Get_Examples -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Get_Examples
             (  Node   : Branch_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Referents -- Overrides Object.Archived...
--
-- The dependency list consists of the graph header, the feature  tested
-- and the children nodes. 
--
   overriding
   procedure Get_Referents
             (  Node : Branch_Node;
                List : in out Deposit_Container'Class
             );
--
-- Get_Type -- Overrides Fuzzy.Graph...
--
-- The result is Tree_Branch
--
   overriding
   function Get_Type (Node : Branch_Node) return Node_Type;
--
-- Like -- Overrides Fuzzy.Graph...
--
   overriding
   function Like
            (  Left  : Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean;
--
-- Modify -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Modify
             (  Node        : in out Branch_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean; 
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             );
   overriding
   procedure Modify
             (  Node         : in out Branch_Node;
                Data         : in out Node_Modification_Data'Class;
                Distribution : Classification;
                Exclusive    : Boolean; 
                Combination  : Node_Modification; 
                New_Node     : in out Graph_Node_Ptr
             );
--
-- Precedent -- Overrides Fuzzy.Graph...
--
   overriding
   function Precedent
            (  Left  : Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean;
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Node    : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Branch_Node
             );             
--
-- Tests -- Overrides Fuzzy.Graph...
--
   overriding
   function Tests
            (  Node    : Branch_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean;

   pragma Inline (Find);
   pragma Inline (Get_Child);
   pragma Inline (Get_Children_Number);
   pragma Inline (Get_Class);
   pragma Inline (Get_Type);

end Fuzzy.Graph.Memory_Resident.Branch;
