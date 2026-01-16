--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident.Leaf            Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
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
--
--  This package defines the leaf node type. It has no children.
--
package Fuzzy.Graph.Memory_Resident.Leaf is
   pragma Elaborate_Body (Fuzzy.Graph.Memory_Resident.Leaf);
--
-- Create -- Create a leaf node
--
--    Header       - Of the graph (a handle to)
--    Distribution - Of the classes
--
-- Returns :
--
--    A pointer to the created node
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of Distribution
--    Use_Error        - Class-feature depends on Header
--
   function Create
            (  Header       : Header_Handle;
               Distribution : Classification
            )  return Graph_Node_Ptr;
private
--
-- The type of a leaf node
--
   type Leaf_Node (Cardinality : Positive) is
      new Memory_Node (Cardinality) with
   record
      Updated      : Boolean := True;
      Distribution : Classification (Cardinality);
   end record;
--
-- Classify -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Classify
             (  Node   : Leaf_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             );
--
-- Find -- Overrides Fuzzy.Graph...
--
   overriding
   function Find
            (  Parent : Leaf_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural;
--
-- Get_Child -- Overrides Fuzzy.Graph...
--
-- A leaf has no children so it always returns null.
--
   overriding
   function Get_Child (Node : Leaf_Node; Index : Positive)
      return Graph_Node_Ptr;
--
-- Get_Children_Number -- Overrides Fuzzy.Graph...
--
-- A leaf has no children so it always returns 0.
--
   overriding
   function Get_Children_Number
            (  Node      : Leaf_Node;
               Immediate : Boolean := True
            )  return Natural;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Node : Leaf_Node) return String;
--
-- Get_Distribution -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Distribution (Node : Leaf_Node)
      return Classification;
--
-- Get_Examples -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Get_Examples
             (  Node   : Leaf_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Referents -- Overrides Object.Archived...
--
-- The dependency list includes the graph header only
--
   procedure Get_Referents
             (  Node : Leaf_Node;
                List : in out Deposit_Container'Class
             );
--
-- Get_Type -- Overrides Fuzzy.Graph...
--
-- The type of a leaf node is Tree_Leaf
--
   function Get_Type (Node : Leaf_Node) return Node_Type;
--
-- Like -- Overrides Fuzzy.Graph...
--
   overriding
   function Like
            (  Left  : Leaf_Node;
               Right : Graph_Node'Class
            )  return Boolean;
--
-- Modify -- Overrides Fuzzy.Graph...
--
   overriding
   procedure Modify
             (  Node        : in out Leaf_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean;
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             );
   overriding
   procedure Modify
             (  Node         : in out Leaf_Node;
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
            (  Left  : Leaf_Node;
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
                Node        : Leaf_Node
             );
--
-- Tests -- Overrides Fuzzy.Graph...
--
   overriding
   function Tests
            (  Node    : Leaf_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean;

   pragma Inline (Get_Child);
   pragma Inline (Get_Children_Number);
   pragma Inline (Get_Class);
   pragma Inline (Get_Type);
   pragma Inline (Is_Modified);
   pragma Inline (Reset_Modified);
   pragma Inline (Tests);

end Fuzzy.Graph.Memory_Resident.Leaf;
