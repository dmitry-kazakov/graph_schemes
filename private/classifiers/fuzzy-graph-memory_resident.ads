--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident                 Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  09:19 27 Jun 2015  --
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

with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;

with Fuzzy.Feature.Classificatory.Clustering;
with Fuzzy.Graph.Node_Class_Sets;
with Generic_Map;

package Fuzzy.Graph.Memory_Resident is
--
-- Memory_Node -- The base type of graph nodes resident in memory
--
   type Memory_Node is abstract new Graph_Node with private;
--
-- Header_Handle -- Handles to graph headers
--
-- Each graph has a header which holds data shared among nodes and  node
-- layers. Headers are objects  to  be  collected  when  no  more  used.
-- Header_Handle is the type of the references.
--
   type Header_Handle is new Ada.Finalization.Controlled with private;
--
-- Factory -- The default factory for memory-resident nodes
--
   function Factory return Node_Factory_Ptr;
--
-- Attach -- Add node to a graph
--
--    Node    - To be attached
--    Header  - The graph header to attach the node (a handle to)
--    Feature - The feature it tests
--
-- When the attached node has a similar node in the graph, Node  is  set
-- to point  to  it.  Attach  deletes  duplicated  nodes  automatically.
-- Program_Error is propagated when Use_Count is  not  zero  when  being
-- deleted.
--
-- Exceptions :
--
--    Constraint_Error - Node is null or not a memory resident node
--    Program_Error    - Node cannot be collected
--    Use_Error        - Node depends on the graph
--
   procedure Attach
             (  Node    : in out Graph_Node_Ptr;
                Header  : Header_Handle;
                Feature : Feature_Object'Class
             );
--
-- Check_Child -- Check a child node
--
--    Parent - The parent node
--    Child  - Its potential child
--
-- This  procedure  checks  if  Child  could  be  connected  to  Parent.
-- Constraint_Error  is  propagated when Child is not memory-resident or
-- else belongs to other or no graph.
--
-- Exceptions :
--
--    Constraint_Error - Wrong child
--
   procedure Check_Child
             (  Parent : Memory_Node;
                Child  : Graph_Node'Class
             );
--
-- Finalize -- Destructor
--
   overriding
   procedure Finalize (Node : in out Memory_Node);

private
--
-- Graph_Layer -- The graph layer
--
-- The graph layer is a set of all nodes testing a feature. The node set
-- is  obtained through an instantiation of Generic_Set. Elements of the
-- set are class-wide pointers. They are compared using addresses of the
-- items they point to (functions Less and Equal).
--
   type Graph_Layer;
   type Graph_Layer_Ptr is access Graph_Layer'Class;
--
-- Layer_Maps -- Sets of graph layers
--
   package Layer_Maps is
      new Generic_Map
          (  Key_Type    => Feature_ID,
             Object_Type => Graph_Layer_Ptr,
             Increment   => 128
          );
--
-- First_Node_Factory -- The default factory of graph nodes
--
-- This factory is used when the first node of a graph is created.
--
   type First_Node_Factory is new Node_Factory with null record;
--
-- Graph_Header -- Header of a graph
--
-- The header contains information common to all nodes. For instance the
-- list of used features could be stored there as the layers map.
--
   type Header_Change_No is mod 2**32;
   type Graph_Header is new Deposit with record
      Classes  : Feature_Handle;
      Sequence : Header_Change_No := 0;
      Layers   : Layer_Maps.Map;
      Sorter   : Fuzzy.Feature.Classificatory.Clustering.
                    Frequency_To_Value_Maps.Set;
   end record;
   type Graph_Header_Ptr is access Graph_Header'Class;
--
-- Memory_Node -- The type of graph nodes resident in memory
--
   type Memory_Node is abstract new Graph_Node with record
      Layer : Graph_Layer_Ptr := null;
   end record;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory  : not null access First_Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory  : not null access First_Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory      : not null access First_Node_Factory;
               Classes      : Feature_Object'Class;
               Distribution : Classification
            )  return Graph_Node_Ptr;
--
-- Create -- Layer factory
--
--    Header - The graph header
--
-- The  caller  is reponsible to attach the result to Header and bind it
-- to the feature.
--
-- Returns :
--
--    A pointer to the new layer compatible with Header
--
   function Create (Header : Graph_Header) return Graph_Layer_Ptr;
--
-- Finalize -- Destructor
--
--    Header - To be destroyed
--
   overriding
   procedure Finalize (Header : in out Graph_Header);
--
-- Find -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Find
            (  Node    : Memory_Node;
               Feature : Feature_Object'Class
            )  return Graph_Node_Ptr;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Header : Graph_Header) return String;
--
-- Get_Classes -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Get_Classes (Node : Memory_Node) return Feature_Object_Ptr;
--
-- Get_Factory -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Get_Factory (Node : Memory_Node)
      return Node_Factory_Ptr;
--
-- Get_Feature -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Get_Feature (Node : Memory_Node) return Feature_Object_Ptr;
--
-- Get_Feature -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Get_Feature (Node : Memory_Node; Index : Positive)
      return Feature_Object_Ptr;
--
-- Get_Features_Number -- Overrides Fuzzy.Graph.Node...
--
   overriding
   function Get_Features_Number (Node : Memory_Node) return Natural;
--
-- Get_Layer -- Find graph layer testing a feature
--
--    Header  - The graph header
--    Feature - The feature (a pointer to)
--
-- Returns :
--
--    The pointer to the layer testing the feature or null
--
   function Get_Layer
            (  Header  : Graph_Header;
               Feature : Feature_Object'Class
            )  return Graph_Layer_Ptr;
--
-- Get_Referents -- Overrides Object.Archived...
--
-- The  dependency list consists of only the class-feature. The distinct
-- features of the layers are referenced by nodes.
--
   overriding
   procedure Get_Referents
             (  Header : Graph_Header;
                List   : in out Deposit_Container'Class
             );
--
-- Get_Sibling -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Sibling (Node : Memory_Node; Index : Positive)
      return Graph_Node_Ptr;
--
-- Get_Siblings_Number -- Overrides Fuzzy.Graph...
--
   overriding
   function Get_Siblings_Number (Node : Memory_Node) return Natural;
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Header : Graph_Header) return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Header : in out Graph_Header);
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Header  : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Header      : Graph_Header
             );
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Graph_Header_Ptr,
             Deposit_Ptr
          );
--
-- To_Graph_Header_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to graph header
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a graph header
--
   function To_Graph_Header_Ptr (Ptr : Deposit_Ptr)
      return Graph_Header_Ptr;
--
-- Header_Handles -- Handles to graph headers
--
   package Header_Handles is
      new Object.Handle (Graph_Header, Graph_Header_Ptr);
   use Header_Handles;
   type Header_Handle is new Ada.Finalization.Controlled with record
      Handle : Header_Handles.Handle;
   end record;
--
-- Graph_Layer -- Type completion
--
--    Feature - The feature tested by the layer
--    Header  - Handle of the graph header to which the layer belongs
--    Nodes   - The set of layer nodes (sorted part)
--
-- Each layer is a node factory as well.
--
   type Graph_Layer is new Node_Factory with record
      Feature : Feature_Handle;
      Header  : Header_Handle;
      Nodes   : Node_Class_Sets.Set;
   end record;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory  : not null access Graph_Layer;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory  : not null access Graph_Layer;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr;
--
-- Create -- Overrides Fuzzy.Graph...
--
   overriding
   function Create
            (  Factory      : not null access Graph_Layer;
               Classes      : Feature_Object'Class;
               Distribution : Classification
            )  return Graph_Node_Ptr;
--
-- Get_Feature -- Get the feature tested by the nodes of a layer
--
--    Layer - The graph layer
--
-- Returns :
--
--    The handle of the feature tested by the nodes of the layer
--
   function Get_Feature (Layer : Graph_Layer)
      return Feature_Object_Ptr;

   procedure Free is new
      Ada.Unchecked_Deallocation (Graph_Header'Class, Graph_Header_Ptr);
--
-- Add -- A node to the list of
--
--    List    - Nodes list (an array of)
--    Free    - The first free element of the array
--    Node    - The node to add
--    Pointer - The index of the node in the array
--
   procedure Add
             (  List    : in out Node_Ptr_Array;
                Free    : in out Integer;
                Node    : Graph_Node_Ptr;
                Pointer : out Integer
             );
--
-- Check_Child -- Check if a child node can be connected
--
--    Header - The parent node or the graph header
--    Child  - The child node to test
--
-- Exceptions :
--
--    Constraint_Error - Child cannot be connected to Parent
--
   procedure Check_Child
             (  Header : Header_Handle;
                Child  : Graph_Node'Class
             );

   pragma Inline (Get_Class);
   pragma Inline (Get_Classes);
   pragma Inline (Get_Feature);
   pragma Inline (To_Graph_Header_Ptr);
end Fuzzy.Graph.Memory_Resident;

