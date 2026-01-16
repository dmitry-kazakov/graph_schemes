--                                                                    --
--  package Fuzzy.Graph             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  07:55 21 Jul 2016  --
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
--  This  package  defines the type Graph_Node. This is the most general
--  node type. The type is abstract and all graph node types are derived
--  from  it.  Each  node  has  a  feature  associated with it, i.e. the
--  feature it tests. The nodes of a classification tree are  accessible
--  through handles of the type Node_Handle defined in the child package
--  Fuzzy.Graph.Handle.  All  public  operations have parameters of only
--  Node_Handle type.
--
with Fuzzy.Classifier;      use Fuzzy.Classifier;
with Fuzzy.Feature;         use Fuzzy.Feature;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;
with Indicator;             use Indicator;
with Object.Archived;       use Object.Archived;

with Ada.Finalization;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Object.Handle;

package Fuzzy.Graph is
   pragma Elaborate_Body (Fuzzy.Graph);
--
-- Graph_Class -- The prefix of all graph-scheme classes
-- Node_Class  -- The prefix of all node classes
--
   Graph_Class : constant String := "Graph-scheme.";
   Node_Class  : constant String := Graph_Class & "Node.";

   type Node_Type is (Tree_Leaf, Tree_Cluster, Tree_Branch);
   type Node_Modification is (Union, Intersection, Replacement);
--
-- Graph_Node -- The base type of all nodes
--
   type Graph_Node (Cardinality : Positive) is
      abstract new Deposit with private;
   type Graph_Node_Ptr is access Graph_Node'Class;
   for Graph_Node_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
   type Graph_Node_Ref is access constant Graph_Node'Class;
   for Graph_Node_Ref'Storage_Pool use Deposit_Ptr'Storage_Pool;
--
-- Node_Ptr_Array -- Array of graph node pointers
--
   type Node_Ptr_Array is array (Integer range <>) of Graph_Node_Ptr;
--
-- Node_Factory -- The base type for node factories
--
-- A  node  factory object is used to create graph nodes. This allows to
-- have  different  implementations  of  nodes selected according to the
-- factory used to create them.
--
   type Node_Factory is
      abstract new Ada.Finalization.Controlled with private;
   type Node_Factory_Ptr is access all Node_Factory'Class;
   type Domain_Subset_Ptr is access Domain_Subset;
--
-- Node_Modification_Data -- Information used for nodes modification
--
--    Lesson - The training set
--    Length - Of the training set external name
--    Viewer - An indication object
--
-- The meaning of some fields:
--
--    Factory - The node factory used to create new nodes when there  is
--              no  other  node to borrow it from. The initial value may
--              be replaced as nodes get created.
--
   type Node_Modification_Data is new Training_Data with record
      Factory : Node_Factory_Ptr;
   end record;
--
-- Classify -- Classification
--
--    Node   - The node
--    Data   - The classification data
--    Weight - Of the node
--
-- This  procedure uses the graph rooted in Node to classify one example
-- from  a  fuzzy  training  set.  The  parameter  Data  specifies   the
-- classification parameters. Any node has to implement this procedure.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, node or result cardinality
--
   procedure Classify
             (  Node   : Graph_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             )  is abstract;
--
-- Create -- Create a branch node
--
--    Factory   - The node factory
--    Feature   - The feature used by the node
--    Children  - The array of successors
--  [ Weights ] - Of the successors
--
-- This  function  creates  a branch node having children. The parameter
-- Factory is one used  to  create  the  node.  Note  that  the  factory
-- returned  by  Get_Factory applied to the created node may differ from
-- one pointed by the parameter Factory.  The  parameter  Feature  is  a
-- pointer  to the feature tested by the node. The parameter Children is
-- the  array  of  successors  which  for each domain point value of the
-- feature contains either null or a pointer to  some  child  node.  The
-- created branch will have an arc  leading  to  each  child  node.  The
-- parameter Weights when specified, determines the weights of the arcs.
-- It is a fuzzy set over the set of children. From each child it  tells
-- the possibility of the  arc  leading  to  the  child.  When  omitted,
-- weights  are  assumed  to be 1 for non-null children and 0 otherwise.
-- All children shall belong to the same graph. There shall be at  least
-- one  child node in Children with the weight greater than 0. Otherwise
-- Constraint_Error  is  propagated. It is also propagated when children
-- nodes belong to no or different graphs, or when Weights  or  Children
-- have wrong range.
--
-- Returns :
--
--    A pointer to the created node
--
-- Exceptions :
--
--    Constraint_Error - Wrong  feature,  illegal  children nodes, wrong
--                       cardniality
--    End_Error        - Training aborted
--
   function Create
            (  Factory  : not null access Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Set
            )  return Graph_Node_Ptr is abstract;
   function Create
            (  Factory  : not null access Node_Factory;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr is abstract;
--
-- Create -- Create a leaf node
--
--    Factory      - The node factory
--    Classes      - The class-feature (a pointer to)
--    Distribution - Of the classes
--
-- The classification the newly created node will give is:
--
--    Possibility - Distribution.Possibility
--    Necessity   - Distribution.Necessity
--
-- The node is created not connected to any parent node. To do  so,  use
-- Connect. Note that the parent shall belong to same graph.  Note  that
-- the factory returned by Get_Factory applied to the created node  does
-- not necessarily one pointed by the parameter Factory.
--
-- Returns :
--
--    A pointer to the created node
--
-- Exceptions :
--
--    Constraint_Error - Wrong cardinality of Distribution
--    End_Error        - Training aborted
--
   function Create
            (  Factory      : not null access Node_Factory;
               Classes      : Feature_Object'Class;
               Distribution : Classification
            )  return Graph_Node_Ptr is abstract;
--
-- Finalize -- Destructor
--
--    Node - The node
--
-- The  procedure  checks  that  the finalized node is detached from any
-- graph.
--
   overriding
   procedure Finalize (Node : in out Graph_Node);
--
-- Find -- Find a node testing a feature
--
--    Node    - A graph node
--    Feature - A pointer to a feature object
--
-- This  function is used to find any node testing Feature and belonging
-- to the same graph as Node.
--
-- Returns :
--
--    A pointer to a graph node testing Feature or null
--
   function Find
            (  Node    : Graph_Node;
               Feature : Feature_Object'Class
            )  return Graph_Node_Ptr is abstract;
--
-- Find -- Find a node testing a feature
--
--    Parent - A parent graph node
--    Child  - A child graph node
--    Index  - To start search at
--
-- Returns :
--
--    Index of the parent's branch leading to Child or 0
--
   function Find
            (  Parent : Graph_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural is abstract;
--
-- Get_Child -- Get a child node of a node
--
--    Node  - The node
--    Index - Of the child (1..)
--
-- This  function  returns  the  child  node associated with the feature
-- domain point  specified  by  Index.  Note  that  same  child  may  be
-- associated  with  more  than  one domain point of the tested feature.
--
-- Returns :
--
--    A pointer to a child node or null
--
   function Get_Child (Node : Graph_Node; Index : Positive)
      return Graph_Node_Ptr is abstract;
--
-- Get_Children_Number -- Get number of children nodes
--
--    Node      - The node
--    Immediate - Count only immediate successors
--
-- Returns :
--
--    The number of successors
--
   function Get_Children_Number
            (  Node      : Graph_Node;
               Immediate : Boolean := True
            )  return Natural is abstract;
--
-- Get_Classes -- Get the class feature of a graph
--
--    Node - A node of a graph
--
-- Each graph has a dedicated feature describing the set of classes. The
-- result  of  a  classification  is  a fuzzy set over the class feature
-- domain.
--
-- Returns :
--
--    A pointer to the class feature
--
-- Exceptions :
--
--    Constraint_Error - The node belongs to no graph
--
   function Get_Classes (Node : Graph_Node)
      return Feature_Object_Ptr is abstract;
--
-- Get_Distribution -- Get distribution of a node
--
--    Node    - The node
--    Context - Feature data context
--
-- The result is a fuzzy classification of the  feature  tested  by  the
-- node. For a leaf node it is a classification. For other nodes  it  is
-- the weights of the arcs leading out of the node.
--
-- Returns :
--
--    The distribution
--
   function Get_Distribution (Node : Graph_Node)
      return Classification is abstract;
--
-- Get_Examples -- Extract a set of examples
--
--    Node   - The root node of the subtree
--    Lesson - The handle to a fuzzy teaching set
--    Image  - The type of image to be extracted
--    Viewer - A progress indication object
--
-- This  procedure  extracts  training examples from the graph rooted in
-- Node.  Each graph has a corresponding training set which exhaustively
-- describes it. The extracted examples are added to  the  training  set
-- specified  by the handle Lesson. The parameter Image determines which
-- feature  images  have to be defined by the extracted examples. Though
-- it  should  have  no  effect   on   the   class-feature,   of   which
-- classifications (has-in, has-not) are defined.
--
-- Exceptions :
--
--    Constraint_Error - Node of no graph
--    Data_Error       - I/O error in Lesson
--    End_Error        - Operation was aborted by Viewer
--
   procedure Get_Examples
             (  Node   : Graph_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is abstract;
--
-- Get_Factory -- Get node factory used to create similar nodes
--
--    Node - A node
--
-- This  function  returns  the node factory which can be used to create
-- nodes compatible with the given one.
--
-- Returns :
--
--    Node factory
--
   function Get_Factory (Node : Graph_Node)
      return Node_Factory_Ptr is abstract;
--
-- Get_Feature -- Get the feature tested by the node
--
--    Node - The node
--
-- Returns :
--
--    A pointer to the feature tested by Node
--
-- Exceptions :
--
--    Constraint_Error - Node belongs to no graph
--
   function Get_Feature (Node : Graph_Node)
      return Feature_Object_Ptr is abstract;
--
-- Get_Feature -- Get a feature used in the graph
--
--    Node  - A graph node
--    Index - A positive feature index
--
-- All  features  of  tested  by nodes of a graph are enumerated from 1.
-- They  can  be  queried  using  this  function. The parameter Node can
-- specify  any  node of the graph. The parameter Index should be in the
-- range 1..Get_Features_Number (see).
--
-- Returns :
--
--    A pointer to the feature tested by the graph
--
-- Exceptions :
--
--    Constraint_Error - No such feature
--
   function Get_Feature (Node : Graph_Node; Index : Positive)
      return Feature_Object_Ptr is abstract;
--
-- Get_Features_Number -- Get the number of feature used in the graph
--
--    Node  - A graph node
--
-- Returns :
--
--    The number of the features tested by the graph containing Node
--
   function Get_Features_Number (Node : Graph_Node)
      return Natural is abstract;
--
-- Get_Number_Of_Nodes -- Get the total number of created nodes
--
-- Returns :
--
--    The number of all nodes existing at the moment
--
   function Get_Number_Of_Nodes return Natural;
--
-- Get_Sibling -- Get a node testing the same feature
--
--    Node  - A node
--    Index - The number of a sibling 1..Get_Siblings_Number
--
-- This  function  allows enumeration of a graph layer. A layer consists
-- of the nodes testing the same feature. The parameter Node  identifies
-- one node of the layer. The parameter Index indicates the number of  a
-- node  in  the  layer.  All  nodes of a layer are enumerated from 1 to
-- Get_Siblings_Number. Note that a node is a sibling of itself.
--
-- Returns :
--
--    A pointer to a node testing same feature as Node
--
-- Exceptions :
--
--    Constraint_Error - Node belongs to no graph, or wrong Index
--
   function Get_Sibling (Node : Graph_Node; Index : Positive)
      return Graph_Node_Ptr is abstract;
--
-- Get_Siblings_Number -- Get the number of nodes testing a feature
--
--    Node - A node
--
-- This  function  returns the number of nodes in a graph layer. A layer
-- consists of the nodes testing the same feature.  The  parameter  Node
-- identifies one node of the layer.
--
-- Returns :
--
--    The number of nodes or 0 if Node belongs to no graph
--
   function Get_Siblings_Number (Node : Graph_Node)
      return Natural is abstract;
--
-- Initialize -- Constructor
--
--    Node - The node
--
   procedure Initialize (Node : in out Graph_Node);
--
-- Get_Type -- Get the type of the node
--
--    Node - The node
--
-- Returns :
--
--    The type of the node
--
   function Get_Type (Node : Graph_Node) return Node_Type is abstract;
--
-- Like -- Node equivalence
--
--    Left  - The first, dispatching parameter
--    Right - The second parameter
--
-- Two nodes are equivalent if they give equivalent  classifications  of
-- same training sets. Nodes of lesser cardinality precede the nodes  of
-- greater one. Nodes of different types are ordered  according  to  the
-- External_Tag   attribute.   Classifications  are  compared  by  their
-- classifications  using  "<"  defined  in   this   package.   Branches
-- comparison is shallow. i.e. if the nodes involved are branches,  then
-- their children can be compared as objects (by memory addresses). This
-- function is used in graph layers to factor out similar nodes. Because
-- comparison is shallow the children nodes  have  to  be  factored  out
-- before the parent ones. See also Precedent.
--
-- Returns :
--
--    True if nodes are equivalent
--
   function Like
            (  Left  : Graph_Node;
               Right : Graph_Node'Class
            )  return Boolean is abstract;
--
-- Modify -- Node modification
--
--    Parent      - To be modified
--    Data        - The node modification context
--    Children    - The nodes to be connected
--    Exclusive   - Node access
--    Combination - The accumulation mode of the example
--    New_Node    - The result
--
-- This  procedure is used to modify a branch node. The parameter Parent
-- is  the  node to be modified. When the result of the operation should
-- change it, then the parameter Exclusive determines whether a new node
-- should  be created. The parameter Children is an array of pointers to
-- the  successors  of  Parent.  The  result  children  are   determined
-- according to the parameter Combination. Children'Length should be the
-- cardinality   of   the   feature   tested  by  the  node.  Otherwise,
-- Constraint_Error  is   propagated.   The   combinations   Union   and
-- Intersection are is defined as follows. With union the node will have
-- a  successor specified in Children or if it is absent then one of the
-- node. Intersection takes the successor node  from  Children  only  if
-- there  already  is a child. Children have to belong to the same graph
-- as  Parent  otherwise Constraint_Error is propagated. If Exclusive is
-- False  then  Modify will never change the node Parent. When Exclusive
-- is  True  Modify is allowed to apply changes to Parent. In both cases
-- the  result  is checked for being equivalent to some already existing
-- node,  which  is used then. New_Node points to the result if and only
-- if it differs from Parent, otherwise  New_Node  is  not  changed.  To
-- determine if New_Node refers to a newly created node, rather  than  a
-- reused one, New_Node.Use_Count should be tested for zero.
--
-- Note :
--
--    An  implementation need not test if any of children is an ancestor
--    of  Parent to report it as an error. It is caller's responsibility
--    to ensure this.
--
-- Exceptions :
--
--    Constraint_Error - Illegal node, wrong cardinality of Children
--
   procedure Modify
             (  Parent      : in out Graph_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean;
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             )  is abstract;
--
-- Modify -- Node modification
--
--    Leaf         - To be modified
--    Data         - The node modification context
--    Distribution - The classification of the example
--    Exclusive    - Node access
--    Combination  - The accumulation mode of the example
--    New_Node     - The result
--
-- This  procedure  is used to modify a leaf node. The parameter Leaf is
-- the  node  to  be modified. The meaning of Exclusive and New_Node are
-- same as above. The  parameter  Distribution  specifies  the  expected
-- classification of the example from Data. It is a combination  of  the
-- current  example   classification   and   the   weight   (conditional
-- possibility) evaluated by the parent nodes. The parameter Combination
-- specifies  how  Distribution  is  combined  with  the  classification
-- delivered by the node. The parameter  Data  determines  the  training
-- context. When the node encapsulates some classifier the latter can be
-- trained on these data. Otherwise the parameter is ignored.
--
-- Exceptions :
--
--    Constraint_Error - Illegal node
--    End_Error        - Learning was aborted
--
   procedure Modify
             (  Leaf         : in out Graph_Node;
                Data         : in out Node_Modification_Data'Class;
                Distribution : Classification;
                Exclusive    : Boolean;
                Combination  : Node_Modification;
                New_Node     : in out Graph_Node_Ptr
             )  is abstract;
--
-- Precedent -- Node equivalence order
--
--    Left  - The first, dispatching parameter
--    Right - The second parameter
--
-- This function have to be compatible with the function Like (see). The
-- comparison implemented is shallow.
--
-- Returns :
--
--    True if Left precedes Right
--
   function Precedent
            (  Left  : Graph_Node;
               Right : Graph_Node'Class
            )  return Boolean is abstract;
--
-- Tests -- Check if the graph tests a feature
--
--    Node    - The root node
--    Feature - The feature (a pointer to)
--
-- A graph tests feature if at least one node of the graph does it.
--
-- Returns :
--
--    True  - The graph rooted in Node tests the feature
--    False - Otherwise
--
   function Tests
            (  Node    : Graph_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean is abstract;
--
-- To_Graph_Node_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to graph node
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a graph node
--
   function To_Graph_Node_Ptr (Ptr : Deposit_Ptr) return Graph_Node_Ptr;
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
          (  Graph_Node_Ptr,
             Deposit_Ptr
          );
private
   pragma Inline (To_Graph_Node_Ptr);

   type Sequence_No is mod 2**32;
--
-- Get_Sequence_No -- Get a new sequence number
--
   function Get_Sequence_No return Sequence_No;
   pragma Inline (Get_Sequence_No);
--
-- Graph_Node -- The graph node type
--
--    Self     - Pointer to the node
--    Sequence - The node sequence number
--
-- Upon  creation  Self  shall  be  set  to  point  to the object. It is
-- necessary  because  there is no safe way of obtaining a pool-specific
-- pointer from an object.
--
   type Graph_Node (Cardinality : Positive) is
      abstract new Deposit with
   record
      Self     : Graph_Node_Ptr;
      Sequence : Sequence_No := Get_Sequence_No;
   end record;
--
-- Is_Modified -- Overrides Object.Archived...
--
   function Is_Modified (Node : Graph_Node) return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   procedure Reset_Modified (Node : in out Graph_Node);

   type Node_Factory is
      abstract new Ada.Finalization.Controlled with null record;

   procedure Free is new
      Ada.Unchecked_Deallocation (Graph_Node'Class, Graph_Node_Ptr);
--
-- Release -- Decrease use count of a node
--
--    Node - A pointer to
--
-- The node is destoryed if no more referenced
--
   procedure Release (Node : Graph_Node_Ptr);
--
-- Handles -- To graph nodes
--
   package Handles is new Object.Handle (Graph_Node, Graph_Node_Ptr);
--
-- < -- Order of classifications when nodes are compared
--
   function "<" (Left, Right : Classification) return Boolean;
   pragma Inline ("<");

end Fuzzy.Graph;
