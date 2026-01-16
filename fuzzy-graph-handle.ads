--                                                                    --
--  package Fuzzy.Graph.Handle      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
--                                                                    --
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
--
--  This  package  defines the type Node_Handle used to access the nodes
--  of a graph. It defines public operations on graph nodes accessed via
--  handles.  The  child  package  Layer  defines  the public operations
--  involving sets of graph nodes.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Indicator.Handle;      use Indicator.Handle;

with Deposit_Handles;

package Fuzzy.Graph.Handle is
--
-- Node_Handle -- Handle to a graph node with operations defined on it
--
   type Node_Handle is tagged private;
   type Node_Array is array (Integer range <>) of Node_Handle;
   No_Node : constant Node_Handle;
--
-- Classify -- Classification of a training example
--
--    Node       - The root graph node (a handle to)
--    Lesson     - The training set (a handle to)
--    Example    - The example to classify
--    Image      - The image to be classified
--    Generalize - The nodes generalization mode
--    Threshold  - The confidence threshold
--
-- This functions use the graph rooted in Node to classify  one  example
-- from a fuzzy training set Lesson. The example  is  specified  by  the
-- parameter Example. Images not represented in  the  training  set  are
-- assumed   unknown.   The  result  of  classification  is  a  pair  of
-- distributions  of  confidence  factors  over  the  classes.  It is an
-- intuitionistic classification. The set of classes is the  domain  set
-- of the class feature (see Get_Classes). The parameter Image specifies
-- the  image  to classify. The parameter Generalize advices how gaps in
-- the  training set should be filled upon classification. The parameter
-- Threshold specifies the truncation level. All confidence factors less
-- than Threshold are treated as 0. Higher values of Threshold may speed
-- up  classification,  though  make it less precise. The parameter Node
-- can be an invalid handle in which case the result  of  classification
-- is uncertain.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - An invalid node
--
   function Classify
            (  Node       : Node_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Image      : Image_Type;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Classification;
--
-- Classify -- Classification of a training example
--
--    Node    - The root graph node (a handle to)
--    Context - The data context
--    Image   - The image to be classified
--
-- These  functions use the graph rooted in Node to classify one example
-- from  a fuzzy training set. The parameter Context is the data context
-- the training set is associated with.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - An invalid node
--
   function Classify
            (  Node    : Node_Handle;
               Context : not null access
                         Classification_Parameters'Class;
               Image   : Image_Type
            )  return Classification;
--
-- Find -- Find a child node
--
--    Parent - The parent node handle
--    Child  - A child node handle
--    Index  - To start search from
--
-- This function is used to find a child node. If the node  Child  is  a
-- direct successor of Parent, then the  result  is  the  index  of  the
-- domain  set  value same as in the function Get_Child. It returns 0 if
-- any of the handles is invalid.
--
-- Returns :
--
--    The index of the child node or 0
--
   function Find
            (  Parent : Node_Handle;
               Child  : Node_Handle;
               Index  : Positive
            )  return Natural;
--
-- Find -- Find a node testing a feature
--
--    Node    - A handle to a graph node
--    Feature - A handle to a feature
--
-- This function is used to find any node testing Feature  that  belongs
-- to the same graph as Node.
--
-- Returns :
--
--    A  handle  to a graph node testing Feature or an invalid handle if
--    there is no one
--
-- Exceptions :
--
--    Contraint_Error - Node is an invalid handle
--
   function Find
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Node_Handle;
--
-- Get_Cardinality -- Get cardinality of a node
--
--    Node - The node handle
--
-- The cardinality of a node is the cardinality of the feature it tests.
-- A leaf node also "tests" a feature, which is the class set.
--
-- Returns :
--
--    The node cardinality
--
-- Exceptions :
--
--    Constraint_Error - Invalid node handle
--
   function Get_Cardinality (Node : Node_Handle) return Positive;
--
-- Get_Child -- Get a child node of a node
--
--    Node  - The node handle
--    Index - Of the child (1..Get_Cardinality (Node))
--
-- This  function  returns  the  child  node associated with the feature
-- domain point  specified  by  Index.  Note  that  same  child  may  be
-- associated with more than one domain point of the tested feature.
--
-- Returns :
--
--    The handle to the child node or No_Node
--
-- Exceptions :
--
--    Constraint_Error - Invalid node handle
--
   function Get_Child
            (  Node  : Node_Handle;
               Index : Positive
            )  return Node_Handle;
--
-- Get_Children_Number -- Get number of children nodes
--
--    Node      - The node handle
--    Immediate - Count only immediate successors
--
-- Returns :
--
--    The number of successors of the node
--
   function Get_Children_Number
            (  Node      : Node_Handle;
               Immediate : Boolean := True
            )  return Natural;
--
-- Get_Class -- Get the graph class
--
--    Node  - The node handle
--
-- Returns :
--
--    The node class
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Class (Node : Node_Handle) return String;
--
-- Get_Classes -- Get the class feature of a graph
--
--    Node - The graph (a handle to)
--
-- Each graph has a dedicated feature describing the set of classes. The
-- result  of  a  classification  is  a fuzzy set over the class feature
-- domain.
--
-- Returns :
--
--    The handle of the class feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or node
--
   function Get_Classes (Node : Node_Handle)
      return Feature_Handle;
--
-- Get_Distribution -- Get distribution of a node
--
--    Node - A handle to the node
--
-- The result is a classification of the feature tested by the node. For
-- a leaf node it is a classification. For other nodes it is the weights
-- of the arcs leading out of the node.
--
-- Returns :
--
--    The distribution
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or node
--
   function Get_Distribution (Node : Node_Handle)
      return Classification;
--
-- Get_Examples -- Extract a set of examples
--
--    Node   - A handle to the node
--    Lesson - The handle to a fuzzy teaching set
--    Image  - The type of image to be extracted
--    Viewer - A progress indication object or a handle to
--
-- This  procedure  extracts  training examples from the graph rooted in
-- Node.  Each graph has a corresponding training set which exhaustively
-- describes it. The extracted examples are added to  the  training  set
-- specified  by the handle Lesson. The parameter Image determines which
-- feature  images  have to be defined by the extracted examples. Though
-- it has no effect  on  the  class-feature,  of  which  classifications
-- (has-in, has-not) are defined. Viewer can be an invalid handle.
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or node
--    End_Error        - Operation was aborted
--    Data_Error       - I/O error
--
   procedure Get_Examples
             (  Node   : Node_Handle;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : Indicator_Handle
             );
   procedure Get_Examples
             (  Node   : Node_Handle;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Feature -- Get the feature tested by a node
--
--    Node - The node handle
--
-- Each  node  tests  some feature. For a leaf node it is the class set.
-- This function returns a handle to the feature the node tests.
--
-- Returns :
--
--    The handle of the feature tested by Node
--
-- Exceptions :
--
--    Constraint_Error - Invalid node handle
--
   function Get_Feature (Node : Node_Handle) return Feature_Handle;
--
-- Get_Number_Of_Nodes -- Get the total number of created nodes
--
-- Returns :
--
--    The number of all nodes existing at the moment
--
   function Get_Number_Of_Nodes return Natural renames
      Fuzzy.Graph.Get_Number_Of_Nodes;
--
-- Get_Type -- Get the type of a node
--
--    Node - The node handle
--
-- For  a  root node Tree_Root is returned. For a branch node the result
-- is Tree_Branch or Tree_Cluster. Tree_Leaf is returned for leaf nodes.
--
-- Returns :
--
--    The type of the node
--
-- Exceptions :
--
--    Constraint_Error - Invalid node handle
--
   function Get_Type (Node : Node_Handle)
      return Node_Type;
--
-- Invalidate -- Detach handle from the object
--
--    Node - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Node : in out Node_Handle);
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Node - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Node : Node_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Node - The handle
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Node : Node_Handle) return Graph_Node_Ptr;
--
-- Ref -- Get handle to a node object
--
--    Node - The graph node
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Node : Graph_Node_Ptr) return Node_Handle;
--
-- Ref -- Set a handle to a node object
--
--    Handle - The handle to set
--    Node   - The node object
--
   procedure Ref
             (  Handle : in out Node_Handle;
                Node   : Graph_Node_Ptr
             );
--
-- Tests -- Check if graph tests a feature
--
--    Node    - The root node handle
--    Feature - The handle of a feature
--
-- A graph tests a feature if at least one node of the graph does it.
--
-- Returns :
--
--    True  - The graph rooted in Node tests the feature
--    False - Otherwise
--
-- Exceptions :
--
--    Constraint_Error - Invalid node handle
--
   function Tests
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Boolean;
--
-- To_Deposit_Handle -- Pointer conversion
--
--    Node - A handle to
--
-- Returns :
--
--    Handle to archived object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Deposit_Handle (Node : Node_Handle)
      return Deposit_Handles.Handle;
--
-- To_Node_Handle -- Handle conversion
--
--    Node - A handle to
--
-- Returns :
--
--    A handle to the graph node
--
-- Exceptions :
--
--    Constraint_Error - The object is not a node, or invalid handle
--
   function To_Node_Handle
            (  Node : Deposit_Handles.Handle
            )  return Node_Handle;
--
-- <, <=, =, >=, > -- Comparisons
--
--    Left  - The first argument
--    Right - The second argument
--
-- Only  valid  handles  are comparable. However it is exception-safe to
-- compare  No_Node for equality. In all other cases Constraint_Error is
-- propagated.
--
-- Returns :
--
--    The result of comparison of the objects
--
-- Exceptions :
--
--    Constraint_Error - One of arguments is an invalid handle
--
   function "<"  (Left, Right : Node_Handle) return Boolean;
   function "<=" (Left, Right : Node_Handle) return Boolean;
   function "="  (Left, Right : Node_Handle) return Boolean;
   function ">=" (Left, Right : Node_Handle) return Boolean;
   function ">"  (Left, Right : Node_Handle) return Boolean;

private
   type Node_Handle is new Handles.Handle with null record;

   No_Node : constant Node_Handle :=
                (Handles.Handle with null record);

   pragma Inline (Find);
   pragma Inline (Get_Cardinality);
   pragma Inline (Get_Child);
   pragma Inline (Get_Children_Number);
   pragma Inline (Get_Classes);
   pragma Inline (Get_Distribution);
   pragma Inline (Get_Feature);
   pragma Inline (Invalidate);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (Tests);
   pragma Inline (To_Deposit_Handle);
   pragma Inline (To_Node_Handle);
   pragma Inline ("<=", "<", "=", ">", ">=");

end Fuzzy.Graph.Handle;
