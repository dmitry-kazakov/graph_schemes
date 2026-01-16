--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Layer                    Luebeck            --
--  Interface                                      Winter, 2002       --
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
--  This package defines operations that involve  several  graph  nodes.
--  Usually they are functions returning a set of  graph  nodes  sharing
--  some property. 
--
with Fuzzy.Graph.Handle.Container;
with Fuzzy.Feature.Handle.Container;

package Fuzzy.Graph.Handle.Layer is
   pragma Elaborate_Body (Fuzzy.Graph.Handle.Layer);
--
-- Connect -- Connect node
--
--    Parent - The parent node (a handle to)
--    Child  - The connected node (a handle to)
--    Index  - The connection point
--  [ Data ] - The node modification context to use
--
-- This procedure connects the node specified by the handle Child to the
-- node Parent.  As  the  result  of  the  parent  node  will  have  arc
-- Parent->Child connected to the  point  Index.  It  is  the  index  in
-- 1..Cardinality of a value of the feature tested by the parent node. A
-- child node can be connected to same parent at more  than  one  point.
-- Both Parent and Child shall belong to a graph and it must be the same
-- graph, otherwise Constraint_Error is propagated. Constraint_Error  is
-- also propagated when either Parent or  Child  is  invalid  handle  or
-- Parent is not a branch node. The connected node cannot be an ancestor
-- of Parent. Otherwise, Use_error is propagated. Note also that if  the
-- operation changes Parent, then these changes are local to the  handle
-- Parent. All other users of the modified node will continue to observe
-- the old node. In particular the Parent's parent. It means that Parent
-- have to be re-connected to its parent to make the latter aware of the
-- changes  made.  The  parameter Data is the node modification context,
-- which when omitted is created as necessary. 
--
-- Exceptions :
--
--    Constraint_Error - Illegal index or node
--    Use_Error        - Parent is a descendant of Child
--
   procedure Connect
             (  Parent : in out Node_Handle;
                Child  : Node_Handle;
                Index  : Positive;
                Data   : in out Node_Modification_Data'Class 
             );
   procedure Connect
             (  Parent : in out Node_Handle;
                Child  : Node_Handle;
                Index  : Positive
             );
--
-- Get_Children -- Get direct descendant nodes
--
--    Node - A handle to the node
--
-- This function returns the set of handles of all immediate children of
-- the node. For a leaf node the result is an empty set. An empty set is
-- also returned when Node is an invalid handle. 
--
-- Returns :
--
--    The set of children handles
--
   function Get_Children (Node : Node_Handle)
      return Fuzzy.Graph.Handle.Container.Set;
--
-- Get_Elder_Feature -- Of the children
--
--    Node - The node (a handle to)
--
-- This function returns the elder feature tested by  non-leaf  children
-- of the node. For two nodes A and B the feature tested by A  is  elder
-- than one of B if B is a descendant of A. The function tries  to  find
-- the  most  elder feature tested by non-leaf nodes descending from the
-- Node. No_Feature is returned if there are only leaf nodes. 
--
-- Returns :
--
--    Feature (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--
   function Get_Elder_Feature (Node : Node_Handle)
      return Feature_Handle;
--
-- Get_Feature -- Get a feature used in the graph
--
--    Node  - A handle to a graph node
--    Index - A positive feature index
--
-- All  features  of  tested  by nodes of a graph are enumerated from 1.
-- They  can  be  queried  using  this  function. The parameter Node can
-- specify  any  node of the graph. The parameter Index should be in the
-- range 1..Get_Features_Number (see).  
--
-- Returns :
--
--    A handle to the feature tested by the graph
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, no such feature
--
   function Get_Feature (Node : Node_Handle; Index : Positive)
      return Feature_Handle;
   pragma Inline (Get_Feature);
--
-- Get_Features -- Get the set of features tested by a graph
--
--    Node - A handle to
--
-- Returns :
--
--    The set of handles to the features tested by the graph
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Features (Node : Node_Handle)
      return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Features_Number -- Get the number of feature used in the graph
--
--    Node  - A handle to a graph node
--
-- Returns :
--
--    The number of the features tested by the graph containing Node
--
   function Get_Features_Number (Node : Node_Handle) return Natural;
   pragma Inline (Get_Features_Number);
--
-- Get_Layer -- Get all nodes testing a feature
--
--    Node    - A handle to a graph node
--    Feature - A handle to the feature
--
-- This function returns the set of handles of all graph  nodes  testing
-- Feature. The graph can be identified by any of its nodes. 
--
-- Returns :
--
--    The set of handles to the nodes testing the feature
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--
   function Get_Layer
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Fuzzy.Graph.Handle.Container.Set;
--
-- Get_Layer_Size -- Get number of nodes testing a feature
--
--    Node    - A handle to a graph-scheme node
--    Feature - A handle to a feature
--
-- This function returns the number of the graph nodes testing  Feature.
-- The  graph can be identified by any of its nodes. Zero is returned if
-- the feature is not tested by the graph. 
--
-- Returns :
--
--    The number of nodes
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or node (not in a graph)
--
   function Get_Layer_Size
            (  Node    : Node_Handle;
               Feature : Feature_Handle
            )  return Natural;
--
-- Get_Parents -- Get direct ancestor nodes
--
--    Node - A handle to
--
-- This  function returns the set of handles to all immediate parents of
-- the node. For a root node the result is an empty set. An empty set is
-- also returned when the node handle is invalid.
--
-- Returns :
--
--    The set of parent handles
--
   function Get_Parents (Node : Node_Handle)
      return Fuzzy.Graph.Handle.Container.Set;
--
-- Get_Sibling -- Get all the nodes testing the same feature
--
--    Node - A handle to a layer's node
--
-- This  function  allows enumeration of a graph layer. A layer consists
-- of the nodes testing the same feature. The parameter Node  identifies
-- one node of the layer. The parameter Index indicates the number of  a
-- node  in  the  layer.  All  nodes of a layer are enumerated from 1 to
-- Get_Siblings_Number.
--
-- Returns :
--
--    A handle to a node testing same feature as Node
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, node or wrong Index
--
   function Get_Sibling (Node : Node_Handle; Index : Positive)
      return Node_Handle;
--
-- Get_Siblings_Number -- Get the number of nodes testing a feature
--
--    Node - A handle to a layer's node
--
-- This  function  returns the number of nodes in a graph layer. A layer
-- consists of the nodes testing the same feature.  The  parameter  Node
-- identifies one node of the layer. 
--
-- Returns :
--
--    The number of nodes or 0 if Node belongs to no graph or invalid
--
   function Get_Siblings_Number (Node : Node_Handle)
      return Natural;
--
-- Get_Size -- Get number of nodes
--
--    Node - A handle to the graph node
--
-- This function returns the number of  nodes  in  the  graph  the  node
-- belongs to. 
--
-- Returns :
--
--    The number of nodes
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or node (not in a graph)
--
   function Get_Size (Node : Node_Handle) return Positive;
--
-- Replace -- Replace a child node
--
--    Parent    - The parent node
--    Old_Child - A child node to be replaced
--    New_Child - The replacement
--  [ Data ]    - The node modification context to use
--
-- This  procedure  replaces all arcs from Parent to Old_Child with ones
-- leading  to New_Child. Nothing happens if Old_Child and New_Child are
-- same or Old_Child is not a child node  of  Parent.  Both  Parent  and
-- New_Child shall belong to a graph and it must be the same  graph,  or
-- else  it  have to be an invalid handle. Otherwise Constraint_Error is
-- propagated. Like Connect (see), Replace creates a new  node  when  it
-- sufficiently  changes  Parent.  This  means that all other interested
-- parties have to be notified about  the  change.  Note  also  that  if
-- replacing  results  in  a  node  with  no  successors,  then  Replace
-- invalidates  Parent.  The  parameter  Data  is  the node modification
-- context, which when omitted is created as necessary.  
--
-- Exceptions :
--
--    Constraint_Error - Parent is invalid, not a branch, the graphs  of
--                       New_Node and Parent are different
--    Use_Error        - New_Child is an ancestor of Parent
--
   procedure Replace
             (  Parent    : in out Node_Handle;
                Old_Child : Node_Handle;
                New_Child : Node_Handle;
                Data      : in out Node_Modification_Data'Class
             );
   procedure Replace
             (  Parent    : in out Node_Handle;
                Old_Child : Node_Handle;
                New_Child : Node_Handle
             );
--
-- Sew -- Remove a node from the graph
--
--    Node - A handle to
--
-- This  procedure  removes the specified node from the graph it belongs
-- to. When the node has a parent, then all arcs from the parent to  the
-- node are removed. All successors of the removed node are  removed  as
-- well  if  they have no other parents. All parents of the removed node
-- are  also  removed  if the node was their last descendant. The handle
-- Node is invalidated upon return from Delete. 
--
   procedure Sew (Node : in out Node_Handle);
          
private
   pragma Inline (Get_Sibling);
   pragma Inline (Get_Siblings_Number);

end Fuzzy.Graph.Handle.Layer;
