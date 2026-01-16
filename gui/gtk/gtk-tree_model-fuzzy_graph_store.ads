--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Tree_Model.                             Luebeck            --
--        Fuzzy_Graph_Store                        Summer, 2006       --
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
--
--  This  package  defines a Tree_View_Model of a fuzzy graph. The graph
--  is  represented  as  a  tree  of  paths  in  the   graph   down   to
--  classification nodes. The tree could have the following outline with
--  the features are Color, Weight, Classes:
--
--  Graph                    | Class 1 |   ...   | Class N |
--  _________________________|_________|_________|_________|
--                           |         |         |         |
--  v Color                  |         |         |         |
--       Red                 |         |         |         |
--     v Black               |         |         |         |
--       v Weight [m]        |         |         |         |
--         v [1.50..1.65]    |         |         |         |
--              Classes      | Pos:Nec |         | Pos:Nec |
--           [1.65..1.80]    |         |         |         |
--           [1.80..2.50]    |         |         |         |
--         Weight [m]        |         |         |         |
--       White               |         |         |         |
--
--  The model has the columns as shown is the following table:
--
--  Row type           | Feature | Row     | Class 1 |   ...
--  ___________________|_________|_________|_________|_________
--  Graph_Node_Icon    | Color   |    -    |         |
--  Graph_Branch_Icon  |         |   Red   |         |
--  Graph_Leaf_Icon    | Classes |    -    | Pos:Nec |
--
--  The types of model's columns:
--
--     0       Row type - GType_String (see Fuzzy.Gtk_Icon_Factory)
--     1       Feature  - GType_String
--     2       Row      - GType_Feature_Value
--     3..N+2  Class n  - GType_Fuzzy_Boolean
--
--  The  column  Feature is an empty string when the row type is branch.
--  Otherwise, it contains the feature name optionally followed  by  its
--  units specification. The column Row contains a feature value. It  is
--  undefined when the row type is  not  branch.  In  a  branch  row  it
--  contains a fuzzy set  of  the  arches  leading  to  the  graph  node
--  represented by the row.
--
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Graph;                    use Fuzzy.Graph;
with Fuzzy.Graph.Handle;             use Fuzzy.Graph.Handle;
with Fuzzy.Logic;                    use Fuzzy.Logic;
with GLib.Values;                    use GLib.Values;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Tree_Model.Abstract_Store;  use Gtk.Tree_Model.Abstract_Store;
with Gtk.Tree_Model.Column_Order;    use Gtk.Tree_Model.Column_Order;

with Generic_Segmented_Stack;
with Gtk.Tree_Model.Generic_Sort;

package Gtk.Tree_Model.Fuzzy_Graph_Store is
--
-- Row_Type -- Types of rows in the view
--
--    Node_Row   - A row corresponding to a branch node (gtk-open)
--    Branch_Row - One that does to a branch (gtk-go-forward)
--    Leaf_Row   - One that does to a leaf node (gtk-jump-to)
--
   type Row_Type is (Node_Row, Branch_Row, Leaf_Row);
--
-- Gtk_Fuzzy_Graph_Store_Record -- A  Tree_View_Model  based  on a fuzzy
--                                 graph
--
   type Gtk_Fuzzy_Graph_Store_Record is
      new Gtk_Root_Tree_Model_Record with private;
   type Gtk_Fuzzy_Graph_Store is
      access all Gtk_Fuzzy_Graph_Store_Record'Class;
--
-- Get_Branch -- Get the branch associated with a row
--
--    Store - A pointer to
--    Iter  - Identifies a row
--
-- Returns :
--
--    The set of arcs leading along the branch
--
-- Exceptions :
--
--    Constraint_Error - Wrong iterator or not a Branch_Row
--
   function Get_Branch
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter
            )  return Fuzzy.Set;
--
-- Get_Feature -- Get the fature associated with
--
--    Store - A pointer to
--    Iter  - Identifies a row
--
-- When Iter is Null_Iter, the result is a handle to the feature  tested
-- by the node returned by Get_Graph.
--
-- Returns :
--
--    A handle to the feature of
--
   function Get_Feature
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return Feature_Handle;
--
-- Get_Graph -- Get the graph associated with
--
--    Store  - A pointer to
--    Iter   - Identifies a row
--    Parent - How to treat branches
--
-- When  Iter is Null_Iter, the result is a handle to the root node. For
-- a row associated with a branch rather than a node, the result depends
-- on the Parent parameter. When it is true, the result  is  the  parent
-- node. Otherwise, it is the child node. When  the  row  is  associated
-- with a node (branch or leaf), the result the node.
--
-- Returns :
--
--    A handle to the node of
--
   function Get_Graph
            (  Store  : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter   : Gtk_Tree_Iter := Null_Iter;
               Parent : Boolean       := True
            )  return Node_Handle;
--
-- Get_Type -- Get type of a row
--
--    Store - A pointer to
--    Iter  - Identifies a row
--
-- When Iter is Null_Iter, the result is of the root node.
--
-- Returns :
--
--    The type of the row
--
-- Exceptions :
--
--    Constraint_Error - Wrong iterator
--
   function Get_Type
            (  Store : not null access Gtk_Fuzzy_Graph_Store_Record;
               Iter  : Gtk_Tree_Iter := Null_Iter
            )  return Row_Type;
--
-- Gtk_New -- Object creation
--
--    Store - The result
--    Graph - A handle to
--
   procedure Gtk_New
             (  Store : out Gtk_Fuzzy_Graph_Store;
                Graph : Node_Handle
             );
--
-- Initialize -- Construction
--
--    Store - A pointer to
--    Graph - A handle to
--
   procedure Initialize
             (  Store : not null access
                        Gtk_Fuzzy_Graph_Store_Record'Class;
                Graph : Node_Handle
             );
private
   type Gtk_Fuzzy_Graph_Record;

   type Parent_Child_Pair is record
      Parent : Node_Handle;  -- The start node of branch
      Child  : Node_Handle;  -- The end node of branch (null if classes)
      Branch : Boolean;      -- The row corresponds to a branch/classes
      Up     : Natural := 0; -- The parent in the tree
      Down   : Natural := 0; -- The first child in the tree
      Next   : Natural := 0; -- The next sibling in the tree
      Prev   : Natural := 0; -- The previous sibling in the tree
   end record;
--
-- Get_Type -- Of the row described by the pair
--
--    Pair - To query
--
-- Returns :
--
--    The type of the row (the column 0)
--
   function Get_Type (Pair : Parent_Child_Pair) return Row_Type;
--
-- Get_Children -- Of a parent
--
--    Pair - To query
--
-- Returns :
--
--    The set of arcs from the pair's parent to the pair's child
--
   function Get_Children (Pair : Parent_Child_Pair) return Fuzzy.Set;
--
-- Get_Classification -- The classification of the row
--
--    Pair   - To query
--    Column - The column number
--    Value  - The value of ((0,0) for non-leafs)
--    Got_It - True if the row is a leaf node
--
-- Returns :
--
--    True if the row has a classification in the column
--
   procedure Get_Classification
             (  Pair   : Parent_Child_Pair;
                Column : GInt;
                Value  : out Fuzzy_Boolean;
                Got_It : out Boolean
             );

   package Pairs_Stack is
      new Generic_Segmented_Stack
          (  Index_Type   => Positive,
             Object_Type  => Parent_Child_Pair,
             Null_Element => (No_Node, No_Node, False, 0, 0, 0, 0)
          );
   use Pairs_Stack.Segmented_Stack;
   type Stack_Ptr is access Stack;

   type Gtk_Fuzzy_Graph_Record is
      new Gtk_Abstract_Model_Record with
   record
      Graph : Node_Handle;
      Tree  : Stack_Ptr;
      Top   : Natural := 0;
   end record;
   type Gtk_Fuzzy_Graph is access all Gtk_Fuzzy_Graph_Record'Class;
--
-- Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Children
               (  Store  : not null access Gtk_Fuzzy_Graph_Record;
                  Parent : Gtk_Tree_Iter
               )  return Gtk_Tree_Iter;
--
-- Finalize -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      procedure Finalize
                (  Store : not null access Gtk_Fuzzy_Graph_Record
                );
--
-- Get_Column_Type -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Column_Type
               (  Store : not null access Gtk_Fuzzy_Graph_Record;
                  Index : Gint
               )  return GType;
--
-- Get_Flags -- Overrides Gtk.Tree_Model.Abstract_Store...
--
-- Returns Tree_Model_Iters_Persist.
--
   overriding
      function Get_Flags
               (  Store : not null access Gtk_Fuzzy_Graph_Record
               )  return Tree_Model_Flags;
--
-- Get_Iter -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Iter
               (  Store : not null access Gtk_Fuzzy_Graph_Record;
                  Path  : Gtk_Tree_Path
               )  return Gtk_Tree_Iter;
--
-- Get_Name -- Of the row's feature described by the pair
--
--    Store - The model
--    Pair  - To query
--
-- Returns :
--
--    The name of the feature
--
   function Get_Name
            (  Store : not null access Gtk_Fuzzy_Graph_Record;
               Pair  : Parent_Child_Pair
            )  return String;
--
-- Get_N_Columns -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_N_Columns
               (  Store : not null access Gtk_Fuzzy_Graph_Record
               )  return GInt;
--
-- Get_Path -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Get_Path
               (  Store : not null access Gtk_Fuzzy_Graph_Record;
                  Iter  : Gtk_Tree_Iter
               )  return Gtk_Tree_Path;
--
-- Get_Type -- The GTK type of the graph store
--
-- Returns :
--
--    The type of
--
   function Get_Type return Gtk_Type;
--
-- Get_Value -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      procedure Get_Value
                (  Store  : not null access Gtk_Fuzzy_Graph_Record;
                   Iter   : Gtk_Tree_Iter;
                   Column : Gint;
                   Value  : out GValue
                );
--
-- Gtk_New -- Object creation
--
--    Store - The result
--    Graph - A handle to
--
   procedure Gtk_New
             (  Store : out Gtk_Fuzzy_Graph;
                Graph : Node_Handle
             );
--
-- Has_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Has_Child
               (  Store : not null access Gtk_Fuzzy_Graph_Record;
                  Iter  : Gtk_Tree_Iter
               )  return Boolean;
--
-- Initialize -- Construction
--
--    Store - A pointer to
--    Graph - A handle to
--
   procedure Initialize
             (  Store : not null access Gtk_Fuzzy_Graph_Record'Class;
                Graph : Node_Handle
             );
--
-- Next -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      procedure Next
                (  Store : not null access Gtk_Fuzzy_Graph_Record;
                   Iter  : in out Gtk_Tree_Iter
                );
--
-- Nth_Child -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Nth_Child
               (  Store  : not null access Gtk_Fuzzy_Graph_Record;
                  Parent : Gtk_Tree_Iter;
                  N      : GInt
               )  return Gtk_Tree_Iter;
--
-- N_Children -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function N_Children
               (  Store  : not null access Gtk_Fuzzy_Graph_Record;
                  Iter   : Gtk_Tree_Iter := Null_Iter
               )  return GInt;
--
-- Parent -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      function Parent
               (  Store : not null access Gtk_Fuzzy_Graph_Record;
                  Child : Gtk_Tree_Iter
               )  return Gtk_Tree_Iter;
--
-- Previous -- Overrides Gtk.Tree_Model.Abstract_Store...
--
   overriding
      procedure Previous
                (  Store : not null access Gtk_Fuzzy_Graph_Record;
                   Iter  : in out Gtk_Tree_Iter
                );
--
-- Set_Graph -- Change the training set shown
--
--    Store - A pointer to
--    Graph - A handle to a node
--
-- The widget will show that graph rooted in Graph. It can be  any  node
-- of the the graph.
--
   procedure Set_Graph
             (  Store : not null access Gtk_Fuzzy_Graph_Record;
                Graph : Node_Handle
             );
--
-- Sorted_Graph_Store -- Sorted graph models (instantiation)
--
   package Sorted_Graph_Store is
      new Gtk.Tree_Model.Generic_Sort
          (  Gtk_Fuzzy_Graph_Record,
             Gtk_Fuzzy_Graph
          );
   use Sorted_Graph_Store;

   type Gtk_Fuzzy_Graph_Store_Record is
      new Gtk_Tree_Model_Sort_Record with
   record
      Order : Gtk_Column_Order;
   end record;
--
-- Compare -- Overrides Gtk.Tree_Model.Generic_Sort...
--
   overriding
      function Compare
               (  Store  : not null access Gtk_Fuzzy_Graph_Store_Record;
                  Left   : Gtk_Tree_Iter;
                  Right  : Gtk_Tree_Iter
               )  return Row_Order;

   pragma Inline (Get_Children);
   pragma Inline (Get_Classification);
   pragma Inline (Get_Name);
   pragma Inline (Get_Type);

end Gtk.Tree_Model.Fuzzy_Graph_Store;
