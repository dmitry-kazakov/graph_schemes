--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GLib.Values.Fuzzy_Graph_Node                Luebeck            --
--  Interface                                      Spring, 2006       --
--                                                                    --
--                                Last revision :  21:25 10 Nov 2009  --
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
--  This package provides a GTK+ type  GType_Graph_Node,  which  can  be
--  used to place a fuzzy graph node into a GTK+ value.
--
with Fuzzy.Graph;         use Fuzzy.Graph;
with Fuzzy.Graph.Handle;  use Fuzzy.Graph.Handle;

with GLib.Values.Handle;

package GLib.Values.Fuzzy_Graph_Node is
--
-- Get_Graph -- Get root node from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- GType_Graph_Node.  Graphs  are  referenced  using its root nodes. Any
-- node of a graph defines a graph rooted in this node.
--
-- Returns :
--
--    A handle to the root node of the graph
--
-- Exceptions :
--
--    Constraint_Error - The value is not a graph node
--
   function Get_Graph (Value : GValue) return Node_Handle;
--
-- GType_Graph_Node -- The GTK+ type of graph nodes
--
   function GType_Graph_Node return GType;
--
-- Set_Graph -- Set a value
--
--    Value - To set
--    Graph - A handle to the root node of
--
-- This procedure sets a graph into GTK+ value,  previously  initialized
-- using Init with the parameter GType_Graph_Node.
--
-- Exceptions :
--
--    Constraint_Error - Not an object value, invalid handle
--
   procedure Set_Graph
             (  Value : in out GValue;
                Graph : Node_Handle
             );
private
--
-- Gtk_Values -- Interfacing of graph nodes to GTK+ values
--
   package Gtk_Values is
      new GLib.Values.Handle
          (  Type_Name       => "GFuzzyGraphNode",
             Object_Type     => Graph_Node,
             Object_Type_Ptr => Graph_Node_Ptr,
             Handle_Type     => Node_Handle
          );
   function Get_Graph (Value : GValue) return Node_Handle
      renames Gtk_Values.Get_Handle;
   function GType_Graph_Node return GType
      renames Gtk_Values.Get_Type;
   procedure Set_Graph
             (  Value : in out GValue;
                Graph : Node_Handle
             )  renames Gtk_Values.Set_Handle;

end GLib.Values.Fuzzy_Graph_Node;
