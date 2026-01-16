--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Graph                             Luebeck            --
--  Interface                                      Spring, 2006       --
--                                                                    --
--                                Last revision :  11:45 29 May 2020  --
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
--  This  package  provides  Gtk_Fuzzy_Graph  widget  to represent fuzzy
--  graphs  and  dialog  boxes  to  show  fuzzy  graphs.  See  the  Show
--  procedures.
--
--  The widget is a scrolled window  that  contains  a  tree  view  that
--  represents  the  graph.  The  style properties are bound to the tree
--  view.
--
with Confidence_Factors;         use Confidence_Factors;
with Fuzzy.Classifier;           use Fuzzy.Classifier;
with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Graph.Handle;         use Fuzzy.Graph.Handle;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;       use Fuzzy.Lecture.Handle;
with Gtk.Fuzzy_Object;           use Gtk.Fuzzy_Object;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Indicator;                  use Indicator;
with Indicator.Handle;           use Indicator.Handle;

with Gtk.Handlers;
with Gtk.Tree_Model.Fuzzy_Graph_Store;

package Gtk.Fuzzy_Graph is
--
-- Graph_Class_Name -- The name of the widget class (the tree view of)
--
   Graph_Class_Name : constant String := Prefix & "Graph";
--
-- Gtk_Fuzzy_Graph_Record -- The widget type
--
-- Style properties (of the tree view component):
--
--    graph-column-title   - The title of the first (expandable) column.
--                           The default value is "Graph".
--    graph-column-spacing - The  gap  between images and description in
--                           the first column. The default is 3.
--
-- Additionally it has the style properties of:
--
--    Gtk_Cell_Renderer_Fuzzy_Boolean (see Fuzzy.Logic.Gtk_View)
--
   type Gtk_Fuzzy_Graph_Record is
      new Gtk_Scrolled_Window_Record with private;
   type Gtk_Fuzzy_Graph is access all Gtk_Fuzzy_Graph_Record'Class;
--
-- Classify_As_Selected -- Classification of a training example
--
--    Widget     - The widget
--    Lesson     - The training set (a handle to)
--    Example    - The example to classify
--    Image      - The image to be classified
--    Generalize - The nodes generalization mode
--    Threshold  - The confidence threshold
--
-- This  function uses the selected graph node to classify an example as
-- does Fuzzy.Graph.Handle.Classify. When a branch is selected, the node
-- it leads to is used. The parameters have same meaning as in Classify.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - No selection, wrong handle (Lesson)
--
   function Classify_As_Selected
            (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Image      : Image_Type;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Classification;
--
-- Classify_As_Selected -- Classification of a training example
--
--    Widget  - The widget
--    Context - The data context
--    Image   - The image to be classified
--
-- This  function uses the selected graph node to classify an example as
-- does Fuzzy.Graph.Handle.Classify. When a branch is selected, the node
-- it leads to is used. The parameters have same meaning as in Classify.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - No selection, wrong handle (Lesson)
--
   function Classify_As_Selected
            (  Widget  : not null access Gtk_Fuzzy_Graph_Record;
               Context : not null access
                         Classification_Parameters'Class;
               Image   : Image_Type
            )  return Classification;
--
-- Collapse_All -- Collapse view
--
--    Widget - The widget
--
-- This procedure collapses the view of the graph.
--
   procedure Collapse_All
             (  Widget : not null access Gtk_Fuzzy_Graph_Record
             );
--
-- Expand_Example -- Expand nodes dealing with a training example
--
--    Widget  - The widget
--    Lesson  - A handle to the training set
--    Example - To use
--    Image   - The image to expand
--
-- Nothing  happens when Lesson is an invalid handle or does not contain
-- the example specified.
--
   procedure Expand_Example
             (  Widget  : not null access Gtk_Fuzzy_Graph_Record;
                Lesson  : Lecture_Handle;
                Example : Positive;
                Image   : Image_Type
             );
--
-- Find_Selected -- Classification of a training example
--
--    Widget     - The widget
--    Lesson     - The data context or a teaching set handle
--    Example    - The example to classify
--    Complement - True if the example is negative
--    Image      - The image to browse
--    Viewer     - A progress indication object or a handle to
--
-- These procedures use the selected part  of  a  graph  to  browse  the
-- training  set  lesson.  It  starts  from  the  image  following  ones
-- identified by Example / Complement and searches for the first Example
-- / Complement which matches the selected part of the graph.  When  the
-- end of the training set is reached the procedures continue  from  the
-- set  beginning.  When  the  whole  set  is searched without a result,
-- End_Error is propagated. Example / Complement are not changed when an
-- exception is propagated.
--
-- Exceptions :
--
--    Constraint_Error - No selection, wrong handle (Lesson)
--    End_Error        - Nothing found, no classifier
--
   procedure Find_Selected
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Image      : Image_Type;
                Viewer     : Indicator_Handle
             );
   procedure Find_Selected
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Image      : Image_Type;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Get_Tree_View -- Get the tree view widget of
--
--    Widget - The widget
--
-- Returns :
--
--    The tree view
--
   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Graph_Record
            )  return Gtk_Tree_View;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Graph);
--
-- Hss_Selected -- Subclassifier check
--
--    Widget - The widget
--
-- Returns :
--
--    True if there is a subclassifier selected
--
   function Has_Selected
            (  Widget : not null access Gtk_Fuzzy_Graph_Record
            )  return Boolean;
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--
-- When  a  new  type is derived from the base, this procedure has to be
-- called as a part of widget object initialization.
--
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Graph_Record'Class
             );
--
-- Put -- A graph into the widget
--
--    Widget     - The feature view widget
--    Graph      - A handle to
--    Parameters - Output formatting parameters
--
-- This procedure causes the widget to show the  graph.  When  Graph  is
-- invalid, the widget will be an empty tree.
--
   procedure Put
             (  Widget     : not null access Gtk_Fuzzy_Graph_Record;
                Graph      : Node_Handle;
                Parameters : Output_Parameters'Class := Output_Defaults
             );
--
-- Show -- Graph in a dialog box
--
--    Graph      - A handle to the graph
--    Parameters - Output parameters
--    Title      - Of the dialog
--    Button     - The button name
--    Parent     - The parent of the dialog
--    Flags      - The flags
--
-- This procedure causes a dialog to be shown with the  graph  indicated
-- in. The dialog shown has a  confirmation  button  if  Button  is  not
-- empty. The procedures don't return until the user presses the  button
-- or closes the window. The parameters Title, Parent,  Flags  are  ones
-- described for dialogs in Gtk.Dialog.
--
   procedure Show
             (  Graph    : Node_Handle;
                Title    : UTF8_String      := "";
                Button   : UTF8_String      := "_OK";
                Parent   : Gtk_Window       := null;
                Flags    : Gtk_Dialog_Flags := Modal
             );
private
   use Gtk.Tree_Model.Fuzzy_Graph_Store;

   type Gtk_Fuzzy_Graph_Record is
      new Gtk_Scrolled_Window_Record with
   record
      Model : Gtk_Fuzzy_Graph_Store;
      View  : Gtk_Tree_View;
   end record;

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Graph
          );

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Graph  : Gtk_Fuzzy_Graph
             );

end Gtk.Fuzzy_Graph;
