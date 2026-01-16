--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.                             Luebeck            --
--        Graph_Training_Progress                  Spring, 2006       --
--  Interface                                                         --
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

with Fuzzy.Feature;            use Fuzzy.Feature;
with Fuzzy.Graph.Learning;     use Fuzzy.Graph.Learning;
with GLib;                     use GLib;
with Gtk.Indicators.Progress;  use Gtk.Indicators.Progress;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Main.Router;          use Gtk.Main.Router;
with Gtk.Progress_Bar;         use Gtk.Progress_Bar;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;

with Fuzzy.Feature.ID_Map;
with Indicator.Gtk_IO;

package Gtk.Indicators.Graph_Training_Progress is
   pragma Elaborate_Body (Gtk.Indicators.Graph_Training_Progress);
--
-- Graph_Progress_Class_Name
--
   Graph_Progress_Class_Name : constant String := "GtkGraphProgress";
--
-- Gtk_Training_State_Record -- The  widget   represenying   status   of
--                              training.
--
   type Gtk_Training_State_Record is new Gtk_Table_Record with private;
   type Gtk_Training_State is
      access all Gtk_Training_State_Record'Class;
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
   procedure Gtk_New (Widget : out Gtk_Training_State);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - To initialize
--
   procedure Initialize
             (  Widget : not null access Gtk_Training_State_Record'Class
             );
--
-- Gtk_Graph_Progress_Record -- The widget type
--
-- Style properties :
--
--    depth-column-title         - The  title  of  the feature rotations
--                                 depth column. Default is "Depth";
--    equivalence-label          - The   label   of   the    equivalence
--                                 parameter. Default is "Equivalence";
--    example-label              - The label  of  the  current  training
--                                 example  number.   The   Default   is
--                                 "Example";
--    example-cut-label          - The label of the  current  estimation
--                                 of  N(r|r). The Default is "estimated
--                                 N(r|r)";
--    feature-column-title       - The title of the feature name column.
--                                 Default is "Feature";
--    image-column-title         - The title  of the image  name column.
--                                 Default is "Image";
--    image-mean-time-title      - The title  of  the  image  mean  time
--                                 column. Default is "Mean time, ms";
--    image-time-title           - The title  of the image  time column.
--                                 Default is "Time, ms";
--    inconsistent-column-title  - The  title  of  the column containing
--                                 the  list of inconsistent examples in
--                                 the     given     feature.    Default
--                                 "Erroneous";
--    rotations-column-title     - The  title  of  the feature rotations
--                                 column. Default is "Rotations";
--    rotations-label            - The  label  of  the  node   rotations
--                                 number. Default is "Rotations";
--    rotations-per-second-label - The  label  of  the  node   rotations
--                                 number  per  second.  Default is "Per
--                                 second";
--    threshold-label            - The label of the threshold parameter.
--                                 Default is "Threshold";
--    total-examples-label       - The  label  of  the  total   examples
--                                 number.   The   default   is   "Total
--                                 examples";
--    order-column-title         - The   title   of  the  feature  order
--                                 column. Default is "Order".
--
   type Gtk_Graph_Training_Progress_Record is
      new Gtk_Progress_Record with private;
   type Gtk_Graph_Training_Progress is
      access all Gtk_Graph_Training_Progress_Record'Class;
--
-- Get_Training_State
--
--    Widget - The widget
--
-- Returns :
--
--    The training state widget used
--
   function Get_Training_State
            (  Widget : not null access
                        Gtk_Graph_Training_Progress_Record'Class
            )  return Gtk_Training_State;
--
-- Gtk_New -- Factory
--
--    Widget   - The result
--  [ Button ] - The cancel button
--    Spacing  - The default spacing to set
--    State    - The state widget to use, when null a new widget will be
--               created
--
   procedure Gtk_New
             (  Widget  : out Gtk_Graph_Training_Progress;
                Button  : not null access Gtk_Button_Record'Class;
                Spacing : Gtk_Size := (3, 3);
                State   : Gtk_Training_State := null
             );
   procedure Gtk_New
             (  Widget  : out Gtk_Graph_Training_Progress;
                Spacing : Gtk_Size := (3, 3);
                State   : Gtk_Training_State := null
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - To initialize
--  [ Button ] - The cancel button
--    Spacing  - The default spacing to set
--    State    - The state widget to use, when null a new widget will be
--               created
--
   procedure Initialize
             (  Widget  : not null access
                             Gtk_Graph_Training_Progress_Record'Class;
                Button  : not null access Gtk_Button_Record'Class;
                Spacing : Gtk_Size;
                State   : Gtk_Training_State
             );
   procedure Initialize
             (  Widget  : access
                             Gtk_Graph_Training_Progress_Record'Class;
                Spacing : Gtk_Size;
                State   : Gtk_Training_State
             );
private
   use Indicator.Gtk_IO;
   use Fuzzy.Feature.ID_Map;
   type Image_Iter_Array is array (Image_Type) of Gtk_Tree_Iter;

   type Gtk_Training_State_Record is new Gtk_Table_Record with record
      Features_Store        : Gtk_List_Store;
      Features_View         : Gtk_Tree_View;
      Images_Store          : Gtk_List_Store;
      Images_View           : Gtk_Tree_View;
      Example_Value         : Gtk_Label;
      Example_Label         : Gtk_Label;
      Example_Cut_Value     : Gtk_Label;
      Example_Cut_Label     : Gtk_Label;
      Total_Value           : Gtk_Label;
      Total_Label           : Gtk_Label;
      Rotations_Value       : Gtk_Label;
      Rotations_Label       : Gtk_Label;
      RotationsPerSec_Value : Gtk_Label;
      RotationsPerSec_Label : Gtk_Label;
      Threshold_Value       : Gtk_Label;
      Threshold_Label       : Gtk_Label;
      Equivalence_Value     : Gtk_Label;
      Equivalence_Label     : Gtk_Label;
      Map                   : Fuzzy.Feature.ID_Map.Map;
      Images_Iter           : Image_Iter_Array;
   end record;

   type Gtk_Graph_Training_Progress_Record is
      new Gtk_Progress_Record with
   record
      Content : Gtk_Training_State;
   end record;

   type Training_Context_Ptr is access all Graph_Training_Data'Class;
   type Training_Viewer
        (  Widget : not null access
                    Gtk_Graph_Training_Progress_Record'Class;
           Status : not null access Gtk_Progress_Bar_Record'Class
        )  is new Timed_Bar (Status) with
   record
      Context : Training_Context_Ptr;
   end record;
--
-- Draw -- Overrides Indicator.Gtk_IO
--
   overriding
   procedure Draw (Viewer : in out Training_Viewer);
--
-- Set_Data -- Overrides Indicator...
--
   overriding
   procedure Set_Data
             (  Viewer : in out Training_Viewer;
                Data   : in out Indicator_Data'Class
             );
   procedure Set_Data (Viewer : in out Training_Viewer);
--
-- Set_List -- Servicing features list change
--
   procedure Set_List (Viewer : not null access Training_Viewer'Class);
--
-- Style_Updated -- Event handler
--
   procedure Style_Updated
             (  Widget : access Gtk_Graph_Training_Progress_Record'Class
             );

   package Handlers is
      new Gtk.Handlers.Callback (Gtk_Graph_Training_Progress_Record);

   package Training_Viewer_Callback is
      new Generic_Callback_Request (Training_Viewer'Class);
   use Training_Viewer_Callback;

end Gtk.Indicators.Graph_Training_Progress;
