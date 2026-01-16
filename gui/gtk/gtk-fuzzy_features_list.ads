--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Features_List                    Luebeck            --
--  Interface                                      Autumn, 2008       --
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

with Gtk.Box;                   use Gtk.Box;
with Gtk.Button;                use Gtk.Button;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Fuzzy_Object;          use Gtk.Fuzzy_Object;
with Gtk.Handlers.References;   use Gtk.Handlers.References;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget;                use Gtk.Widget;
with Object.Archived;           use Object.Archived;
with Object.Archived.Features;  use Object.Archived.Features;

with Backward_Link_Handles.Sets;
with Fuzzy.Feature.Handle.Bounded_Arrays;
with Fuzzy.Feature.Handle.Container;
with Gtk.Generic_Style_Button;
with Gtk.Handlers;

package Gtk.Fuzzy_Features_List is
   pragma Elaborate_Body (Gtk.Fuzzy_Features_List);
   use Fuzzy.Feature.Handle.Bounded_Arrays;
   use Fuzzy.Feature.Handle.Container;

   Class_Name : constant String := Prefix & "FeaturesList";
--
-- Gtk_Fuzzy_Features_List -- Editing features list
--
   type Gtk_Fuzzy_Features_List_Record is
      new Gtk_HBox_Record with private;
   type Gtk_Fuzzy_Features_List is
      access all Gtk_Fuzzy_Features_List_Record'Class;
--
-- Check_Features
--
--    Widget - The widget
--    List   - The features to check
--
-- The procedure checks all features from List to  be  dependent  of  or
-- from  the  list  of  features maintained by the widget. If yes the up
-- button is enabled if any. Otherwise it is disabled.
--
   procedure Check_Features
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record;
                List   : Fuzzy.Feature.Handle.Container.Set
             );
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Construction
--
--    Widget - The result
--
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Features_List);
--
-- Get_Button_Box
--
--    Widget - The widget
--
-- Returns :
--
--    The button box
--
   function Get_Button_Box
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_VBox;
--
-- Get_Deselected
--
--    Widget - The widget
--
-- Returns :
--
--    The list of currently deselected features
--
   function Get_Deselected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Bounded_Array;
--
-- Get_Deselected_Tree_View
--
--    Widget - The widget
--
-- Returns :
--
--    The tree view of the deselected features
--
   function Get_Deselected_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_Tree_View;
--
-- Get_External_Buttons
--
--    Widget - The widget
--    Up     - The button to remove a feature
--    Down   - The button to add a feature from the list
--
   procedure Get_External_Buttons
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record;
                Up     : out Gtk_Button;
                Down   : out Gtk_Button
             );
--
-- Get_Selected
--
--    Widget - The widget
--
-- Returns :
--
--    The list or the set of currently selected features
--
   function Get_Selected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Bounded_Array;
   function Get_Selected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Selected_Tree_View
--
--    Widget - The widget
--
-- Returns :
--
--    The tree view of the selected features
--
   function Get_Selected_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_Tree_View;
--
-- Initialize -- To be called by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record
             );
--
-- Put -- Set the lists of selected and deselected features
--
--    Widget       - The widget
--    Selected     - To appear in the list of selected features
--  [ Deselected ] - To appear in the list of deselected features
--
   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Features_List_Record;
                Selected   : Bounded_Array;
                Deselected : Bounded_Array := Empty
             );
   procedure Put
             (  Widget   : not null access
                           Gtk_Fuzzy_Features_List_Record;
                Selected : Fuzzy.Feature.Handle.Container.Set
             );
--
-- Select_Features
--
--    Widget - The widget
--    List   - The features to select
--
-- The procedure moves all features from List to the selected list.
--
   procedure Select_Features
             (  Widget : not null access
                         Gtk_Fuzzy_Features_List_Record;
                List   : Fuzzy.Feature.Handle.Container.Set
             );

   package Down_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DownFeature",
             Icon       => Stock_Go_Down,
             Tip        => "Move the selected features down",
             Relief     => Relief_None
          );
   use Down_Buttons;

   package Left_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ExcludeFeature",
             Icon       => Stock_Go_Back,
             Tip        => "Exclude the selected features",
             Relief     => Relief_None
          );
   use Left_Buttons;

   package Right_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "UseFeature",
             Icon       => Stock_Go_Forward,
             Tip        => "Use the selected features",
             Relief     => Relief_None
          );
   use Right_Buttons;

   package Up_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "UpFeature",
             Icon       => Stock_Go_Up,
             Tip        => "Move the selected features up",
             Relief     => Relief_None
          );
   use Up_Buttons;

private
   use Fuzzy.Feature.Handle;

   package Down_Upper_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "UseExternalFeature",
             Icon       => Stock_Go_Down,
             Relief     => Relief_None,
             Tip => (  "Use the features selected in the features "
                    &  "list pane above"
          )         );
   use Down_Upper_Buttons;

   package Up_Upper_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ExcludeExternalFeature",
             Icon       => Stock_Go_Up,
             Tip        => "Exclude the selected features",
             Relief     => Relief_None
          );
   use Up_Upper_Buttons;

   type Feature_Observer
        (  List : not null access Gtk_Fuzzy_Features_List_Record'Class
        )  is new Feature_Link with null record;
   type Feature_Observer_Ptr is access Feature_Observer'Class;
--
-- Deleted -- Overrides Object.Archived...
--
   procedure Deleted
             (  Link  : in out Feature_Observer;
                Temps : in out Deposit_Container'Class
             );
--
-- Destroyed -- Overrides Object.Archived...
--
   procedure Destroyed (Link : in out Feature_Observer);
--
-- Renamed -- Overrides Object.Archived.Features...
--
   procedure Renamed
             (  Link     : in out Feature_Observer;
                Old_Name : String;
                New_Name : String
             );
--
-- Gtk_Fuzzy_Features_List -- Editing features list
--
   type Gtk_Fuzzy_Features_List_Record is
      new Gtk_HBox_Record with
   record
      Buttons_Box     : Gtk_VBox;
      Up_Button       : Up_Buttons.Gtk_Style_Button;
      Down_Button     : Down_Buttons.Gtk_Style_Button;
      Left_Button     : Left_Buttons.Gtk_Style_Button;
      Right_Button    : Right_Buttons.Gtk_Style_Button;
      Take_Button     : Down_Upper_Buttons.Gtk_Style_Button;
      Release_Button  : Up_Upper_Buttons.Gtk_Style_Button;
      Take_Signal     : Handler_Reference;
      Release_Signal  : Handler_Reference;
      Deselected      : Gtk_Tree_View;
      Selected        : Gtk_Tree_View;
      Selected_List   : Gtk_List_Store;
      Deselected_List : Gtk_List_Store;
      Observers       : Backward_Link_Handles.Sets.Set;
      Features_List   : Fuzzy.Feature.Handle.Container.Set;
   end record;
--
-- Destroy -- Event handler
--
   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             );
--
-- Deselected_Changed -- Selection changed event
--
   procedure Deselected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_List
             );
--
-- Down -- Button click event
--
   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             );
--
-- Left -- Button click event
--
   procedure Left
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             );
--
-- Right -- Button click event
--
   procedure Right
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             );
--
-- Selected_Changed -- Selection changed event
--
   procedure Selected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_List
             );
--
-- Up -- Button click event
--
   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             );

   package Widget_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Gtk_Fuzzy_Features_List
           );

   package Selection_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Tree_Selection_Record,
              Gtk_Fuzzy_Features_List
           );

   type Deleted_Data
        (  Observer : not null access Feature_Observer'Class
        )  is new Request_Data with null record;
   procedure Service (Data : in out Deleted_Data);

   type Renamed_Data
        (  Observer : not null access Feature_Observer'Class;
           Length   : Natural
        )  is new Request_Data with
   record
      Name : String (1..Length);
   end record;
   procedure Service (Data : in out Renamed_Data);

end Gtk.Fuzzy_Features_List;
