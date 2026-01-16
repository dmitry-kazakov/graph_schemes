--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Feature_Pane                           Winter, 2008       --
--  Interface                                                         --
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

with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Gtk.Fuzzy_Feature;          use Gtk.Fuzzy_Feature;
with Gtk.Fuzzy_Feature.Factory;  use Gtk.Fuzzy_Feature.Factory;
with Gtk.GEntry;                 use Gtk.GEntry;

with Fuzzy.Feature.Handle.Container;

private package Gtk.Fuzzy_Catalogue.Feature_Pane is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Feature_Pane);
   use Fuzzy.Feature.Handle.Container;

   package Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "AddFeature",
             Icon       => New_Feature_Icon,
             Tip        => "Create a new feature",
             Relief     => Relief_None
          );
   use Add_Buttons;

   package Copy_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "CopyFeature",
             Icon       => Feature_Copy_Icon,
             Tip        => "Create a copy of the feature",
             Relief     => Relief_None
          );
   use Copy_Buttons;

   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DeleteFeature",
             Icon       => Stock_Delete,
             Tip        => "Remove selected features or folders",
             Relief     => Relief_None
          );
   use Delete_Buttons;

   package Move_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveFeature",
             Icon       => Rename_Icon,
             Tip        => "Move selected features or folders",
             Relief     => Relief_None
          );
   use Move_Buttons;

   package Rename_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "RenameFeature",
             Icon       => Rename_Item_Icon,
             Tip        => "Rename selected feature or folder",
             Relief     => Relief_None
          );
   use Rename_Buttons;

   package View_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "PropertiesFeature",
             Icon       => Feature_View_Icon,
             Tip        => "Show feature properties",
             Relief     => Relief_None
          );
   use View_Buttons;
--
-- Feature_Create_Box_Record -- Box of feature creation
--
   type Feature_Create_Box_Record is new Gtk_Item_Box_Record with record
      Folder_Name : Folder_Selection;
      Name_Edit   : Gtk_Entry;
      Name_Hint   : Gtk_Box;
      Factory     : Gtk_Fuzzy_Feature_Factory;
   end record;
   type Feature_Create_Box is
      access all Feature_Create_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item      - The result
--  [ Store     - A handle to
--    Feature ] - The template
--    Browser   - The parent widget
--
   procedure Gtk_New
             (  Item    : out Feature_Create_Box;
                Store   : Storage_Handle;
                Feature : Feature_Handle;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
   procedure Gtk_New
             (  Item    : out Feature_Create_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Feature_Create_Box_Record);
--
-- Initialize -- To be called by derived types
--
--    Item    - To construct
--    Store   - A handle to
--    Feature - The template
--
   procedure Initialize
             (  Item    : not null access
                          Feature_Create_Box_Record'Class;
                Store   : Storage_Handle;
                Feature : Feature_Handle
             );
--
-- Name_Changed -- Name changed event
--
   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Feature_Create_Box
             );

   package Feature_Create_Boxes is
      new GLib.Object.Weak_References (Feature_Create_Box_Record);
   use Feature_Create_Boxes;
--
-- Feature_View_Box_Record -- Box of feature viewing
--
   type Feature_View_Box_Record is new Gtk_Item_Box_Record with record
      Factory : Gtk_Fuzzy_Feature_Factory;
   end record;
   type Feature_View_Box is access all Feature_View_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Feature - A handle to
--    Title   - Of the item
--    Browser - The parent widget
--
   procedure Gtk_New
             (   Item    : out Feature_View_Box;
                 Feature : Feature_Handle;
                 Title   : UTF8_String;
                 Browser : not null access
                           Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item    - To construct
--    Feature - A handle to
--    Title   - Of the item
--
   procedure Initialize
             (  Item    : not null access Feature_View_Box_Record'Class;
                Feature : Feature_Handle;
                Title   : UTF8_String
             );
--
-- Set -- Change the feature indicated
--
--    Item    - The box
--    Feature - A handle to
--
   procedure Set
             (  Item    : not null access Feature_View_Box_Record;
                Feature : Feature_Handle
             );

   package Feature_View_Boxes is
      new GLib.Object.Weak_References (Feature_View_Box_Record);
   use Feature_View_Boxes;

   type Feature_Panel_Record is new Panel_Record with record
      Add_Button    : Add_Buttons.Gtk_Style_Button;
      Copy_Button   : Copy_Buttons.Gtk_Style_Button;
      Delete_Button : Delete_Buttons.Gtk_Style_Button;
      Move_Button   : Move_Buttons.Gtk_Style_Button;
      Rename_Button : Rename_Buttons.Gtk_Style_Button;
      View_Button   : View_Buttons.Gtk_Style_Button;
      Create        : Feature_Create_Boxes.Weak_Reference;
      View          : Feature_View_Boxes.Weak_Reference;
   end record;
   type Feature_Panel is access all Feature_Panel_Record'Class;
--
-- Add -- Clicked callback
--
   procedure Add
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Build_Item_Menu -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Build_Item_Menu
             (  View  : not null access Feature_Panel_Record;
                Index : Positive
             );
--
-- Check -- Overrides Gtk.Fuzzy_Object...
--
   overriding
   procedure Check
             (  View       : not null access Feature_Panel_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
--
-- Copy -- Clicked callback
--
   procedure Copy
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Delete -- Clicked callback
--
   procedure Delete
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Get -- Overrides Gtk.Fuzzy_Object...
--
   overriding
   procedure Get
             (  View       : not null access Feature_Panel_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
   function Get
            (  View       : not null access Feature_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path;
--
-- Get_Current -- The currently selected feature
--
   procedure Get_Current
             (  View     : not null access Feature_Panel_Record;
                Selected : Selection;
                Store    : out Storage_Handle;
                Feature  : out Feature_Handle
             );
--
-- Get_Selected -- The currently selected features set
--
--    View     - The feature pane
--    Store    - The storage of the features
--    Features - Selected features
--
   procedure Get_Selected
             (  View     : not null access Feature_Panel_Record;
                Store    : out Storage_Handle;
                Features : out Fuzzy.Feature.Handle.Container.Set
             );
--
-- Gtk_New -- Factory
--
   procedure Gtk_New
             (  View      : out Feature_Panel;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             );
--
-- Initialize -- To be called by children
--
   procedure Initialize
             (  View      : not null access Feature_Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             );
--
-- Is_Feature -- Check if a feature was selected
--
--    View     - The panel
--    Selected - A selection in
--
-- Returns :
--
--    True if a feature is selected (exactly one)
--
   function Is_Feature
            (  View     : not null access Feature_Panel_Record;
               Selected : Selection
            )  return Boolean;
--
-- Move -- Clicked callback
--
   procedure Move
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Rename -- Clicked callback
--
   procedure Rename
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Root_Changed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Root_Changed (View : not null access Feature_Panel_Record);
--
-- Selection_Changed -- overrides Gtk.Fuzzy_Catalogue...
--
   procedure Selection_Changed
             (  View : not null access Feature_Panel_Record
             );
--
-- Show -- Clicked callback
--
   procedure Show
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Style_Updated -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Style_Updated
             (  View : not null access Feature_Panel_Record
             );
--
-- Tree_Selection_Changed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Tree_Selection_Changed
             (  View : not null access Feature_Panel_Record
             );

   package Create_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Feature_Create_Box
           );

end Gtk.Fuzzy_Catalogue.Feature_Pane;
