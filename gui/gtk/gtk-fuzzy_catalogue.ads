--                                                                    --
--  package Gtk.Fuzzy_Catalogue     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2008       --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Fuzzy.Classifier.Handle;    use Fuzzy.Classifier.Handle;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Gtk_Icon_Factory;     use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Lecture.Handle;       use Fuzzy.Lecture.Handle;
with Gdk.Color;                  use Gdk.Color;
with Gdk.Event;                  use Gdk.Event;
with GLib.Properties.Icon_Size;  use GLib.Properties.Icon_Size;
with GLib.Values;                use GLib.Values;
with Gtk.Abstract_Browser;       use Gtk.Abstract_Browser;
with Gtk.Alignment;              use Gtk.Alignment;
with Gtk.Fuzzy_Object;           use Gtk.Fuzzy_Object;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Container;              use Gtk.Container;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Directory_Browser;      use Gtk.Directory_Browser;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Indicators;             use Gtk.Indicators;
with Gtk.Notebook;               use Gtk.Notebook;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Image_Menu_Item;        use Gtk.Image_Menu_Item;
with Gtk.Paned;                  use Gtk.Paned;
with Gtk.Progress_Bar;           use Gtk.Progress_Bar;
with Gtk.Recent_Manager_Alt;     use Gtk.Recent_Manager_Alt;
with Gtk.Separator;              use Gtk.Separator;
with Gtk.Stock;                  use Gtk.Stock;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Persistent;                 use Persistent;
with Persistent.Handle;          use Persistent.Handle;

with Gtk.Persistent_Storage_Browser;
use  Gtk.Persistent_Storage_Browser;

with Ada.Unchecked_Deallocation;
with Generic_Set;
with Generic_Stack;
with Generic_Unbounded_Array;
with Generic_Unbounded_Ptr_Array;
with GLib.Object.Strong_References;
with GLib.Object.Weak_References;
with Gtk.Generic_Style_Button;
with Gtk.Handlers;
with Gtk.Image;
with Gtk.Main.Router;
with Object.Handle;
with Tables;

package Gtk.Fuzzy_Catalogue is

   Class_Name : constant String := Prefix & "Catalogue";

   type Filter_Mode is mod 2**4;
   Full_List        : constant Filter_Mode := 2#0001#;
   Features_List    : constant Filter_Mode := 2#0010#;
   Sets_List        : constant Filter_Mode := 2#0100#;
   Classifiers_List : constant Filter_Mode := 2#1000#;
--
-- Gtk_Fuzzy_Catalogue_Record -- A persistent  storage  browser  widget.
--                               The widget is a  descendant  of  paned.
-- One  of  its  children  is  a  tree  view  of  the persistent storage
-- directory browsed another is a columned list of persistent objects in
-- the currently viewed directory.
--
   type Gtk_Fuzzy_Catalogue_Record (Mode : Filter_Mode) is
      new Gtk_Paned_Record with private;
   type Gtk_Fuzzy_Catalogue is
      access all Gtk_Fuzzy_Catalogue_Record'Class;
--
-- Gtk_Item_Box_Record -- Tab item content base type
--
   type Gtk_Item_Box_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_VBox_Record with private;
   type Gtk_Item_Box is access all Gtk_Item_Box_Record'Class;
--
-- Add_Buttons -- Add buttons box
--
--    Item - The item
--
   procedure Add_Buttons (Item : not null access Gtk_Item_Box_Record);
--
-- Add_Item -- Add new page to the tab of items
--
--    Widget - The catalogue widget
--    Title  - The page title
--    Icon   - The icon to use
--    Page   - The page content
--
   procedure Add_Item
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Title  : UTF8_String;
                Icon   : UTF8_String;
                Page   : not null access Gtk_Item_Box_Record'Class
             );
--
-- Cancel -- Cancel confiramation
--
--    Widget - The widget
--
-- Returns :
--
--    True if the widget can be destroyed
--
   function Cancel
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Boolean;
--
-- Cancel -- Cancel confiramation
--
--    Item - The item
--
-- The default implementation returns True
--
-- Returns :
--
--    True if the item can be destroyed
--
   function Cancel
            (  Item : not null access Gtk_Item_Box_Record
            )  return Boolean;
--
-- Current_Item -- Get the current page
--
--    Widget - The catalogue widget
--
-- Returns :
--
--    The item or else null
--
   function Current_Item
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Item_Box;
--
-- Fault -- Show error message in the widget
--
--    Widget - The widget
--    Text   - The message to show
--
   procedure Fault
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Text   : UTF8_String
             );
--
-- Finalize -- Called on finalization
--
--    Item - The widget
--
   procedure Finalize (Item : not null access Gtk_Item_Box_Record);
--
-- Get_Cache -- Get directory cache
--
--    Widget - The widget
--
-- Returns :
--
--    The directory cache
--
   function Get_Cache
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Persistent_Directory;
--
-- Get_Constraint -- Get the constraint
--
--    Widget - The widget
--
-- Returns :
--
--    The constraint
--
   function Get_Constraint
            (  Widget : not null access Gtk_Item_Box_Record
            )  return Picker_Constraint;
--
-- Get_Folder -- Folder name
--
--    Widget  - The widget
--    Storage - Persistent storage handle
--    Folder  - A handle to (can be invalid)
--
-- Returns :
--
--    The path to Folder
--
   function Get_Folder
            (  Widget  : not null access Gtk_Fuzzy_Catalogue_Record;
               Storage : Storage_Handle;
               Folder  : Deposit_Handle
            )  return Item_Path;
--
-- Get_Tree_View -- Directory view widget
--
--    Widget - The widget
--
-- Returns :
--
--    The widget responsible for directory tree representation
--
   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Persistent_Storage_Tree_View;
--
-- Get_Type -- Widget type
--
--    Vertical - The vertically vs. horizontally panned widget
--
-- Returns :
--
--    The widget type
--
   function Get_Type (Vertical : Boolean) return GType;
--
-- Gtk_New -- Factory
--
--    Widget    - The result
--    Dock      - The docking container for dialogs
--    Path      - A path in the storage to browse
--    Mode      - Filter mode
--    Columns   - The number of columns to use in the files list
--    Vertical  - The vertically vs. horizontally panned
--    Buttons   - Has buttons
--    Tree_Size - The maximal size of the tree pane upon start
--    List_Size - The maximal size of the list pane upon start
--    Store     - The cache to use
--    Manager   - The recently used resources manager
--    Tracing   - The desired level of action tracing
--
-- When  the  parameter  Store  is  null  a new Gtk_Persistent_Directory
-- object  is  created.  Otherwise,  the  specified  one  is  used.  The
-- parameters Tree_Size and List_Size controls the  size  of  the  panes
-- upon  start. The panes will initially try to show all content without
-- scroll   bars   but  not  larger  than  the  size  specified  by  the
-- corresponding parameter.
--
   procedure Gtk_New
             (  Widget    : out Gtk_Fuzzy_Catalogue;
                Dock      : not null access Gtk_Container_Record'Class;
                Path      : Item_Path   := "";
                Mode      : Filter_Mode := Features_List or
                                           Sets_List or
                                           Classifiers_List;
                Columns   : Positive    := 4;
                Vertical  : Boolean     := False;
                Buttons   : Boolean     := True;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 550);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 550);
                Store     : Gtk_Persistent_Directory := null;
                Manager   : access Gtk_Recent_Manager_Record'Class :=
                               Get_Default;
                Tracing   : Traced_Actions := Trace_Nothing
             );
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Name    - The picker constraint name
--    Browser - The parent
--    Confirm - Has confirmation button
--
   procedure Gtk_New
             (  Item    : out Gtk_Item_Box;
                Name    : String;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Confirm : Boolean
             );
--
-- Initialize -- To be called by any derived type
--
--    Widget    - To initialize
--    Dock      - The docking container for dialogs
--    Path      - A path in the storage to browse
--    Columns   - The number of columns to use in the files list
--    Vertical  - The vertically vs. horizontally panned
--    Buttons   - Has buttons
--    Tree_Size - The maximal size of the tree pane upon start
--    List_Size - The maximal size of the list pane upon start
--    Store     - The cache to use
--    Query     - The credentials dialog to use with
--    Manager   - The recently used resources manager
--    Tracing   - The desired level of action tracing
--
   procedure Initialize
             (  Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Dock      : not null access Gtk_Container_Record'Class;
                Path      : Item_Path;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Persistent_Directory;
                Manager   : not null access
                            Gtk_Recent_Manager_Record'Class;
                Tracing   : Traced_Actions
             );
--
-- Initialize -- To be called by derived types
--
--    Item    - To construct
--    Name    - The picker constraint name
--    Confirm - Has confirmation button
--
   procedure Initialize
             (  Item    : not null access Gtk_Item_Box_Record'Class;
                Name    : String;
                Confirm : Boolean
             );
--
-- Remove_Buttons -- Remove buttons box
--
--    Item - The item
--
   procedure Remove_Buttons
             (  Item : not null access Gtk_Item_Box_Record
             );
--
-- Restore -- A recently used value
--
--    Widget  - The catalogue widget
--    Key     - The key of the value
--    Default - The default value
--
-- Returns :
--
--    The value
--
   function Restore
            (  Widget  : not null access Gtk_Fuzzy_Catalogue_Record;
               Key     : UTF8_String;
               Default : UTF8_String
            )  return UTF8_String;
--
-- Set_Icon -- Change the icon
--
--    Item - The item
--    Icon - The ID of the icon
--
   procedure Set_Icon
             (  Item : not null access Gtk_Item_Box_Record;
                Icon : UTF8_String
             );
--
-- Set_Title -- Change the title
--
--    Item  - The item
--    Title - The ID of the icon
--
   procedure Set_Title
             (  Item  : not null access Gtk_Item_Box_Record;
                Title : UTF8_String
             );
--
-- Store -- A recently used value
--
--    Widget - The catalogue widget
--    Key    - The key of the value
--    Value  - The value to store
--
   procedure Store
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Key    : UTF8_String;
                Value  : UTF8_String
             );
private
   Def_Scanned_Color    : constant Gdk_Color := Parse ("#AAFFC8");
   Def_Error_Color      : constant Gdk_Color := Parse ("#FF0000");
   Def_Code_Error_Color : constant Gdk_Color := Parse ("#FF0000");
   Def_Unscanned_Color  : constant Gdk_Color := Parse ("#FFC8AA");

   package Abort_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Abort",
             Label      => "Abort",
             Icon       => Stock_Cancel,
             Size       => Icon_Size_Menu,
             Tip        => "Abort",
             Relief     => Relief_None
          );
   use Abort_Buttons;

   package Backward_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "BackwardDirectory",
             Icon       => Stock_Go_Back,
             Tip        => "Go to the previous directory",
             Relief     => Relief_None
          );
   use Backward_Buttons;

   package Bookmark_Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "BookmarkAdd",
             Icon       => Bookmark_Add_Icon,
             Tip        => "Bookmark current directory",
             Relief     => Relief_None
          );
   use Bookmark_Add_Buttons;

   package Bookmark_GoTo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "BookmarkGoTo",
             Icon       => Stock_Jump_To,
             Tip        => "Go to the bookmarked directory",
             Relief     => Relief_None
          );
   use Bookmark_GoTo_Buttons;

   package Bookmark_Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "BookmarkDelete",
             Icon       => Bookmark_Delete_Icon,
             Tip        => "Delete the bookmark",
             Relief     => Relief_None
          );
   use Bookmark_Delete_Buttons;

   package Cancel_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Cancel",
             Icon       => Cancel_Icon,
             Size       => Icon_Size_Menu,
             Tip        => "Close the tab",
             Relief     => Relief_None
          );
   use Cancel_Buttons;

   package Close_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Close",
             Icon       => "gtk-close",
             Size       => Icon_Size_Menu,
             Tip        => "Cancel",
             Label      => "Cancel",
             Relief     => Relief_None
          );
   use Close_Buttons;

   package Delete_Folder_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DeleteObject",
             Icon       => Stock_Delete,
             Tip        => "Delete selected project or folder",
             Relief     => Relief_None
          );
   use Delete_Folder_Buttons;

   package Edit_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Edit",
             Icon       => "gtk-edit",
             Size       => Icon_Size_Menu,
             Tip        => "Edit",
             Relief     => Relief_None
          );

   package Forward_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ForwardDirectory",
             Icon       => Stock_Go_Forward,
             Tip        => "Go to the next directory",
             Relief     => Relief_None
          );
   use Forward_Buttons;

   package Home_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Home",
             Icon       => Stock_Home,
             Tip        => "Home directory",
             Relief     => Relief_None
          );
   use Home_Buttons;

   package New_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "New",
             Icon       => Stock_New,
             Tip        => "Create new",
             Relief     => Relief_None
          );
   use New_Buttons;

   package New_Folder_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "NewFolder",
             Icon       => New_Folder_Icon,
             Tip        => "Create a new folder",
             Relief     => Relief_None
          );
   use New_Folder_Buttons;

   package New_From_FCL_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "NewFromFCL",
             Icon       => New_From_FCL_Icon,
             Tip        => "Read a folder from a FCL source",
             Relief     => Relief_None
          );
   use New_From_FCL_Buttons;

   package New_Project_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "NewProject",
             Icon       => Stock_Connect,
             Tip        => "Connect to a data source",
             Relief     => Relief_None
          );
   use New_Project_Buttons;

   package Next_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Next",
             Icon       => Stock_Go_Forward,
             Tip        => "Next",
             Relief     => Relief_None
          );
   use Next_Buttons;

   package OK_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "OK",
             Icon       => Stock_OK,
             Label      => "OK",
             Tip        => "Confirm",
             Relief     => Relief_None
          );
   use OK_Buttons;

   package Parent_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Parent",
             Icon       => Parent_Icon,
             Tip        => "Go to the parent directory",
             Relief     => Relief_None
          );
   use Parent_Buttons;

   package Previous_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Previous",
             Icon       => Stock_Go_Back,
             Tip        => "Back",
             Relief     => Relief_None
          );
   use Previous_Buttons;

   package Redo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Redo",
             Icon       => Stock_Redo,
             Tip        => "Redo",
             Relief     => Relief_None
          );

   package Refresh_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Refresh",
             Icon       => Stock_Refresh,
             Tip        => "Refresh content",
             Relief     => Relief_None
          );
   use Refresh_Buttons;

   package Save_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Save",
             Icon       => Stock_Save,
             Tip        => "Save",
             Relief     => Relief_None
          );

   package Save_As_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "SaveAs",
             Icon       => Stock_Save_As,
             Tip        => "Save as",
             Relief     => Relief_None
          );

   package Show_Location_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ShowLocation",
             Icon       => Stock_Jump_To,
             Tip        => "Show error location",
             Relief     => Relief_None
          );
   use Show_Location_Buttons;

   package Undo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Undo",
             Icon       => Stock_Undo,
             Tip        => "Undo",
             Relief     => Relief_None
          );

   package View_File_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ViewFile",
             Icon       => Preview_Icon,
             Tip        => "View file",
             Relief     => Relief_None
          );
   use View_File_Buttons;

   package Windowize_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Windowize",
             Icon       => Windowize_Icon,
             Size       => Icon_Size_Menu,
             Tip        => "Undock the tab",
             Relief     => Relief_None
          );
   use Windowize_Buttons;

   package Menu_References is
      new GLib.Object.Strong_References (Gtk_Menu_Record);
------------------------------------------------------------------------
-- Gtk_Item_Box_Record -- Tab item content base type
--
   task type Worker_Task
             (  Item : not null access Gtk_Item_Box_Record'Class
             );
   type Worker_Task_Ptr is access Worker_Task;
   type Item_Path_Ptr is access Item_Path;
   type Exception_Occurrence_Ptr is access Exception_Occurrence;
   type Gtk_Item_Box_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_VBox_Record with
   record
      Frame            : Gtk_Frame;
      Tab_Buttons      : Gtk_HBox;
      Text             : Gtk_Label;
      Image            : Gtk.Image.Gtk_Image;
      Image_Box        : Gtk_HBox;
      Error_Label      : Gtk_Text_View;
      Separator        : Gtk_HSeparator;
      Error_Box        : Gtk_HBox;
      Cancel           : Cancel_Buttons.Gtk_Style_Button;
      Undock           : Windowize_Buttons.Gtk_Style_Button;
      Window           : Gtk_Window; -- If undocked
      Icon             : Unbounded_String;
      Constraint       : Picker_Constraint;
         -- Confirmation button
      Button_Box       : Gtk_HBox; -- Of buttons, only if Commit present
      Button_Separator : Gtk_HSeparator;
      Commit           : OK_Buttons.Gtk_Style_Button;
      Close            : Close_Buttons.Gtk_Style_Button;
      Next             : Next_Buttons.Gtk_Style_Button;
      Previous         : Previous_Buttons.Gtk_Style_Button;
         -- Task
      Worker           : Worker_Task_Ptr;
      Indicator        : Gtk_Indicator;
      Error            : Exception_Occurrence_Ptr;
         -- Error location path
      Path             : Item_Path_Ptr;
   end record;
--
-- Add_Close -- Adds the close button
--
--    Item - The page content
--
   procedure Add_Close (Item : not null access Gtk_Item_Box_Record);
--
-- Add_Next -- Adds the next button
--
--    Item - The page content
--
   procedure Add_Next (Item : not null access Gtk_Item_Box_Record);
--
-- Add_OK -- Adds the confirm button
--
--    Item - The page content
--
   procedure Add_OK (Item : not null access Gtk_Item_Box_Record);
--
-- Add_Previous -- Adds the previous button
--
--    Item - The page content
--
   procedure Add_Previous (Item : not null access Gtk_Item_Box_Record);
--
-- Aborted -- Task completion notification
--
--    Item  - The page content
--    Fault - The error occurence
--
-- The default implementation calls to Error.
--
   procedure Aborted
             (  Item  : not null access Gtk_Item_Box_Record;
                Fault : Exception_Occurrence
             );
--
-- Clean -- Remove error messages from the box
--
--    Item - The page content
--
   procedure Clean (Item : not null access Gtk_Item_Box_Record);
--
-- Close -- Called in response to the close button
--
--    Item - The page content
--
-- The default implementation does nothing.
--
   procedure Close (Item : not null access Gtk_Item_Box_Record);
--
-- Commit -- Called to commit changes in the page content
--
--    Item - The page content
--
-- The default implementation does nothing.
--
   procedure Commit (Item : not null access Gtk_Item_Box_Record);
--
-- Completed -- Task completion notification
--
--    Item - The page content
--
-- This procedure is also called on the context of the  main  GTK+  task
-- and   here   the   poage   can  safely  delete  itself.  The  default
-- implementation closes the page.
--
   procedure Completed (Item : not null access Gtk_Item_Box_Record);
--
-- Create_Indicator -- Called to create progress indicator for the task
--
--    Item - The page content
--
-- The default implementation creates a Gtk_Progress indicator.
--
-- Returns :
--
--    The indicator
--
   function Create_Indicator
            (  Item : not null access Gtk_Item_Box_Record
            )  return Gtk_Indicator;
--
-- Delete -- The item and its page
--
--    Item - The page content
--
   procedure Delete (Item : not null access Gtk_Item_Box_Record);
--
-- Check_New_Name -- Of a newly created object
--
--    Item    - The catalogue widget
--    Storage - A handle to
--    Parent  - Of the object
--    Name    - To check
--    Unique  - Check if duplicated
--
-- Returns :
--
--    Checked name or empty string if name is duplicated or illegal
--
-- Effects :
--
--    Error is called
--
   function Check_New_Name
            (  Item    : not null access Gtk_Item_Box_Record;
               Storage : Storage_Handle;
               Parent  : Deposit_Handle;
               Name    : UTF8_String;
               Unique  : Boolean := True
            )  return String;
--
-- Error -- Show error message in the box
--
--    Item - The widget
--    Text - The message to show
--
   procedure Error
             (  Item : not null access Gtk_Item_Box_Record;
                Text : UTF8_String
             );
--
-- Error_Duplicated -- Show duplicated error message in the box
--
--    Item    - The widget
--    Storage - A handle to
--    Parent  - Of the object
--    Name    - Duplicated name
--
   procedure Error_Duplicated
             (  Item    : not null access Gtk_Item_Box_Record;
                Storage : Storage_Handle;
                Parent  : Deposit_Handle;
                Name    : UTF8_String
             );
--
-- Get_Classifier -- From path
-- Get_Feature
-- Get_Lesson
-- Get_Object
--
--    Path - A full path to the training set
--    View - The training set pane
--    Hint - Error hint (optional)
--
-- When Hint is null no indication of errors is made.
--
-- Returns :
--
--    A handle to, invalid on errors
--
   function Get_Object
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Deposit_Handle;
   function Get_Classifier
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Classifier_Handle;
   function Get_Feature
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Feature_Handle;
   function Get_Lecture
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Lecture_Handle;
--
-- Get_Title -- Of an item
--
--    Item - The item
--
-- Returns :
--
--    The title of
--
   function Get_Title
            (  Item : not null access Gtk_Item_Box_Record
            )  return UTF8_String;
--
-- Next -- Called in response to the next button
--
--    Item - The page content
--
-- The default implementation does nothing.
--
   procedure Next (Item : not null access Gtk_Item_Box_Record);
--
-- Previous -- Called in response to the previous button
--
--    Item - The page content
--
-- The default implementation does nothing.
--
   procedure Previous (Item : not null access Gtk_Item_Box_Record);
--
-- Remove_Close -- Removes the close button
--
--    Item - The page content
--
   procedure Remove_Close (Item : not null access Gtk_Item_Box_Record);
--
-- Remove_Next -- Removes the next button
--
--    Item - The page content
--
   procedure Remove_Next (Item : not null access Gtk_Item_Box_Record);
--
-- Remove_OK -- Removes the confirm button
--
--    Item - The page content
--
   procedure Remove_OK (Item : not null access Gtk_Item_Box_Record);
--
-- Remove_Previous -- Removes the previous button
--
--    Item - The page content
--
   procedure Remove_Previous
             (  Item : not null access Gtk_Item_Box_Record
             );
--
-- Service -- The asynchronous processing
--
--    Item - The page content
--
-- This  procedure  is  called  by  a  task  started  by   a   call   to
-- Start_Servicing. Note that this procedure is  executed  outside  GTK+
-- context and may call no GTK+ routines. The default impementation does
-- nothing. Upon successful completion  Completed  is  called.  Upon  an
-- exception propagation Aborted is called. This procedure is overridden
-- in order to perform some actions asynchronously.
--
-- Exceptions :
--
--    End_Error  - Canceled by user
--    Data_Error - I/O error
--    other      - Other errors
--
   procedure Service (Item : not null access Gtk_Item_Box_Record);
--
-- Start_Servicing -- The asynchronous processing
--
--    Item - The page content
--
-- Exceptions :
--
--    Use_Error - Already serviced
--
   procedure Start_Servicing
             (  Item : not null access Gtk_Item_Box_Record'Class
             );
--
-- Service_Update -- Task notification
--
--    Item - The page content
--
-- This procedure is called from Service in order to call Update on  the
-- context  of  GTK+.  Service  is  not alloed to do any GTK+ I/O. If it
-- needs  to  do  this  it  shall call to Service_Update and perform the
-- necessary actions from Update.
--
   procedure Service_Update
             (  Item : not null access Gtk_Item_Box_Record'Class
             );
--
-- Updated -- Task notification
--
--    Item - The page content
--
-- The default implementation does nothing.
--
   procedure Updated (Item : not null access Gtk_Item_Box_Record);
------------------------------------------------------------------------
--
-- Cancel -- Button clicked event
--
   procedure Cancel
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Commit -- Button clicked event
--
   procedure Commit
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Close -- Button clicked event
--
   procedure Close
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Delete_Event -- Delete request event
--
   function Destroy_Event
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Item   : Gtk_Item_Box
            )  return Boolean;
--
-- Destroy -- Destroyed event
--
   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Destroy -- Destroyed event
--
   procedure Destroy
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Folder_Changed -- Directory changing event
--
   procedure Folder_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Browser   : Gtk_Fuzzy_Catalogue
             );
--
-- Next -- Button clicked event
--
   procedure Next
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Previous -- Button clicked event
--
   procedure Previous
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Style_Updated -- The style-updated event's callback
--
   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Switch_{Items|List}_Page -- Event handler
--
   procedure Switch_Items_Page
             (  Widget  : access Gtk_Widget_Record'Class;
                Params  : GValues;
                Browser : Gtk_Fuzzy_Catalogue
             );
   procedure Switch_List_Page
             (  Widget  : access Gtk_Widget_Record'Class;
                Params  : GValues;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Show_Object -- Button clicked event
--
   procedure Show_Object
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
--
-- Undock -- Button clicked event
--
   procedure Undock
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             );
------------------------------------------------------------------------
-- Panel_Record -- Right panel, contains objects to deal with
--
   type Gtk_Fuzzy_Objects_View_Record;
   type Gtk_Fuzzy_Objects_View is
      access all Gtk_Fuzzy_Objects_View_Record'Class;
   type Panel_Record is abstract
      new Gtk_Object_Picker_Record with
   record
      Menu          : Menu_References.Strong_Reference;
      List          : Gtk_Fuzzy_Objects_View;
      Box           : Gtk_Box;
      Buttons       : Gtk_Box;
      Tab_Label     : Gtk_Label;
      Creation_Time : Gtk_Label;
      Progress      : Gtk_Progress_Bar;
      Progress_Box  : Gtk_Alignment;
   end record;
   type Panel is access all Panel_Record'Class;
--
-- Build_Item_Menu -- Fills the item menu
--
--    View  - The panel
--    Index - Of the item
--
   procedure Build_Item_Menu
             (  View  : not null access Panel_Record;
                Index : Positive
             )  is abstract;
--
-- Directory_Changed -- Directory changing event
--
   procedure Directory_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             );
--
-- Get_Current -- Get current panel
--
--    Widget - The catalogue
--
-- Returns :
--
--    The curent panel
--
   function Get_Current
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Panel;
--
-- Get_Parent -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      function Get_Parent
               (  View : not null access Panel_Record
               )  return Gtk_Widget;
--
-- Get_Path -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      function Get_Path
               (  Picker : not null access Panel_Record;
                  Object : Deposit_Handle
               )  return Item_Path;
--
-- Initialize -- The panel
--
--    View      - The panel
--    Widget    - The widget
--    Columns   - Of the object's list
--    Vertical  - Layout
--    Buttons   - Has buttons
--    Mode      - Filtering mode
--    List_Size - The object's list size
--
   procedure Initialize
             (  View      : not null access Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                Mode      : Filter_Mode;
                List_Size : Gtk_Requisition
             );
--
-- Is_Current -- Check a panel
--
--    Panel - The panel
--
-- Returns :
--
--    True if the panel exists and current
--
   function Is_Current (Panel : access Panel_Record) return Boolean;
--
-- Refresh_Clicked -- Event handler
--
   procedure Refresh_Clicked
             (  Object : access GObject_Record'Class;
                View   : Panel
             );
--
-- Root_Changed -- Of the list of objects
--
--    View - The panel
--
   procedure Root_Changed
             (  View : not null access Panel_Record
             )  is abstract;
--
-- Selection_Changed -- Of the list of objects
--
--    View - The panel
--
-- The default implementation does nothing
--
   procedure Selection_Changed (View : not null access Panel_Record);
--
-- Selection_Changed -- Event handler
--
   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             );
--
-- Set -- Overrides Gtk.Fuzzy_Object
--
--    View   - The panel
--    Store  - Storage (a handle to)
--    Object - The object to select (a handle to)
--
   overriding
      procedure Set
                (  View   : not null access Panel_Record;
                   Store  : Storage_Handle;
                   Object : Deposit_Handle
                );
   overriding
      procedure Set
                (  View : not null access Panel_Record;
                   Path : Item_Path
                );
--
-- Set_Progress -- Delete/show progress indicator
--
--    View  - The panel
--    State - True to show, False to remove
--
   procedure Set_Progress
             (  View  : not null access Panel_Record;
                State : Boolean
             );
--
-- Style_Updated -- Called when style is updated
--
--    View - The panel
--
   procedure Style_Updated
             (  View : not null access Panel_Record
             )  is abstract;
--
-- Switch_To -- Switch to the page
--
--    View   - The panel
--    Widget - The widget
--
   procedure Switch_To
             (  View   : not null access Panel_Record;
                Widget : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Switch_To -- Switch to the page
--
--    Widget - The widget
--    Mode   - The mode
--
-- Switches  to  the  specified  mode if present. When several modes are
-- specified, an arbitrary one is tried.
--
   procedure Switch_To
             (  Widget : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class;
                Mode   : Filter_Mode
             );
--
-- Tree_Selection_Changed -- Of the tree of folders
--
--    View - The panel
--
-- The default implementation does nothing
--
   procedure Tree_Selection_Changed
             (  View : not null access Panel_Record
             );
--
-- Tree_Selection_Changed -- Event handler
--
   procedure Tree_Selection_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             );
------------------------------------------------------------------------
   type Gtk_Fuzzy_Objects_View_Record is
      new Gtk_Persistent_Storage_Items_View_Record with
   record
      Mode    : Filter_Mode;
      Parent  : Panel;
      Browser : Gtk_Fuzzy_Catalogue;
   end record;
--
-- Filter -- Overrides Gtk.Abstract_Browser...
--
   overriding
      function Filter
               (  Widget    : not null access
                              Gtk_Fuzzy_Objects_View_Record;
                  Directory : Boolean;
                  Name      : Item_Name;
                  Kind      : Item_Type
               )  return Boolean;
--
-- Get_Icon -- Overrides Gtk.Persistent_Storage_Browser...
--
   overriding
      function Get_Icon
               (  Widget       : not null access
                                 Gtk_Fuzzy_Objects_View_Record;
                  Name         : Item_Name;
                  Kind         : Item_Type;
                  Directory    : Boolean;
                  Has_Children : Boolean
               )  return Icon_Data;
--
-- Input_Event -- Overrides Gtk.Abstract_Browser...
--
   overriding
      function Input_Event
               (  Widget : not null access
                           Gtk_Fuzzy_Objects_View_Record;
                  Index  : Positive;
                  Event  : Gdk_Event
               )  return Boolean;
------------------------------------------------------------------------
-- Folder_Picker_Record -- Picker of folders
--
   type Folder_Picker_Record is
      new Gtk_Object_Picker_Record with
   record
      Browser : Gtk_Fuzzy_Catalogue;
   end record;
   type Folder_Picker is access all Folder_Picker_Record'Class;
--
-- Check -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      procedure Check
                (  View       : not null access Folder_Picker_Record;
                   Path       : Item_Path;
                   Constraint : Picker_Constraint;
                   Store      : out Storage_Handle;
                  Object     : out Deposit_Handle
                );
--
-- Get -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      procedure Get
                (  View       : not null access Folder_Picker_Record;
                   Constraint : Picker_Constraint;
                   Store      : out Storage_Handle;
                   Object     : out Deposit_Handle
             );
   overriding
      function Get
               (  View       : not null access Folder_Picker_Record;
                  Constraint : Picker_Constraint
               )  return Item_Path;
--
-- Get_Parent -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      function Get_Parent
               (  View : not null access Folder_Picker_Record
               )  return Gtk_Widget;
--
-- Get_Path -- Overrides Gtk.Fuzzy_Object...
--
   overriding
      function Get_Path
               (  Picker : not null access Folder_Picker_Record;
                  Object : Deposit_Handle
               )  return Item_Path;
--
-- Set -- Overrides Gtk.Fuzzy_Object
--
--    View   - The panel
--    Store  - Storage (a handle to)
--    Object - The object to select (a handle to)
--
   overriding
      procedure Set
                (  View   : not null access Folder_Picker_Record;
                   Store  : Storage_Handle;
                   Object : Deposit_Handle
                );
   overriding
      procedure Set
                (  View : not null access Folder_Picker_Record;
                   Path : Item_Path
                );
------------------------------------------------------------------------
-- Folder_Selection_Record -- Folder selection widget
--
   type Folder_Selection_Record is new Gtk_Picker_Box_Record with record
      Item : Gtk_Item_Box;
   end record;
   type Folder_Selection is access all Folder_Selection_Record'Class;
--
-- Gtk_New -- Widget creation
--
--    Widget - The result
--    Name   - The widget's constraint name (can be any)
--    Hint   - The hint box
--    Item   - The parent widget
--
   procedure Gtk_New
             (  Widget : out Folder_Selection;
                Name   : String;
                Hint   : out Gtk_Box;
                Item   : not null access Gtk_Item_Box_Record'Class
             );
--
-- Initialize -- To be called by a derived type
--
--    Widget - The widget to initialize
--    Name   - The widget's constraint name (can be any)
--    Hint   - The hint box
--    Item   - The parent widget
--
   procedure Initialize
             (  Widget : not null access Folder_Selection_Record'Class;
                Name   : String;
                Hint   : out Gtk_Box;
                Item   : not null access Gtk_Item_Box_Record'Class
             );
--
-- Get_Folder -- Get the folder picked
--
--    Widget - The picker
--    Store  - The persistent storage of the folder
--    Folder - The folder
--
-- The procedure browses the  folder  specified.  Upon  error  Store  is
-- invalid and the corresponding message is shown and the picker hint is
-- set.
--
   procedure Get_Folder
             (  Widget : not null access Folder_Selection_Record;
                Store  : out Storage_Handle;
                Folder : out Deposit_Handle
             );

   package Gtk_Item_Boxes is
      new GLib.Object.Weak_References (Gtk_Item_Box_Record);
   use Gtk_Item_Boxes;

   function "<" (Left, Right : Gtk_Widget) return Boolean;
   package Gtk_Window_Sets is new Generic_Set (Gtk_Widget, null);
   use Gtk_Window_Sets;

   type Item_Path_Ptr_Array is
      array (Positive range <>) of Item_Path_Ptr;
   package Item_Path_Arrays is
      new Generic_Unbounded_Ptr_Array
          (  Index_Type            => Positive,
             Object_Type           => Item_Path,
             Object_Ptr_Type       => Item_Path_Ptr,
             Object_Ptr_Array_Type => Item_Path_Ptr_Array
          );
   use Item_Path_Arrays;

   package Item_Tables is new Tables (Gtk_Item_Box);
   use Item_Tables;
--
-- Catalogue_Directory_Record -- Cache of the file system
--
   type Catalogue_Directory_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_Directory_Record with null record;
   type Catalogue_Directory is
      access all Catalogue_Directory_Record'Class;
   overriding
      procedure Progress
                (  Store : not null access Catalogue_Directory_Record;
                   Path  : Item_Path;
                   State : GDouble
                );

   type Files_Browsing is record
      Store    : Catalogue_Directory;
      Stack    : Unbounded_Ptr_Array;
      Size     : Natural  := 0;
      Current  : Positive := 1;
      Open     : Item_Tables.Table;
   end record;
   type Files_Browsing_Ptr is access Files_Browsing;

   function At_Bottom (Browsing : Files_Browsing_Ptr) return Boolean;
   function Next (Browsing : Files_Browsing_Ptr) return Item_Path;
   function On_Top (Browsing : Files_Browsing_Ptr) return Boolean;
   function Previous (Browsing : Files_Browsing_Ptr) return Item_Path;
   procedure Push (Browsing : Files_Browsing_Ptr; Path : Item_Path);
------------------------------------------------------------------------
-- Move_Set -- Renaming actions
--
   type Path_Ptr is access Item_Path;
   type Name_Ptr is access Item_Name;
   type Name_List is array (Positive range <>) of Name_Ptr;
   type Move_Set (Size : Positive) is new Object.Entity with record
      Into : Boolean;
      From : Path_Ptr;
      To   : Path_Ptr;
      List : Name_List (1..Size);
   end record;
   type Move_Set_Ptr is access Move_Set'Class;
   procedure Finalize (Set : in out Move_Set);

   procedure Free is
      new Ada.Unchecked_Deallocation (Item_Name, Name_Ptr);

   package Move_Set_Handles is
      new Object.Handle (Move_Set, Move_Set_Ptr);
   use Move_Set_Handles;

   type Move_Set_Array is
      array (Positive range <>) of Move_Set_Handles.Handle;

   package Move_Set_Arrays is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Move_Set_Handles.Handle,
             Object_Array_Type => Move_Set_Array,
             Null_Element      => Move_Set_Handles.Ref (null)
          );
   package Move_Set_Stacks is
      new Generic_Stack
          (  Index_Type   => Positive,
             Object_Type  => Move_Set_Handles.Handle,
             Array_Type   => Move_Set_Arrays.Unbounded_Array,
             Null_Element => Move_Set_Handles.Ref (null),
             Get          => Move_Set_Arrays.Get,
             Put          => Move_Set_Arrays.Put
          );
   type Move_Data is record
      Redo : Move_Set_Stacks.Stack;
      Undo : Move_Set_Stacks.Stack;
   end record;
   type Move_Data_Ptr is access Move_Data;
------------------------------------------------------------------------
-- Gtk_Fuzzy_Catalogue_Record -- Implementation
--
   type Gtk_Fuzzy_Catalogue_Record (Mode : Filter_Mode) is
      new Gtk_Paned_Record with
   record
      Cache         : Gtk_Persistent_Directory;
      Files         : Files_Browsing_Ptr;
      Dock          : Gtk_Container;
      Buttons       : Gtk_Box;
      Tree          : Gtk_Persistent_Storage_Tree_View;
      List_Tabs     : Gtk_Notebook;
      Item_Tabs     : Gtk_Notebook;
      Features      : Panel;
      Lectures      : Panel;
      Classifiers   : Panel;
      Directories   : Folder_Picker;
      New_Folder    : New_Folder_Buttons.Gtk_Style_Button;
      New_From_FCL  : New_From_FCL_Buttons.Gtk_Style_Button;
      Delete_Folder : Delete_Folder_Buttons.Gtk_Style_Button;
      New_Project   : New_Project_Buttons.Gtk_Style_Button;
      Project       : Gtk_Item_Boxes.Weak_Reference;
      Folder        : Gtk_Item_Boxes.Weak_Reference;
      Floating      : Gtk_Window_Sets.Set;
      Renaming      : Move_Data_Ptr;
      Current       : Panel;
      Tracing       : Traced_Actions;
      Menu          : Menu_References.Strong_Reference;
   end record;
--
-- Delete -- Objects deletion
--
--    Button  - The deletion button
--    View    - The panel
--    Browser - The catalogue widget
--
-- The procedure queries for deletion of objects selected  in  View.  No
-- query is made when nothing is selected.
--
   procedure Delete
             (  Button  : not null access Gtk_Widget_Record'Class;
                View    : Panel;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Get_Classifier -- The currently selected
-- Get_Lecture
--
--    Widget - The catalogue widget
--
-- Returns :
--
--    A handle to or else invalid
--
   function Get_Classifier
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Classifier_Handle;
   function Get_Lecture
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Lecture_Handle;
--
-- Get_Item -- Get the page by number
--
--    Widget - The catalogue widget
--    No     - The page number
--
-- Returns :
--
--    The item or else null
--
   function Get_Item
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
               No     : GInt
            )  return Gtk_Item_Box;
--
-- Key_Press -- Event handler
--
   function Key_Press
            (  Widget  : access Gtk_Widget_Record'Class;
               Event   : Gdk_Event;
               Browser : Gtk_Fuzzy_Catalogue
            )  return Boolean;
--
-- Query -- Dialog box
--
--    Widget     - The catalogue widget
--    Title      - Of the dialig
--    Icon_Stock - To show left of the message
--    Icon_Size  - Of the icon
--    Message    - To show
--
-- Returns :
--
--    The button pressed
--
   function Query
            (  Widget     : not null access Gtk_Fuzzy_Catalogue_Record;
               Title      : UTF8_String;
               Icon_Stock : UTF8_String;
               Icon_Size  : Gtk_Icon_Size_Enum;
               Message    : UTF8_String;
               Cancel     : Boolean := True
            )  return Gtk_Response_Type;
--
-- Set_Item -- Set current page
--
--    Widget - The catalogue widget
--    Page   - The page content
--
   procedure Set_Item
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Page   : not null access Gtk_Item_Box_Record'Class
             );
--
-- Gtk_New -- Menu item creation from a button and style name
--
--    Widget  - The menu item
--    Name    - The style name of the item label
--    Buttom  - The style buttom to get the image of
--    Browser - The catalogue widget
--
   procedure Gtk_New
             (  Widget  : out Gtk_Image_Menu_Item;
                Name    : UTF8_String;
                Button  : not null access Gtk_Widget_Record'Class;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Handlers
--
   package Box_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Item_Box
          );
   package Box_Return_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Item_Box
          );
   package Folder_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Folder_Picker
          );
   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Catalogue
          );
   package Panel_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Panel
          );
   package Return_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Widget_Record,
             Boolean,
             Gtk_Fuzzy_Catalogue
          );
   package Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Fuzzy_Catalogue
          );
   package Object_Popup is
      new Popup_User_Data (Gtk_Fuzzy_Objects_View);

   package Directory_Popup is
      new Popup_User_Data (Gtk_Persistent_Storage_Tree_View);

   package Gtk_Item_Box_Messages is
      new Gtk.Main.Router.Generic_Message (Gtk_Item_Box);

   procedure Do_Abort    (Item : in out Gtk_Item_Box);
   procedure Do_Complete (Item : in out Gtk_Item_Box);

   type Update_Request
        (  Item : not null access Gtk_Item_Box_Record'Class
        )  is new Gtk.Main.Router.Request_Data with null record;
   procedure Service (Data : in out Update_Request);

end Gtk.Fuzzy_Catalogue;
