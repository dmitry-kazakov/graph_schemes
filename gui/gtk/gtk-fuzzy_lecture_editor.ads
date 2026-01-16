--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Lecture_Editor                    Luebeck            --
--  Interface                                      Autumn, 2009       --
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
--  This  package  provides a widget to represent training sets. It also
--  provides dialog boxes based on the widget. See the Show procedures.
--
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;      use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Gtk_Icon_Factory;         use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Cell_Renderer;              use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Fuzzy;        use Gtk.Cell_Renderer_Fuzzy;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Fuzzy_Object;               use Gtk.Fuzzy_Object;
with Gtk.Image;                      use Gtk.Image;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Table;                      use Gtk.Table;
with Gtk.Toggle_Button;              use Gtk.Toggle_Button;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Selection;             use Gtk.Tree_Selection;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Widget;                     use Gtk.Widget;
with Indicator;                      use Indicator;
with Indicator.Handle;               use Indicator.Handle;

with Generic_Unbounded_Array;
with Generic_Set;
with Gtk.Generic_Style_Button;
with Gtk.Handlers;
with Gtk.Tree_Model.Fuzzy_Data_Store;

package Gtk.Fuzzy_Lecture_Editor is
--
-- Class_Name -- The name of the tree view widget class
--
   Class_Name : constant String := Prefix & "LectureEditor";
--
-- Gtk_Fuzzy_Lecture_Editor_Record -- The widget type
--
-- Style properties of the tree view within the widget:
--
--    example-no-icon           - The  stock  name  of the icon used for
--                                the example number column.
--    example-no-icon-size      - The icon size of the icon used for the
--                                example  number  column. Default value
--                                is Icon_Size_Small_Toolbar.
--    example-type-icon         - The  stock  name  of the icon used for
--                                the example type column.
--    example-type-icon-size    - The icon size of the icon used for the
--                                example  type column. Default value is
--                                Icon_Size_Small_Toolbar.
--    feature-title-box-spacing - The   spacing   used  in  the  feature
--                                columns. The default value is 3.
--
-- Signals :
--
--    modified-changed           - Emitted when  the status of  modified
--                                 flag changes;
--    features-selection-changed - Emitted  when  the toggle  status  of
--                                 fectures  selection changes from none
--                                 to some.
--
   type Gtk_Fuzzy_Lecture_Editor_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Lecture_Editor is
      access all Gtk_Fuzzy_Lecture_Editor_Record'Class;
--
-- Add_Feature -- Add feature to the edited set
--
--    Widget  - The widget
--    Feature - The handle of the feature to add
--
   procedure Add_Feature
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             );
--
-- Delete_Feature -- Delete feature
--
--    Widget  - The widget
--    Feature - The handle of the feature to delete
--
   procedure Delete_Feature
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             );
--
-- Features_Selection_Changed -- Emits features-selection-changed event
--
--    Widget - The widget
--
   procedure Features_Selection_Changed
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             );
--
-- Get_Buttons_Box -- Get buttons box of the widget
--
--    Widget - The widget
--
-- Returns :
--
--    The button box
--
   function Get_Buttons_Box
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Gtk_Box;
--
-- Get_Examples -- Copy examples to a training set
--
--    Widget - The widget
--    Lesson - To add examples to
--    Viewer - To use with
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    End_Error        - Operation was aborted by the user
--
   procedure Get_Examples
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : in out Lecture_Handle;
                Viewer : Indicator_Handle
             );
   procedure Get_Examples
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : in out Lecture_Handle;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Get_Modified -- Modification flag
--
--    Widget - The widget
--
-- Returns :
--
--    True if the undo buffer is not empty
--
   function Get_Modified
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Boolean;
--
-- Get_Selected_Feature -- Get feature of the selected column
--
--    Widget - The widget
--
-- Returns :
--
--    The feature of the selected column or else invalid feature handle
--
   function Get_Selected_Feature
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
            )  return Feature_Handle;
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
            (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
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
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Lecture_Editor);
--
-- Has_Feature -- Check if a feature is in the edited set
--
--    Widget  - The widget
--    Feature - The handle of the feature
--
   function Has_Feature
            (  Widget  : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
               Feature : Feature_Handle
            )  return Boolean;
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record'Class
             );
--
-- Modified_Changed -- Emits modified-changed event
--
--    Widget - The widget
--
   procedure Modified_Changed
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             );
--
-- Put -- A lecture into the widget
--
--    Widget - The widget
--    Lesson - A handle to the training set
--    Input  - Input formatting parameters
--    Output - Output formatting parameters
--    Viewer - To use with
--
-- This  procedure  causes  the  widget  to  show Lesson. When Lesson is
-- invalid the widget will show an empty training set. The parameter
-- Size specifies the cache size in examples per feature.
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults;
                Viewer : Indicator_Handle
             );
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Set_Stored -- Make the undo buffer empty
--
--    Widget - The widget
--
   procedure Set_Stored
             (  Widget : not null access Gtk_Fuzzy_Lecture_Editor_Record
             );
private
   use Gtk.Tree_Model.Fuzzy_Data_Store;

   package Copy_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Copy",
             Icon       => Lecture_Copy_Example_Icon,
             Tip        => "Add copies of the selected examples",
             Relief     => Relief_None
          );
   use Copy_Buttons;

   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Delete",
             Icon       => Lecture_Delete_Example_Icon,
             Tip        => "Delete selected examples",
             Relief     => Relief_None
          );
   use Delete_Buttons;

   package New_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "New",
             Icon       => Lecture_Add_Example_Icon,
             Tip        => "Add new example",
             Relief     => Relief_None
          );
   use New_Buttons;

   package Random_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Random",
             Icon       => Lecture_Add_Random_Example_Icon,
             Relief     => Relief_None,
             Tip => (  "Add a new singleton example generated "
                    &  "using random generator"
          )         );
   use Random_Buttons;

   package Redo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Redo",
             Icon       => "gtk-redo",
             Tip        => "Redo",
             Relief     => Relief_None
          );
   use Redo_Buttons;

   package Undo_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "Undo",
             Icon       => "gtk-undo",
             Tip        => "Undo",
             Relief     => Relief_None
          );
   use Undo_Buttons;

   type Column_Title_Record is new Gtk_Toggle_Button_Record with record
      Grid          : Gtk_Table;
      Feature_Image : Gtk_Image;
      Feature_Label : Gtk_Label;
      Scale_Label   : Gtk_Label;
   end record;
   type Column_Title is access all Column_Title_Record'Class;
   procedure Gtk_New
             (  Widget  : out Column_Title;
                Feature : Feature_Handle;
                Output  : Output_Parameters'Class
             );
   procedure Initialize
             (  Widget  : not null access Column_Title_Record'Class;
                Feature : Feature_Handle;
                Output  : Output_Parameters'Class
             );

   type Column_Title_Array is array (GInt range <>) of Column_Title;
   package Column_Titles is
      new Generic_Unbounded_Array
          (  GInt,
             Column_Title,
             Column_Title_Array,
             null
          );
   type Column_Title_Array_Ptr is access Column_Titles.Unbounded_Array;
   use Column_Titles;

   type Input_Parameters_Ptr  is access Input_Parameters'Class;
   type Output_Parameters_Ptr is access Output_Parameters'Class;
   type Gtk_Fuzzy_Lecture_Editor_Record is
      new Gtk_VBox_Record with
   record
      Action   : Boolean := False;
      Buttons  : Gtk_HBox;
      Model    : Gtk_Fuzzy_Data_Store;
      Add      : New_Buttons.Gtk_Style_Button;
      Copy     : Copy_Buttons.Gtk_Style_Button;
      Delete   : Delete_Buttons.Gtk_Style_Button;
      Random   : Random_Buttons.Gtk_Style_Button;
      Redo     : Redo_Buttons.Gtk_Style_Button;
      Undo     : Undo_Buttons.Gtk_Style_Button;
      View     : Gtk_Tree_View;
      Titles   : Column_Title_Array_Ptr;
      Input    : Input_Parameters_Ptr :=
                    new Input_Parameters'(Input_Defaults);
      Output   : Output_Parameters_Ptr :=
                    new Output_Parameters'(Output_Defaults);
   end record;
   procedure Reset_Columns
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Editor_Record;
                From   : Positive
             );

   package Row_Sets is new Generic_Set (GInt, -1);

   procedure Add_Value_Column
             (  Widget  : not null access
                          Gtk_Fuzzy_Lecture_Editor_Record;
                Feature : Feature_Handle
             );

   function Get_Selection
            (  Editor : not null access
                        Gtk_Fuzzy_Lecture_Editor_Record'Class
            )  return Row_Sets.Set;

   function Get_Selection
            (  Editor : not null access
                        Gtk_Fuzzy_Lecture_Editor_Record'Class
            )  return GInt_Array;

   type Cell_Renderer_Record is
      new Gtk_Cell_Renderer_Fuzzy_Record with
   record
      Column : GInt;
   end record;
   type Cell_Renderer is access all Cell_Renderer_Record'Class;

   procedure Add
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Add_Random
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Added_Column
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Column_Selected
             (  Column : access Column_Title_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Commit
             (  Cell   : access Cell_Renderer_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Copy
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Delete
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Deleted_Column
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Destroyed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Redo
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Redo_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Style_Updated
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Undo
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
   procedure Undo_Changed
             (  Object : access GObject_Record'Class;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );

   package Column_Handlers is
      new Gtk.Handlers.User_Callback
          (  Column_Title_Record,
             Gtk_Fuzzy_Lecture_Editor
          );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Fuzzy_Lecture_Editor
          );

   package Renderer_Handlers is
      new Gtk.Handlers.User_Callback
          (  Cell_Renderer_Record,
             Gtk_Fuzzy_Lecture_Editor
          );

   type Row_Set_Ptr is access all Row_Sets.Set;
   package Foreach is new Selected_Foreach_User_Data (Row_Set_Ptr);
   procedure Browsing_Selection
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Row_Set_Ptr
             );

   package Example_Cell_Data is
      new Set_Column_Cell_Data (Gtk_Fuzzy_Lecture_Editor);
   procedure Example_No_Func
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access
                         Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Editor : Gtk_Fuzzy_Lecture_Editor
             );
end Gtk.Fuzzy_Lecture_Editor;
