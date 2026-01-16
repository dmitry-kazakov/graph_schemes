--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Lecture_Diff                      Luebeck            --
--  Interface                                      Summer, 2006       --
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
--
--  This  package provides tools for comparing fuzzy training sets under
--  GTK+.  The  widget  Gtk_Fuzzy_Lecture_Diff  represents training sets
--  comparison. It contains a  tree  view  widget  which  indicates  the
--  differences between the features of both sets. The Dialog boxes  are
--  provided  to  represent  training  sets  comparison.  See  the  Show
--  procedures.
--
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;      use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Observer_Handle;  use Fuzzy.Lecture.Observer_Handle;
with GLib;                           use GLib;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Fuzzy_Object;               use Gtk.Fuzzy_Object;
with Gtk.Main.Router;                use Gtk.Main.Router;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Widget;                     use Gtk.Widget;
with Gtk.Window;                     use Gtk.Window;

with Gtk.Handlers;
with Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store;

package Gtk.Fuzzy_Lecture_Diff is
--
-- Class_Name -- The name of the tree view widget class
--
   Class_Name : constant String := Prefix & "LectureDiff";
--
-- Gtk_Fuzzy_Lecture_Diff_Record -- The widget type
--
-- Style properties (of the Gtk_Tree_View component):
--
--    diff-icon                 - The  stock  name  of the icon used for
--                                the feature's difference columns.
--    diff-icon-size            - The icon size of the icon used for the
--                                feature's difference columns.  Default
--                                value is Icon_Size_Menu.
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
-- Additionally the component it has the style properties of:
--
--    Gtk_Cell_Renderer_Fuzzy_Boolean (see Fuzzy.Logic.Gtk_View)
--
   type Gtk_Fuzzy_Lecture_Diff_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Lecture_Diff is
      access all Gtk_Fuzzy_Lecture_Diff_Record'Class;
--
-- Get_Iter -- Composition of an iterator
--
--    Widget     - The widget
--    Example    - An example number
--    Complement - The image type
--
-- Returns :
--
--    The tree iterator of
--
-- Exceptions :
--
--    Constraint_Error - Wrong example or widget
--
   function Get_Iter
            (  Widget     : not null access
                            Gtk_Fuzzy_Lecture_Diff_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter;
--
-- Get_Reference_Lesson -- Get the reference training set
--
--    Widget - The widget
--
-- The  result is an invalid handle when no set has been placed into the
-- widget.
--
-- Returns :
--
--    A handle to the training set
--
   function Get_Reference_Lesson
            (  Widget : not null access Gtk_Fuzzy_Lecture_Diff_Record
            )  return Lecture_Handle;
--
-- Get_Result_Lesson -- Get the result training set
--
--    Widget - The widget
--
-- The  result is an invalid handle when no set has been placed into the
-- widget.
--
-- Returns :
--
--    A handle to the training set
--
   function Get_Result_Lesson
            (  Widget : not null access Gtk_Fuzzy_Lecture_Diff_Record
            )  return Lecture_Handle;
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
            (  Widget : not null access Gtk_Fuzzy_Lecture_Diff_Record
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
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Lecture_Diff);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Lecture_Diff_Record'Class
             );
--
-- Put -- Data into the widget
--
--    Widget    - The result
--    Reference - A handle to the reference training set
--    Result    - A handle to the result training set
--  [ Size ]    - The cache size in number of examples per feature
--    Input     - Input formatting parameters
--    Output    - Output formatting parameters
--    Symmetric - Show only shared features
--
-- This  procedure causes the widget to show the date. When Reference is
-- invalid the widget will  show  an  empty  training  set.  The  widget
-- represents  a  difference  between  the Reference and Result training
-- sets. The first two columns of the widget are the type and the number
-- of a training example. The following columns are grouped in triplets.
-- Each triplet corresponds to a common feature in both sets. Usually it
-- is only one feature, a class-feature. In a triplet the  first  column
-- is from the result set, the third is from the reference set  and  the
-- second in the middle  is  the  deifference  between  them.  The  rest
-- columns represent unique features from the reference set.
--
   procedure Put
             (  Widget    : not null access
                            Gtk_Fuzzy_Lecture_Diff_Record;
                Reference : Lecture_Handle;
                Result    : Lecture_Handle;
                Input     : Input_Parameters'Class  := Input_Defaults;
                Output    : Output_Parameters'Class := Output_Defaults;
                Symmetric : Boolean                 := False
             );
   procedure Put
             (  Widget    : not null access
                            Gtk_Fuzzy_Lecture_Diff_Record;
                Reference : Lecture_Handle;
                Result    : Lecture_Handle;
                Size      : Positive;
                Input     : Input_Parameters'Class  := Input_Defaults;
                Output    : Output_Parameters'Class := Output_Defaults;
                Symmetric : Boolean                 := False
             );
--
-- Set_Visible
--
--    Widget     - The widget
--    Example    - An example number
--    Complement - The image type
--    Feature    - The feature of
--    Reference  - Flag if the feature is of the reference set
--
-- This procedure makes the specified example visible by  scrolling  the
-- widget  when  necessary.  When  Feature  specifies  a  feature of the
-- training  set,  the  procedure scrolls to the corresponding column of
-- the widget as well. When Reference is True the Feature denotes one of
-- the reference training set. Otherwise it is a feature either from the
-- result or from reference set.
--
   procedure Set_Visible
             (  Widget     : not null access
                             Gtk_Fuzzy_Lecture_Diff_Record;
                Example    : Positive;
                Complement : Boolean;
                Feature    : Feature_Handle := No_Feature;
                Reference  : Boolean        := True
             );
--
-- Show -- Training set in a dialog box
--
--    Reference - A handle to the reference training set
--    Result    - A handle to the result training set
--  [ Size ]    - The cache size in number of examples per feature
--    Title     - Of the dialog
--    Button    - The button name
--    Parent    - The parent of the dialog
--    Input     - Input formatting parameters
--    Output    - Output formatting parameters
--    Symmetric - Show only shared features
--    Flags     - The flags
--
-- This  procedure  causes  a dialog  to be shown with the training sets
-- indicated in. The dialog shown has a confirmation button if Button is
-- not  empty.  The  procedures  don't return until the user presses the
-- button or closes the window. The parameters Title, Parent, Flags  are
-- ones described for dialogs in Gtk.Dialog.
--
   procedure Show
             (  Reference : Lecture_Handle;
                Result    : Lecture_Handle;
                Title     : UTF8_String             := "";
                Button    : UTF8_String             := "_OK";
                Parent    : Gtk_Window              := null;
                Input     : Input_Parameters'Class  := Input_Defaults;
                Output    : Output_Parameters'Class := Output_Defaults;
                Symmetric : Boolean                 := False;
                Flags     : Gtk_Dialog_Flags        := Modal
             );
   procedure Show
             (  Reference : Lecture_Handle;
                Result    : Lecture_Handle;
                Size      : Positive;
                Title     : UTF8_String             := "";
                Button    : UTF8_String             := "_OK";
                Parent    : Gtk_Window              := null;
                Input     : Input_Parameters'Class  := Input_Defaults;
                Output    : Output_Parameters'Class := Output_Defaults;
                Symmetric : Boolean                 := False;
                Flags     : Gtk_Dialog_Flags        := Modal
             );
private
   use Gtk.Tree_Model.Fuzzy_Lectures_Diff_Store;
   use Fuzzy.Lecture;

   type Input_Parameters_Ptr  is access Input_Parameters'Class;
   type Output_Parameters_Ptr is access Output_Parameters'Class;

   type Fuzzy_Lecture_Observer
        (  Widget : not null access Gtk_Fuzzy_Lecture_Diff_Record'Class;
           Lesson : not null access Lecture_Object'Class
        )  is new Lecture_Observer (Lesson) with null record;
--
-- Added -- Overrides Fuzzy.Lecture...
--
   procedure Added
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Changed -- Overrides Fuzzy.Lecture...
--
   procedure Changed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             );
--
-- Deleted -- Overrides Fuzzy.Lecture...
--
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             );
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Renamed -- Overrides Fuzzy.Lecture...
--
   procedure Renamed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             );

   type Gtk_Fuzzy_Lecture_Diff_Record is
      new Gtk_Scrolled_Window_Record with
   record
      Model     : Gtk_Fuzzy_Lectures_Diff_Store;
      View      : Gtk_Tree_View;
      Input     : Input_Parameters_Ptr;
      Output    : Output_Parameters_Ptr;
      Reference : Fuzzy.Lecture.Observer_Handle.Handle;
      Result    : Fuzzy.Lecture.Observer_Handle.Handle;
      Columns   : GInt := 2;
      Shared    : GInt := 0; -- Number of shared columns
      Symmetric : Boolean := False;
   end record;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Diff   : Gtk_Fuzzy_Lecture_Diff
             );

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Diff   : Gtk_Fuzzy_Lecture_Diff
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Lecture_Diff
          );

   type Added_Data
        (  Observer : not null access Fuzzy_Lecture_Observer
        )  is new Request_Data with null record;
   procedure Service (Data : in out Added_Data);

   type Deleted_Data
        (  Observer : not null access Fuzzy_Lecture_Observer
        )  is new Request_Data with null record;
   procedure Service (Data : in out Deleted_Data);

   type Renamed_Data
        (  Observer : not null access Fuzzy_Lecture_Observer;
           Feature  : not null access Feature_Object'Class;
           Length   : Natural
        )  is new Request_Data with
   record
      Name : String (1..Length);
   end record;
   procedure Service (Data : in out Renamed_Data);

end Gtk.Fuzzy_Lecture_Diff;
