--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Lecture                           Luebeck            --
--  Interface                                      Summer, 2006       --
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
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Observer_Handle;  use Fuzzy.Lecture.Observer_Handle;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Fuzzy_Object;               use Gtk.Fuzzy_Object;
with Gtk.Main.Router;                use Gtk.Main.Router;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Widget;                     use Gtk.Widget;
with Gtk.Window;                     use Gtk.Window;

with Gtk.Handlers;
with Gtk.Tree_Model.Fuzzy_Lecture_Store;

package Gtk.Fuzzy_Lecture is
--
-- Class_Name -- The name of the tree view widget class
--
   Class_Name : constant String := Prefix & "Lecture";
--
-- Gtk_Fuzzy_Lecture_Record -- The widget type
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
   type Gtk_Fuzzy_Lecture_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Lecture is
      access all Gtk_Fuzzy_Lecture_Record'Class;
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
            (  Widget     : not null access Gtk_Fuzzy_Lecture_Record;
               Example    : Positive;
               Complement : Boolean
            )  return Gtk_Tree_Iter;
--
-- Get_Lesson -- Get the training set associated with
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
   function Get_Lesson
            (  Widget : not null access Gtk_Fuzzy_Lecture_Record
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
            (  Widget : not null access Gtk_Fuzzy_Lecture_Record
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
-- Cache is not used, when the parameter Size is missing.
--
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Lecture);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record'Class
             );
--
-- Put -- A lecture into the widget
--
--    Widget - The widget
--    Lesson - A handle to the training set
--  [ Size ] - The cache size in number of examples per feature
--    Input  - Input formatting parameters
--    Output - Output formatting parameters
--
-- This  procedure  causes  the  widget  to  show Lesson. When Lesson is
-- invalid the widget will show an empty training set. The parameter
-- Size specifies the cache size in examples per feature.
--
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults
             );
   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Lecture_Record;
                Lesson : Lecture_Handle;
                Size   : Positive;
                Input  : Input_Parameters'Class  := Input_Defaults;
                Output : Output_Parameters'Class := Output_Defaults
             );
--
-- Set_Visible
--
--    Widget     - The widget
--    Example    - An example number
--    Complement - The image type
--    Feature    - The feature of
--
-- This procedure makes the specified example visible by  scrolling  the
-- widget  when  necessary.  When  Feature  specifies  a  feature of the
-- training  set,  the  procedure scrolls to the corresponding column of
-- the widget as well.
--
   procedure Set_Visible
             (  Widget     : not null access Gtk_Fuzzy_Lecture_Record;
                Example    : Positive;
                Complement : Boolean;
                Feature    : Feature_Handle := No_Feature
             );
--
-- Show -- Training set in a dialog box
--
--    Lesson - A handle to the lecture
--  [ Size ] - The cache size in number of examples per feature
--    Title  - Of the dialog
--    Button - The button name
--    Parent - The parent of the dialog
--    Input  - Input formatting parameters
--    Output - Output formatting parameters
--    Flags  - The flags
--
-- This  procedure  causes  a  dialog  to be shown with the training set
-- indicated in. The dialog shown has a confirmation button if Button is
-- not  empty.  The  procedures  don't return until the user presses the
-- button or closes the window. The parameters Title, Parent, Flags  are
-- ones described for dialogs in Gtk.Dialog.
--
   procedure Show
             (  Lesson   : Lecture_Handle;
                Title    : UTF8_String             := "";
                Button   : UTF8_String             := "_OK";
                Parent   : Gtk_Window              := null;
                Input    : Input_Parameters'Class  := Input_Defaults;
                Output   : Output_Parameters'Class := Output_Defaults;
                Flags    : Gtk_Dialog_Flags        := Modal
             );
   procedure Show
             (  Lesson   : Lecture_Handle;
                Size     : Positive;
                Title    : UTF8_String             := "";
                Button   : UTF8_String             := "_OK";
                Parent   : Gtk_Window              := null;
                Input    : Input_Parameters'Class  := Input_Defaults;
                Output   : Output_Parameters'Class := Output_Defaults;
                Flags    : Gtk_Dialog_Flags        := Modal
             );
private
   use Gtk.Tree_Model.Fuzzy_Lecture_Store;
   use Fuzzy.Lecture;

   type Input_Parameters_Ptr  is access Input_Parameters'Class;
   type Output_Parameters_Ptr is access Output_Parameters'Class;

   type Fuzzy_Lecture_Observer
        (  Widget : not null access Gtk_Fuzzy_Lecture_Record'Class;
           Lesson : not null access Lecture_Object'Class
        )  is new Lecture_Observer (Lesson) with
   record
      Input  : Input_Parameters_Ptr;
      Output : Output_Parameters_Ptr;
   end record;
--
-- Added -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Added
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Changed -- Overrides Fuzzy.Lecture...
--
   overriding
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
   overriding
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             );
   overriding
   procedure Deleted
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
--
-- Finalize -- Destructor
--
   overriding
   procedure Finalize (Observer : in out Fuzzy_Lecture_Observer);
--
-- Renamed -- Overrides Object.Archived.Feature...
--
   overriding
   procedure Renamed
             (  Observer : in out Fuzzy_Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             );

   type Gtk_Fuzzy_Lecture_Record is
      new Gtk_Scrolled_Window_Record with
   record
      Model    : Gtk_Fuzzy_Lecture_Store;
      View     : Gtk_Tree_View;
      Observer : Fuzzy.Lecture.Observer_Handle.Handle;
   end record;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Lecture : Gtk_Fuzzy_Lecture
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Lecture
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

end Gtk.Fuzzy_Lecture;
