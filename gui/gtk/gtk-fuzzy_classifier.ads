--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Classifier                        Luebeck            --
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
--  This package provides tools for dealing with fuzzy classifiers under
--  GTK+:
--
--  (o)  Gtk_Fuzzy_Classifier is a widget to represent fuzzy classifiers.
--
--  (o)  Dialog  boxes  can  be  used to show fuzzy classifiers. See the
--       Show procedures.
--
--  (o)  The  GTK+  type  GType_Classifier  can  be  used  to  place   a
--       classifier node into a GTK+ value.
--
with Confidence_Factors;       use Confidence_Factors;
with Fuzzy.Classifier;         use Fuzzy.Classifier;
with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Fuzzy.Feature;            use Fuzzy.Feature;
with Fuzzy.Intuitionistic;     use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;     use Fuzzy.Lecture.Handle;
with GLib;                     use GLib;
with GLib.Values;              use GLib.Values;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Fuzzy_Graph;          use Gtk.Fuzzy_Graph;
with Gtk.Fuzzy_Object;         use Gtk.Fuzzy_Object;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Label;                use Gtk.Label;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Indicator;                use Indicator;
with Indicator.Handle;         use Indicator.Handle;

with GLib.Values.Handle;
with Gtk.Handlers;

package Gtk.Fuzzy_Classifier is
--
-- Class_Name -- The name of the widget class
--
   Class_Name : constant String := Prefix & "Classifier";
--
-- Gtk_Fuzzy_Classifier_Record -- The widget type
--
-- Style properties:
--
--    has-in-page-title            - The page title of the graph deduced
--                                   from  has-in  images.  Default   is
--                                   "in".
--    has-not-page-title           - The page title of the graph deduced
--                                   from  has-not  images.  Default  is
--                                   "not in".
--    has-out-page-title           - The page title of the graph deduced
--                                   from  has-not  images.  Default  is
--                                   "out".
--    has-not-out-page-title       - The page title of the graph deduced
--                                   from  has-not  images.  Default  is
--                                   "not out".
--    invalid-classifier-label     - The label of no classifier. Default
--                                   is "Invalid".
--    page-title-spacing           - The  spacing  used  for  the   page
--                                   titles.  It  is the gap between the
--                                   image and the label. The default is
--                                   3.
--    separator-classifier-label   - The label of separator  classifier.
--                                   Default is "Separator".
--    unsupported-classifier-label - The label of an unsupported type of
--                                   classifier. The  default  value  is
--                                   "Unsupported".
--
-- Signals :
--
--    selection-changed - The subclassifier selection is changed
--
   type Gtk_Fuzzy_Classifier_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Classifier is
      access all Gtk_Fuzzy_Classifier_Record'Class;
--
-- Classify_As_Selected -- Classification of a training example
--
--    Widget     - The widget
--    Lesson     - The training set (a handle to)
--    Example    - The example to classify
--    Generalize - The nodes generalization mode
--    Threshold  - The confidence threshold
--
-- This function uses the selected part of a classifier to  classify  an
-- example   as   does  Fuzzy.Classifier.Handle.Classify.  The  behavior
-- depends on the classifier type.
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
            (  Widget     : not null access Gtk_Fuzzy_Classifier_Record;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification;
--
-- Classify_As_Selected -- Classification of a training example
--
--    Widget  - The widget
--    Context - The data context
--
-- This function uses the selected part of a classifier to  classify  an
-- example   as   does  Fuzzy.Classifier.Handle.Classify.  The  behavior
-- depends on the classifier type.
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
            (  Widget     : not null access Gtk_Fuzzy_Classifier_Record;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification;
--
-- Collapse_All -- Collapse view
--
--    Widget  - The widget
--    Visible - Of the visible classifier if false
--
-- This procedure collapses the view of all classifiers or only  of  the
-- visible one, when the parameter Visible is true.
--
   procedure Collapse_All
             (  Widget  : not null access Gtk_Fuzzy_Classifier_Record;
                Visible : Boolean := False
             );
--
-- Expand_Example -- Expand nodes dealing with a training example
--
--    Widget     - The widget
--    Lesson     - A handle to the training set
--    Example    - To use
--    Complement - True if the example is negative
--    Visible    - Of the visible classifier if false
--
-- Nothing  happens when Lesson is an invalid handle or does not contain
-- the  example  specified.  The  operation  affects  only  the  visible
-- classifier if the parameter Visible is true.
--
   procedure Expand_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : Positive;
                Complement : Boolean;
                Visible    : Boolean := False
             );
--
-- Find_Selected -- Classification of a training example
--
--    Widget     - The widget
--    Lesson     - The data context or a teaching set handle
--    Example    - The example to classify
--    Complement - True if the example is negative
--    Viewer     - A progress indication object or a handle to
--
-- These  procedures use the selected part of a classifier to browse the
-- training  set  lesson.  It  starts  from  the image following the one
-- identified by Example / Complement and searches for the first Example
-- / Complement which matches the selected part of the classifier.  When
-- the end of the training set is reached the procedures  wrap  process.
-- When  the  whole  set  is  searched  without  a  result, End_Error is
-- propagated.  The behavior depends on the classifier type. Example and
-- Complement are changed only upon successful completion.
--
-- Exceptions :
--
--    Constraint_Error - No selection, wrong handle (Lesson)
--    End_Error        - Nothing found
--
   procedure Find_Selected
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Viewer     : Indicator_Handle
             );
   procedure Find_Selected
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Lesson     : Lecture_Handle;
                Example    : in out Positive;
                Complement : in out Boolean;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Get_Classifier -- The currently displayed classifier
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the classifier (it can be invalid)
--
   function Get_Classifier
            (  Widget : not null access Gtk_Fuzzy_Classifier_Record
            )  return Classifier_Handle;
--
-- Get_Classifier -- Get classifier from a GTK+ value
--
--    Value - A GTK+ value
--
-- The  value  must  have  been  initialized  using  Init  with the type
-- GType_Classifier.
--
-- Returns :
--
--    A handle to the root node of the classifier
--
-- Exceptions :
--
--    Constraint_Error - The value is not a classifier
--
   function Get_Classifier (Value : GValue) return Classifier_Handle;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Classifier);
--
-- GType_Classifier -- The GTK+ type of classifiers
--
   function GType_Classifier return GType;
--
-- Has_Selected -- Subclassifier check
--
--    Widget - The widget
--
-- Returns :
--
--    True if there is a subclassifier selected
--
   function Has_Selected
            (  Widget : not null access Gtk_Fuzzy_Classifier_Record
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
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Record'Class
             );
--
-- Put -- A classifier into the widget
--
--    Widget     - The widget
--    Classifier - A handle to
--
-- This procedure  causes  the  widget  to  show  the  classifier.  When
-- Classifier is invalid, the widget will be empty.
--
   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Record;
                Classifier : Classifier_Handle
             );
--
-- Set_Classifier -- Set a value
--
--    Value      - To set
--    Classifier - A handle to the classifier
--
-- This  procedure  sets  a  classifier  into  GTK+  value,   previously
-- initialized using Init with the parameter GType_Classifier.
--
-- Exceptions :
--
--    Constraint_Error - Not an object value, invalid handle
--
   procedure Set_Classifier
             (  Value      : in out GValue;
                Classifier : Classifier_Handle
             );
--
-- Show -- Classifier in a dialog box
--
--    Classifier - A handle to the classifier
--    Title      - Of the dialog
--    Button     - The button name
--    Parent     - The parent of the dialog
--    Flags      - The flags
--
-- This procedure causes a  dialog  to  be  shown  with  the  classifier
-- indicated in. The dialog shown has a confirmation button if Button is
-- not  empty.  The  procedures  don't return until the user presses the
-- button or closes the window. The parameters Title, Parent, Flags  are
-- ones described for dialogs in Gtk.Dialog.
--
   procedure Show
             (  Classifier : Classifier_Handle;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             );
private
   type Content_State is (Invalid, Scheme, Unknown);
   type Graph_View_Page is record
      Box   : Gtk_HBox;
      Label : Gtk_Label;
      Graph : Gtk_Fuzzy_Graph;
   end record;
   type Graph_Array is array (Image_Type) of Graph_View_Page;
   type Content_View (State : Content_State := Unknown) is record
      case State is
         when Invalid =>
            null;
         when Unknown =>
            null;
         when Scheme  =>
            Image  : Image_Type := Has_In;
            Tabs   : Gtk_Notebook;
            Graphs : Graph_Array;
      end case;
   end record;

   type Gtk_Fuzzy_Classifier_Record is
      new Gtk_Box_Record with
   record
      Content    : Content_View;
      Label      : Gtk_Label;   -- Can be null
      Classifier : Classifier_Handle;
   end record;
--
-- Changed_Selection -- Event handler
--
   procedure Changed_Selection
             (  Object : access GObject_Record'Class;
                View   : Gtk_Fuzzy_Classifier
             );
--
-- Changed_Page -- Event handler
--
   procedure Changed_Page
             (  Object : access GObject_Record'Class;
                Params : Glib.Values.GValues;
                View   : Gtk_Fuzzy_Classifier
             );
--
-- Style_Updated -- Event handler
--
   procedure Style_Updated
             (  Object : access GObject_Record'Class;
                View   : Gtk_Fuzzy_Classifier
             );
--
-- Gtk_Values -- Interfacing of classifier to GTK+ values
--
   package Gtk_Values is
      new GLib.Values.Handle
          (  Type_Name       => "GFuzzyClassifier",
             Object_Type     => Classifier_Object,
             Object_Type_Ptr => Classifier_Object_Ptr,
             Handle_Type     => Classifier_Handle
          );
   function Get_Classifier (Value : GValue) return Classifier_Handle
      renames Gtk_Values.Get_Handle;
   function GType_Classifier return GType renames Gtk_Values.Get_Type;
   procedure Set_Classifier
             (  Value      : in out GValue;
                Classifier : Classifier_Handle
             )  renames Gtk_Values.Set_Handle;

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Fuzzy_Classifier
          );
end Gtk.Fuzzy_Classifier;
