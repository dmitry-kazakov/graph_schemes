--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Classifier_Report                 Luebeck            --
--  Interface                                      Winter, 2003       --
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

with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Fuzzy.Gtk_Icon_Factory;   use Fuzzy.Gtk_Icon_Factory;
with Fuzzy.Lecture.Handle;     use Fuzzy.Lecture.Handle;
with GLib;                     use GLib;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Fuzzy_Classifier;     use Gtk.Fuzzy_Classifier;
with Gtk.Label;                use Gtk.Label;
with Gtk.List_Store;           use Gtk.List_Store;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Table;                use Gtk.Table;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Window;               use Gtk.Window;
with Gtk.Fuzzy_Object;         use Gtk.Fuzzy_Object;
with Gtk.Tree_View;            use Gtk.Tree_View;

with Fuzzy.Intuitionistic;
with Gtk.Fuzzy_Generalization_Combo;
with Gtk.Fuzzy_Lecture_Diff;
with Gtk.Fuzzy_Lecture;
with Gtk.Handlers;
with Gtk.Generic_Style_Button;

package Gtk.Fuzzy_Classifier_Report is
   use Gtk.Fuzzy_Generalization_Combo;
--
-- Class_Name -- The name of the widget class
--
   Class_Name : constant String :=
      Gtk.Fuzzy_Classifier.Class_Name & "Report";
--
-- Gtk_Fuzzy_Classifier_Report_Record -- The widget type
--
-- The widget consists of panes:
--    ._____________________._______________________.
--    |                     |   |                   |
--    |   General report    |   | Detailed report / |
--    |     and partial     |   | Original set      |
--    |   classification    |___| (optional)        |
--    |                     ||  |                   |
--    |---------------------||  |                   |
--    | [-]                 ||  |                   |
--    |  |-[+]  Classifier  ||__|                   |
--    |  |-                 |   |                   |
--    |  |-[-]              |   |___________________|
--    |  |  |-              |   |  Buttons  |       |
--    |_____________________|___|___|___|___|_______|
--
-- The  general  report  is  an  intuitionistic  fuzzy  set  of means of
-- classification errors. The detailed report represents classifications
-- and  the  corresponding  examples  from  the  control  set. The field
-- Classification is optional and appears when a partial  classification
-- is  queried  by  pressing  a  button.  The  original  training set is
-- optional and appears when specified.
--
-- Style properties:
--
--    button-gap             - The  gaps between  buttons.  Integer, the
--                             default is 3.
--    comparison-tab-title   - Title of the comparison tab. String.
--    domain-column-title    - Title of the first column  in the general
--                             report. String.
--    explained-column-title - Title of the third column  in the general
--                             report. String.
--    no-example-message     - The message no example selected. String.
--    no-match-message       - The message nothing is matched. String.
--    no-classifier-message  - The  message   no   classifier   selected.
--                             String.
--    source-tab-title       - Title of the source tab. String.
--    verified-column-title  - Title of the second column in the general
--                             report. String.
--
   type Gtk_Fuzzy_Classifier_Report_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Classifier_Report is
      access all Gtk_Fuzzy_Classifier_Report_Record'Class;
--
-- Error -- User notification
--
--    Widget  - The report view widget
--    Message - The error message
--
-- This  procedure  is called to show a message to the user. The default
-- implementation pops a message box.
--
   procedure Error
             (  Widget  : not null access
                          Gtk_Fuzzy_Classifier_Report_Record;
                Message : UTF8_String
             );
--
-- Error_Reset -- End of user notification
--
--    Widget - The report view widget
--
-- This  procedure  is  called  to  remove any messages to the user. The
-- default implementation does nothing.
--
   procedure Error_Reset
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Report_Record
             );
--
-- Get_Selected_Example -- The currently selected example in the report
--
--    Widget     - The report view widget
--    Example    - In the reference set example
--    Complement - False if the example is negative
--
-- This  procedure  is  used  to  determine  which  example is currently
-- selected in the detailed report pane.
--
-- Exceptions :
--
--    Constraint_Error - No example selected
--
   procedure Get_Selected_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : out Positive;
                Complement : out Boolean
             );
--
-- Get_Selected_Source_Example -- The currently selected source  example
--                                in the report
--
--    Widget     - The report view widget
--    Example    - In the source set example
--    Complement - False if the example is negative
--
-- This  procedure  is  used  to  determine  which  example is currently
-- selected in the source set pane.
--
-- Exceptions :
--
--    Constraint_Error - No example selected
--
   procedure Get_Selected_Source_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : out Positive;
                Complement : out Boolean
             );
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
   procedure Gtk_New (Widget : out Gtk_Fuzzy_Classifier_Report);
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - The widget to initialize
--    Tooltips - The tooltips group to use or null
--
-- When  a  new  type is derived from the base, this procedure has to be
-- called as a part of widget object initialization.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Report_Record'Class
             );
--
-- Put -- A classifier into the widget
--
--    Widget     - The report view widget
--    Classifier - A handle to
--    Control    - A handle to the control set
--    Result     - A handle to the classification results
--  [ Source ]   - A handle to the source set
--    Report     - Overal result
--
-- This procedure causes the widget to  show  the  data.  Control  is  a
-- handle to the control set for which Result  contains  classifications
-- of the examples from the former.  Report  is  the  means  of  errors.
-- Result and Report are usually obtained  by  a  call  to  Verify  from
-- Fuzzy.Classifier.Handle. The variant with Result and Report
-- omitted uses
--
   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Source     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set
             );
--
-- Set_Selected_Example -- An example in the report
--
--    Widget     - The report view widget
--    Example    - In the reference set example
--    Complement - False if the example is negative
--
-- This  procedure  is  used to select an example in the detailed report
-- pane.
--
-- Exceptions :
--
--    Constraint_Error - Wrong selection
--
   procedure Set_Selected_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : Positive;
                Complement : Boolean
             );
--
-- Set_Selected_Source_Example -- A source example in the report
--
--    Widget     - The report view widget
--    Example    - In the source set example
--    Complement - False if the example is negative
--
-- This procedure is used to select an example in the source set pane.
--
-- Exceptions :
--
--    Constraint_Error - Wrong selection
--
   procedure Set_Selected_Source_Example
             (  Widget     : not null access
                             Gtk_Fuzzy_Classifier_Report_Record;
                Example    : Positive;
                Complement : Boolean
             );
--
-- Show -- Classifier in a dialog box
--
--    Classifier - A handle to
--    Control    - A handle to the control set
--    Result     - A handle to the classification results
--  [ Source ]   - A handle to the source set
--    Report     - Overal result
--    Title      - Of the dialog
--    Button     - The button name
--    Parent     - The parent of the dialog
--    Flags      - The flags
--
-- This procedure causes a dialog to be shown with a verification report
-- indicated in. The dialog shown has a confirmation button if Button is
-- not  empty.  The  procedures  don't return until the user presses the
-- button or closes the window. The parameters Title, Parent, Flags  are
-- ones described for dialogs in Gtk.Dialog.
--
   procedure Show
             (  Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Source     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             );
   procedure Show
             (  Classifier : Classifier_Handle;
                Control    : Lecture_Handle;
                Result     : Lecture_Handle;
                Report     : Fuzzy.Intuitionistic.Set;
                Title      : UTF8_String      := "";
                Button     : UTF8_String      := "_OK";
                Parent     : Gtk_Window       := null;
                Flags      : Gtk_Dialog_Flags := Modal
             );
private
   use Gtk.Fuzzy_Lecture_Diff;
   use Gtk.Fuzzy_Lecture;

   package Classify_Button is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "Classify",
             Icon       => Classify_Icon,
             Relief     => Relief_None,
             Tip =>
                (  "Classify the selected example using "
                &  "the selected part of the classifier. "
                &  "The training example is selected in the "
                &  "training examples list above. The classifier part "
                &  "is selected on the left, in its tree view. "
                &  "This subtree is used as a classifier. "
                &  "The result is shown in the third column top left "
                &  "to compare with verification result shown in the "
                &  "second column"
          )     );

   package Collapse_Button is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "Collapse",
             Icon       => Collapse_Icon,
             Tip        => "Collapse the classifier's view",
             Relief     => Relief_None
          );

   package Expand_Button is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "Expand",
             Icon       => Explain_Icon,
             Relief     => Relief_None,
             Tip =>
                (  "Expand parts of the classifier related "
                &  "to the selected example. A part of the classifier "
                &  "is unrelated when removing it from the classifier "
                &  "does not change classification of the example "
                &  "selected"
          )     );

   package Find_Button is
      new Gtk.Generic_Style_Button
          (  Class_Name => Prefix & "Find",
             Icon       => Find_Icon,
             Relief     => Relief_None,
             Tip =>
                (  "Find the next source example of the selected part "
                &  "of the classifier, that has contributed to the "
                &  "part. The search starts from the example "
                &  "following the selected one, or from the training "
                &  "set beginning. If an example is found, it is "
                &  "selected"
          )     );

   type Comparison_Data is record
      View    : Gtk_Fuzzy_Lecture_Diff;
      Buttons : Gtk_Table;
      Label   : Gtk_Label;
      Collapse_All      : Collapse_Button.Gtk_Style_Button;
      Classify_Selected : Classify_Button.Gtk_Style_Button;
      Expand_Selected   : Expand_Button.Gtk_Style_Button;
      Generalize        : Gtk_Fuzzy_Generalization_Combo;
   end record;

   type Source_Data is record
      View    : Gtk_Fuzzy_Lecture;
      Buttons : Gtk_Table;
      Label   : Gtk_Label;
      Collapse_All  : Collapse_Button.Gtk_Style_Button;
      Find_Selected : Find_Button.Gtk_Style_Button;
   end record;

   type Gtk_Fuzzy_Classifier_Report_Record is
      new Gtk_Hpaned_Record with
   record
      V_Pane     : Gtk_Vpaned;
      Classifier : Gtk_Fuzzy_Classifier;
      Comparison : Comparison_Data;
      Source     : Source_Data;
      Report     : Gtk_Tree_View;
      Store      : Gtk_List_Store;
      Lectures   : Gtk_Notebook;
   end record;

   procedure Classify_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );
   procedure Collapse
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );
   procedure Expand_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );
   procedure Find_Selected
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );
   procedure Put
             (  Store : Gtk_List_Store;
                Value : Fuzzy.Intuitionistic.Set
             );
   procedure Put
             (  Store : Gtk_List_Store;
                Value : Fuzzy.Intuitionistic.Classification
             );
   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );
   procedure Style_Updated
             (  Object : access GObject_Record'Class;
                Report : Gtk_Fuzzy_Classifier_Report
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Gtk_Fuzzy_Classifier_Report
          );

end Gtk.Fuzzy_Classifier_Report;
