--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Classifier_Pane                        Winter, 2008       --
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

with Gtk.Fuzzy_Classifier;  use Gtk.Fuzzy_Classifier;

with Gtk.Fuzzy_Catalogue.Classification_Boxes;
use  Gtk.Fuzzy_Catalogue.Classification_Boxes;

private package Gtk.Fuzzy_Catalogue.Classifier_Pane is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Classifier_Pane);

   package Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "AddClassifier",
             Icon       => New_Classifier_Icon,
             Tip        => "Create a new classifier",
             Relief     => Relief_None
          );
   use Add_Buttons;

   package Classify_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "UseClassifier",
             Icon       => Classify_Icon,
             Tip        => "Classify examples of a training set",
             Relief     => Relief_None
          );
   use Classify_Buttons;

   package Classify_Input_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "UseClassifierOnce",
             Icon       => Classify_Input_Icon,
             Relief     => Relief_None,
             Tip =>
             (  "Classify an arbitrary instance. "
             &  "You will be asked for the feature values the "
             &  "classifier needs"
          )  );
   use Classify_Input_Buttons;

   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DeleteClassifier",
             Icon       => Stock_Delete,
             Tip        => "Remove selected classifiers or folders",
             Relief     => Relief_None
          );
   use Delete_Buttons;

   package Move_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveClassifier",
             Icon       => Rename_Icon,
             Tip        => "Move selected classifiers or folders",
             Relief     => Relief_None
          );
   use Move_Buttons;

   package Rename_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "RenameClassifier",
             Icon       => Rename_Item_Icon,
             Tip        => "Rename selected classifier or folder",
             Relief     => Relief_None
          );
   use Rename_Buttons;

   package Verify_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "VerifyClassifier",
             Icon       => Verify_Icon,
             Tip    => "Verify selected classifier on a training set",
             Relief => Relief_None
          );
   use Verify_Buttons;

   package View_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ViewClassifier",
             Icon       => Classifier_View_Icon,
             Tip        => "View selected classifier",
             Relief     => Relief_None
          );
   use View_Buttons;
--
-- Classifier_View_Box_Record -- Box of classifier viewing
--
   type Classifier_View_Box_Record is
      new Gtk_Item_Box_Record with
   record
      Classifier : Gtk_Fuzzy_Classifier;
   end record;
   type Classifier_View_Box is
      access all Classifier_View_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item       - The result
--    Classifier - A handle to
--    Title      - Of the item
--    Browser    - The parent widget
--
   procedure Gtk_New
             (  Item       : out Classifier_View_Box;
                Classifier : Classifier_Handle;
                Title      : UTF8_String;
                Browser    : not null access
                             Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item       - To construct
--    Title      - Of the item
--    Classifier - A handle to
--
   procedure Initialize
             (  Item       : not null access
                             Classifier_View_Box_Record'Class;
                Classifier : Classifier_Handle;
                Title      : UTF8_String
             );

   package Classifier_View_Boxes is
      new GLib.Object.Weak_References (Classifier_View_Box_Record);
   use Classifier_View_Boxes;

   type Classifier_Panel_Record is new Panel_Record with record
      Add_Button            : Add_Buttons.Gtk_Style_Button;
      Classify_Button       : Classify_Buttons.Gtk_Style_Button;
      Classify_Input_Button : Classify_Input_Buttons.Gtk_Style_Button;
      Delete_Button         : Delete_Buttons.Gtk_Style_Button;
      Move_Button    : Move_Buttons.Gtk_Style_Button;
      Rename_Button  : Rename_Buttons.Gtk_Style_Button;
      Verify_Button  : Verify_Buttons.Gtk_Style_Button;
      View_Button    : View_Buttons.Gtk_Style_Button;
      Classify       : Classifications.Weak_Reference;
      View           : Classifier_View_Boxes.Weak_Reference;
   end record;
   type Classifier_Panel is access all Classifier_Panel_Record'Class;
--
-- Build_Item_Menu -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Build_Item_Menu
             (  View  : not null access Classifier_Panel_Record;
                Index : Positive
             );
--
-- Check -- Overrides Gtk.Fuzzy_Object...
--
   overriding
   procedure Check
             (  View       : not null access Classifier_Panel_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
--
-- Classify -- Clicked callback
--
   procedure Classify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Classify_Input -- Clicked callback
--
   procedure Classify_Input
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
             (  View       : not null access Classifier_Panel_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
   overriding
   function Get
            (  View       : not null access Classifier_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path;
--
-- Get_Current -- The currently selected classifier
--
   function Get_Current
            (  View     : not null access Classifier_Panel_Record;
               Selected : Selection
            )  return Classifier_Handle;
--
-- Gtk_New -- Factory
--
   procedure Gtk_New
             (  View      : out Classifier_Panel;
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
             (  View      : not null access
                            Classifier_Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             );
--
-- Is_Classifier -- Check if a classifier was selected
--
--    View     - The panel
--    Selected - A selection in
--
-- Returns :
--
--    True if a classifier is selected (exactly one)
--
   function Is_Classifier
            (  View     : not null access Classifier_Panel_Record;
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
   procedure Root_Changed
             (  View : not null access Classifier_Panel_Record
             );
--
-- Selection_Changed -- overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Selection_Changed
             (  View : not null access Classifier_Panel_Record
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
             (  View : not null access Classifier_Panel_Record
             );
--
-- Verify -- Clicked callback
--
   procedure Verify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );

end Gtk.Fuzzy_Catalogue.Classifier_Pane;
