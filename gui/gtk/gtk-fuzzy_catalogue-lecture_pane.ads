--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Pane           Luebeck            --
--  Interface                                      Winter, 2008       --
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

with Gtk.Fuzzy_Lecture;         use Gtk.Fuzzy_Lecture;
with Gtk.Fuzzy_Lecture_Diff;    use Gtk.Fuzzy_Lecture_Diff;

with Gtk.Fuzzy_Catalogue.Classification_Boxes;
with Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text;
with Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text;

private package Gtk.Fuzzy_Catalogue.Lecture_Pane is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Lecture_Pane);

   use Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text;
   use Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text;
   use Classification_Boxes;

   package Add_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "AddLecture",
             Icon       => New_Lecture_Icon,
             Tip        => "Create a new training set",
             Relief     => Relief_None
          );
   use Add_Buttons;

   package Classify_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ClassifyLecture",
             Icon       => Classify_Icon,
             Tip        => "Classify examples of a training set",
             Relief     => Relief_None
          );
   use Classify_Buttons;

   package Copy_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "CopyLecture",
             Icon       => Lecture_Copy_Icon,
             Tip        => "Create a copy of the training set",
             Relief     => Relief_None
          );
   use Copy_Buttons;

   package Delete_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DeleteLecture",
             Icon       => Stock_Delete,
             Tip        => "Remove selected training sets or folders",
             Relief     => Relief_None
          );
   use Delete_Buttons;

   package Diff_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DiffLecture",
             Icon       => Compare_Lectures_Icon,
             Tip        => "Compare two training sets",
             Relief     => Relief_None
          );
   use Diff_Buttons;

   package Get_As_Text_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "GetAsText",
             Icon       => New_Lecture_From_Text_Icon,
             Relief     => Relief_None,
             Tip =>
                (  "Read a training set from a plain text file. "
                &  "The text file is one example per line"
          )     );
   use Get_As_Text_Buttons;

   package Learn_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "LearnFromLecture",
             Icon       => Learn_Icon,
             Tip        => "Learn from examples of a training set",
             Relief     => Relief_None
          );
   use Learn_Buttons;

   package Lecture_Edit_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "EditLecture",
             Icon       => Lecture_Edit_Icon,
             Tip        => "Edit training set",
             Relief     => Relief_None
          );
   use Lecture_Edit_Buttons;

   package Move_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "MoveLecture",
             Icon       => Rename_Icon,
             Tip        => "Move selected training sets or folders",
             Relief     => Relief_None
          );
   use Move_Buttons;

   package Rename_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "RenameLecture",
             Icon       => Rename_Item_Icon,
             Tip        => "Rename selected training set or folder",
             Relief     => Relief_None
          );
   use Rename_Buttons;

   package Save_As_Text_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "SaveAsText",
             Icon       => Save_As_Text_Icon,
             Relief     => Relief_None,
             Tip =>
                (  "Save training set as a plain text file. "
                &  "The text file is one example per line"
          )     );
   use Save_As_Text_Buttons;

   package Verify_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "VerifyLecture",
             Icon       => Verify_Icon,
             Relief     => Relief_None,
             Tip => "Verify a classifier on the selected training set"
          );
   use Verify_Buttons;

   package View_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "ViewLecture",
             Icon       => Lecture_View_Icon,
             Tip        => "View selected training set",
             Relief     => Relief_None
          );
   use View_Buttons;
--
-- Lecture_Compare_Box_Record -- Box of training set viewing
--
   type Lecture_Compare_Box_Record is
      new Gtk_Item_Box_Record with
   record
      Lesson : Gtk_Fuzzy_Lecture_Diff;
   end record;
   type Lecture_Compare_Box is
      access all Lecture_Compare_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    First   - A handle to a training set
--    Second  - A handle to a training set
--    Browser - The parent widget
--
   procedure Gtk_New
             (   Item    : out Lecture_Compare_Box;
                 First   : Lecture_Handle;
                 Second  : Lecture_Handle;
                 Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item   - To construct
--    First  - A handle to a training set
--    Second - A handle to a training set
--
   procedure Initialize
             (  Item   : access Lecture_Compare_Box_Record'Class;
                First  : Lecture_Handle;
                Second : Lecture_Handle
             );
   package Lecture_Compare_Boxes is
      new GLib.Object.Weak_References (Lecture_Compare_Box_Record);
   use Lecture_Compare_Boxes;
--
-- Lecture_View_Box_Record -- Box of training set viewing
--
   type Lecture_View_Box_Record is new Gtk_Item_Box_Record with record
      Lesson : Gtk_Fuzzy_Lecture;
   end record;
   type Lecture_View_Box is access all Lecture_View_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Lesson  - A handle to
--    Title   - Of the item
--    Browser - The parent widget
--
   procedure Gtk_New
             (   Item    : out Lecture_View_Box;
                 Lesson  : Lecture_Handle;
                 Title   : UTF8_String;
                 Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item   - To construct
--    Lesson - A handle to
--    Title  - Of the item
--
   procedure Initialize
             (  Item   : access Lecture_View_Box_Record'Class;
                Lesson : Lecture_Handle;
                Title  : UTF8_String
             );
   package Lecture_View_Boxes is
      new GLib.Object.Weak_References (Lecture_View_Box_Record);
   use Lecture_View_Boxes;

   package Lecture_Create_From_Text_Boxes is
      new GLib.Object.Weak_References
          (  Lecture_Create_From_Text_Box_Record
          );
   use Lecture_Create_From_Text_Boxes;

   package Lecture_Save_As_Text_Boxes is
      new GLib.Object.Weak_References (Lecture_Save_As_Text_Box_Record);
   use Lecture_Save_As_Text_Boxes;

   type Lecture_Panel_Record is new Panel_Record with record
      Add_Button           : Add_Buttons.Gtk_Style_Button;
      Classify_Button      : Classify_Buttons.Gtk_Style_Button;
      Copy_Button          : Copy_Buttons.Gtk_Style_Button;
      Delete_Button        : Delete_Buttons.Gtk_Style_Button;
      Diff_Button          : Diff_Buttons.Gtk_Style_Button;
      Edit_Button          : Lecture_Edit_Buttons.Gtk_Style_Button;
      Learn_Button         : Learn_Buttons.Gtk_Style_Button;
      Move_Button          : Move_Buttons.Gtk_Style_Button;
      Rename_Button        : Rename_Buttons.Gtk_Style_Button;
      Verify_Button        : Verify_Buttons.Gtk_Style_Button;
      View_Button          : View_Buttons.Gtk_Style_Button;
      Get_From_Text_Button : Get_As_Text_Buttons.Gtk_Style_Button;
      Save_As_Text_Button  : Save_As_Text_Buttons.Gtk_Style_Button;
      Classify         : Classifications.Weak_Reference;
      Compare          : Lecture_Compare_Boxes.Weak_Reference;
      Create_From_Text : Lecture_Create_From_Text_Boxes.Weak_Reference;
      Save_As_Text     : Lecture_Save_As_Text_Boxes.Weak_Reference;
      Verify           : Classifications.Weak_Reference;
      View             : Lecture_View_Boxes.Weak_Reference;
   end record;
   type Lecture_Panel is access all Lecture_Panel_Record'Class;
--
-- Build_Item_Menu -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Build_Item_Menu
             (  View  : not null access Lecture_Panel_Record;
                Index : Positive
             );
--
-- Check -- Overrides Gtk.Fuzzy_Object...
--
   procedure Check
             (  View       : not null access Lecture_Panel_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
--
-- Get -- Overrides Gtk.Fuzzy_Object...
--
   procedure Get
             (  View       : not null access Lecture_Panel_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             );
   function Get
            (  View       : not null access Lecture_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path;
--
-- Get_Current -- The currently selected training set
--
   procedure Get_Current
             (  View     : not null access Lecture_Panel_Record;
                Selected : Selection;
                Store    : out Storage_Handle;
                Lesson   : out Lecture_Handle
             );
--
-- Gtk_New -- Factory
--
   procedure Gtk_New
             (  View      : out Lecture_Panel;
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
                            Lecture_Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             );
--
-- Is_Lecture -- Check if a training set was selected
--
--    View     - The panel
--    Selected - A selection in
--
-- Returns :
--
--    True if a training set is selected (exactly one)
--
   function Is_Lecture
            (  View     : not null access Lecture_Panel_Record;
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
   procedure Root_Changed (View : not null access Lecture_Panel_Record);
--
-- Selection_Changed -- overrides Gtk.Fuzzy_Catalogue...
--
   procedure Selection_Changed
             (  View : not null access Lecture_Panel_Record
             );
--
-- Style_Updated -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Style_Updated
             (  View : not null access Lecture_Panel_Record
             );
--
-- Add -- Clicked callback
--
   procedure Add
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Classify -- Clicked callback
--
   procedure Classify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Compare -- Clicked callback
--
   procedure Compare
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
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
-- Edit -- Clicked callback
--
   procedure Edit
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Learn -- Clicked callback
--
   procedure Learn
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Get_From_Text -- Clicked callback
--
   procedure Get_From_Text
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Save_As_Text -- Clicked callback
--
   procedure Save_As_Text
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Show -- Clicked callback
--
   procedure Show
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Verify -- Clicked callback
--
   procedure Verify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );

end Gtk.Fuzzy_Catalogue.Lecture_Pane;
