--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Gtk_Icon_Factory                      Luebeck            --
--  Interface                                      Summer, 2006       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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

with GLib;        use GLib;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Widget;  use Gtk.Widget;

with Fuzzy.Feature.Binary;
with Fuzzy.Feature.Binary.Mutually_Independent;
with Fuzzy.Feature.Classificatory;
with Fuzzy.Feature.Discrete;
with Fuzzy.Feature.Float_Handle;
with Fuzzy.Feature.Integer_Handle;
with Fuzzy.Feature.Isosceles_Trapezoids_Handle;
with Fuzzy.Feature.Linguistic_Handle;
with Fuzzy.Feature.Output_Handle;
with Fuzzy.Graph.Scheme;
with Fuzzy.Classifier.Separator;
with Fuzzy.Lecture.Empty;
with Fuzzy.Lecture.Memory_Resident;
with Fuzzy.Lecture.Subrange;
with Persistent.Native_ODBC.Lectures;
with Persistent.SQLite.Lectures;

package Fuzzy.Gtk_Icon_Factory is
   pragma Elaborate_Body (Fuzzy.Gtk_Icon_Factory);
--
-- The icons provided by the factory
--
   Cancel_Icon         : constant String := "fuzzy-cancel-id";
   Bookmark_Add_Icon   : constant String := "fuzzy-bookmark-add-id";
   Bookmark_Delete_Icon: constant String := "fuzzy-bookmark-delete-id";
   Classify_Icon       : constant String := "fuzzy-classify-id";
   Classify_Input_Icon : constant String := "fuzzy-classify-input_id";
   Classifier_Icon     : constant String := "fuzzy-classifier-id";
   Classifier_View_Icon: constant String := "fuzzy-classifier-view-id";
   Collapse_Icon       : constant String := "fuzzy-collapse-id";
   Compare_Icon        : constant String := "fuzzy-compare-id";
   Confirm_Icon        : constant String := "fuzzy-confirm-id";
   Delete_Icon         : constant String := "fuzzy-delete-id";
   Down_Icon           : constant String := "fuzzy-down-id";
   Edit_Icon           : constant String := "fuzzy-edit-id";
   Explain_Icon        : constant String := "fuzzy-explain-id";
   Feature_Icon        : constant String := "fuzzy-feature-id";
   Feature_Copy_Icon   : constant String := "fuzzy-feature-copy-id";
   Feature_Delete_Icon : constant String := "fuzzy-feature-delete-id";
   Feature_View_Icon   : constant String := "fuzzy-feature-view-id";
   Find_Icon           : constant String := "fuzzy-find-id";
   Graph_Branch_Icon   : constant String := "fuzzy-graph-branch-id";
   Graph_Leaf_Icon     : constant String := "fuzzy-graph-leaf-id";
   Graph_Node_Icon     : constant String := "fuzzy-graph-node-id";
   Has_In_Icon         : constant String := "fuzzy-has-in-id";
   Has_Not_Icon        : constant String := "fuzzy-has-not-id";
   Has_Not_Out_Icon    : constant String := "fuzzy-has-out-id";
   Has_Out_Icon        : constant String := "fuzzy-has-not-out-id";
   Insert_Icon         : constant String := "fuzzy-insert-id";
   Learn_Icon          : constant String := "fuzzy-learn-id";
   Lecture_Icon        : constant String := "fuzzy-lecture-id";
   Lecture_Copy_Icon   : constant String := "fuzzy-lecture-copy-id";
   Lecture_Edit_Icon   : constant String := "fuzzy-lecture-edit-id";
   Lecture_Edited_Icon : constant String := "fuzzy-lecture-edited-id";
   Lecture_View_Icon   : constant String := "fuzzy-lecture-view-id";
   Negative_Icon       : constant String := "fuzzy-negative-example-id";
   New_Classifier_Icon : constant String := "fuzzy-new-classifier-id";
   New_Feature_Icon    : constant String := "fuzzy-new-feature-id";
   New_Folder_Icon     : constant String := "fuzzy-new-folder-id";
   New_From_FCL_Icon   : constant String := "fuzzy-new-from-fcl-id";
   New_Lecture_Icon    : constant String := "fuzzy-new-lecture-id";
   Number_Icon         : constant String := "fuzzy-number-id";
   Parent_Icon         : constant String := "fuzzy-parent-id";
   Positive_Icon       : constant String := "fuzzy-positive-example-id";
   Preview_Icon        : constant String := "fuzzy-preview-id";
   Rename_Icon         : constant String := "fuzzy-rename-id";
   Rename_Item_Icon    : constant String := "fuzzy-rename-item-id";
   Save_As_FCL_Icon    : constant String := "fuzzy-save_as_fcl-id";
   Save_As_Text_Icon   : constant String := "fuzzy-save_as_text-id";
   Swap_Folders_Icon   : constant String := "fuzzy-swap-folders-id";
   Verify_Icon         : constant String := "fuzzy-verify-id";
   Windowize_Icon      : constant String := "fuzzy-windowize-id";
   Up_Icon             : constant String := "fuzzy-up-id";

   Compare_Lectures_Icon : constant String :=
      "fuzzy-compare-lectures-id";
   New_Lecture_From_Text_Icon : constant String :=
      "fuzzy-lecture-from_text-id";

   Classificatory_Feature_Icon : constant String :=
      Fuzzy.Feature.Classificatory.Classificatory_Class;
   Dependent_Binary_Feature_Icon : constant String :=
      Fuzzy.Feature.Binary.Binary_Class;
   Float_Feature_Icon : constant String :=
      Fuzzy.Feature.Float_Handle.Float_Class;
   Independent_Binary_Feature_Icon : constant String :=
      Fuzzy.Feature.Binary.Mutually_Independent.
      Independent_Binary_Class;
   Integer_Feature_Icon : constant String :=
      Fuzzy.Feature.Integer_Handle.Integer_Class;
   Isosceles_Feature_Icon : constant String :=
      Fuzzy.Feature.Isosceles_Trapezoids_Handle.
      Isosceles_Trapezoid_Class;
   Lecture_Add_Example_Icon : constant String :=
      "fuzzy-lecture-add-example-id";
   Lecture_Add_Random_Example_Icon : constant String :=
      "fuzzy-lecture-add-random-example-id";
   Lecture_Copy_Example_Icon : constant String :=
      "fuzzy-lecture-copy-example-id";
   Lecture_Delete_Example_Icon : constant String :=
      "fuzzy-lecture-delete-example-id";
   Linear_Feature_Icon : constant String :=
      Fuzzy.Feature.Linguistic_Handle.Linguistic_Class;
   Nominal_Feature_Icon : constant String :=
      Fuzzy.Feature.Discrete.Discrete_Class;
   Output_Feature_Icon : constant String :=
      Fuzzy.Feature.Output_Handle.Output_Class;

   Empty_Lecture_Icon : constant String := Fuzzy.Lecture.Empty.Class;
   General_Lecture_Icon : constant String :=
      Fuzzy.Lecture.Memory_Resident.Class;
   ODBC_Lecture_Icon : constant String :=
      Persistent.Native_ODBC.Lectures.Class;
   SQLite_Lecture_Icon : constant String :=
      Persistent.SQLite.Lectures.Class;
   Subrange_Lecture_Icon : constant String :=
      Fuzzy.Lecture.Subrange.Class;

   Graph_Scheme_Classifier_Icon : constant String :=
      Fuzzy.Graph.Scheme.Class;
   Separator_Classifier_Icon : constant String :=
      Fuzzy.Classifier.Separator.Class;
--
-- Icon_Size_Panel -- Size of huge  224x224 icons
--
   function Icon_Size_Panel return Gtk_Icon_Size;
--
-- Escape_Name -- Double each underscore in the string
--
--    Name - To add escapes
--
-- Returns :
--
--    Name that can be used as a label
--
   function Escape_Name (Name : UTF8_String) return UTF8_String;
--
-- Get_Feature_Icon -- The icon stock name that fits to the feature
--
--    Class -- Of the feature
--
-- Returns :
--
--    The icon name
--
   function Get_Feature_Icon (Class : UTF8_String) return UTF8_String;
--
-- Init -- Factory initialization
--
-- The initialization has to be called before the first use of any  icon
-- from the factory. It can be called several times. Note that it can be
-- done  during  package elaboration, because GTK+ might be not ready at
-- that point.
--
   procedure Init;
--
-- Set_Button_Style -- Set button style
--
--    Class - Of buttons
--
   procedure Set_Button_Style (Class : UTF8_String);

   Fuzzy_ML_Domain : constant String := "FuzzyML";

end Fuzzy.Gtk_Icon_Factory;
