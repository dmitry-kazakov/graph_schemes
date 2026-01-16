--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Edit           Luebeck            --
--  Interface                                      Winter, 2009       --
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

with Gtk.Fuzzy_Lecture_Editor;  use Gtk.Fuzzy_Lecture_Editor;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers.References;   use Gtk.Handlers.References;
with Gtk.Indicators.Progress;   use Gtk.Indicators.Progress;
with Gtk.Table;                 use Gtk.Table;

with Generic_Map;

private package Gtk.Fuzzy_Catalogue.Lecture_Edit is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Lecture_Edit);
--
-- Lecture_Edit_Box_Record -- Box of training set editing
--
   type Lecture_Edit_Box_Record is new Gtk_Item_Box_Record with private;
   type Lecture_Edit_Box is access all Lecture_Edit_Box_Record'Class;
--
-- Cancel -- Overrides Gtk.Fuzzy_Catalogue...
--
   function Cancel
            (  Item : not null access Lecture_Edit_Box_Record
            )  return Boolean;
--
-- Gtk_New -- Construction
--
--    Item     - The result
--    Name     - The name of the set
--  [ Store    - A handle to
--    Lesson ] - A handle to
--    Browser  - The parent widget
--
   procedure Gtk_New
             (   Item    : out Lecture_Edit_Box;
                 Name    : String;
                 Store   : Storage_Handle;
                 Lesson  : Lecture_Handle;
                 Browser : not null access
                           Gtk_Fuzzy_Catalogue_Record'Class
             );
   procedure Gtk_New
             (   Item    : out Lecture_Edit_Box;
                 Name    : String;
                 Browser : not null access
                           Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Finalize -- Destruction
--
   procedure Finalize (Item : not null access Lecture_Edit_Box_Record);
--
-- Initialize -- To be called by derived types
--
--    Item   - To construct
--    Name   - The name of the set
--    Store  - A handle to
--    Lesson - A handle to
--
   procedure Initialize
             (  Item   : not null access Lecture_Edit_Box_Record'Class;
                Name   : String;
                Store  : Storage_Handle;
                Lesson : Lecture_Handle
             );
private
   package Add_Feature_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "AddLectureFeature",
             Icon       => New_Feature_Icon,
             Relief     => Relief_None,
             Tip    => "Add the selected feature to the training set"
          );
   use Add_Feature_Buttons;

   package Delete_Feature_Buttons is
      new Gtk.Generic_Style_Button
          (  Class_Name => Class_Name & "DeleteLectureFeature",
             Icon       => Feature_Delete_Icon,
             Relief     => Relief_None,
             Tip => "Delete the selected feature from the training set"
          );
   use Delete_Feature_Buttons;

   package Feature_Constraints is
      new Generic_Map
          (  Object_Type => Picker_Constraint,
             Key_Type    => Feature_Handle
          );
   use Feature_Constraints;
--
-- Lecture_Edit_Box_Record -- Box of training set editing
--
   type Lecture_Edit_Box_Record is new Gtk_Item_Box_Record with record
      Exists         : Boolean := False; -- Has save button visible
         -- Editor
      Editor         : Gtk_Fuzzy_Lecture_Editor;
      Add_Feature    : Add_Feature_Buttons.Gtk_Style_Button;
      Delete_Feature : Delete_Feature_Buttons.Gtk_Style_Button;
      Stop           : Abort_Buttons.Gtk_Style_Button;
      Save           : Save_Buttons.Gtk_Style_Button;
      Save_As        : Save_As_Buttons.Gtk_Style_Button;
      Progress       : Gtk_Progress;
         -- Save grid
      Result         : Lecture_Handle;
      Save_Grid      : Gtk_Table;
      Folder_Name    : Folder_Selection;
      Folder_Hint    : Gtk_Box;
      Name_Edit      : Gtk_Entry;
      Name_Hint      : Gtk_Box;
         -- Naming
      Overwrite      : Boolean := False;
      Store          : Storage_Handle;
      Folder         : Deposit_Handle;
      Name           : Unbounded_String;
         -- Constaints
      Features_List  : Feature_Constraints.Map;
      Selector_1     : Handler_Reference;
      Selector_2     : Handler_Reference;
   end record;
--
-- Aborted -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Aborted
             (  Item  : not null access Lecture_Edit_Box_Record;
                Fault : Exception_Occurrence
             );
--
-- Add_Feature -- Clicked callback
--
   procedure Add_Feature
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Close -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Close (Item : not null access Lecture_Edit_Box_Record);
--
-- Columns_Selection_Changed -- Handler of features-selection-changed
--
   procedure Columns_Selection_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Commit (Item : not null access Lecture_Edit_Box_Record);
--
-- Completed -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Completed (Item : not null access Lecture_Edit_Box_Record);
--
-- Delete_Feature -- Clicked callback
--
   procedure Delete_Feature
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Features_Selection_Changed -- Handler of selection-changed
--
   procedure Features_Selection_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Modified_Changed -- Handler of changed
--
   procedure Modified_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Name_Changed -- Handler of changed
--
   procedure Name_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Save -- Handler of clicked
--
   procedure Save
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Save_As -- Handler of clicked
--
   procedure Save_As
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Service (Item : not null access Lecture_Edit_Box_Record);

   package Edit_Handlers is
      new Gtk.Handlers.User_Callback
          (  GObject_Record,
             Lecture_Edit_Box
          );

end Gtk.Fuzzy_Catalogue.Lecture_Edit;
