--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Lecture_Create_From_Text                 Winter, 2009       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Fuzzy.Lecture.Text_IO;     use Fuzzy.Lecture.Text_IO;
with Fuzzy.Logic;               use Fuzzy.Logic;
with Gtk.Check_Button;          use Gtk.Check_Button;
with Gtk.Fuzzy_Encoding_Combo;  use Gtk.Fuzzy_Encoding_Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Table;                 use Gtk.Table;
with Units;                     use Units;

with Fuzzy.Feature.Handle.Unbounded_Arrays;
with Gtk.Fuzzy_Catalogue.Delimiter_Frame;
with Gtk.Fuzzy_Catalogue.Features_Sequence;
with Gtk.Fuzzy_Catalogue.File_Selection;

private package Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text);
   use Gtk.Fuzzy_Catalogue.Delimiter_Frame;
--
-- Lecture_Create_From_Text_Box_Record -- Box of lecture creation
--
   type Lecture_Create_From_Text_Box_Record is
      new Gtk_Item_Box_Record with private;
   type Lecture_Create_From_Text_Box is
      access all Lecture_Create_From_Text_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Lecture_Create_From_Text_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--
   procedure Initialize
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record'Class
             );
private
   use Fuzzy.Feature.Handle.Unbounded_Arrays;
   use Gtk.Fuzzy_Catalogue.File_Selection;
   use Gtk.Fuzzy_Catalogue.Features_Sequence;
--
-- Lecture_Create_From_Text_Box_Record -- Box of feature creation
--
   type Lecture_Create_From_Text_Box_Record is
      new Gtk_Item_Box_Record with
   record
         -- Widget components
      Grid                : Gtk_Table;
      Folder_Name         : Folder_Selection;
      Name_Edit           : Gtk_Entry;
      Name_Hint           : Gtk_Box;
      File_Name_Label     : Gtk_Label;
      File_Name_Hint      : Gtk_Box;
      File_Name_Edit      : Gtk_Entry;
      Encoding_Box        : Gtk_Fuzzy_Encoding_Combo;
      Directory           : Gtk_File_Selection;
      Sequence            : Gtk_Fuzzy_Features_Sequence;
      Allow_Empty_Button  : Gtk_Check_Button;
      Necessity_Button    : Gtk_Check_Button;
      Get_Units_Button    : Gtk_Check_Button;
      Quote_Units_Button  : Gtk_Check_Button;
      Comment_Edit        : Gtk_Entry;
      Comment_Hint        : Gtk_Box;
      Positive_Edit       : Gtk_Entry;
      Positive_Hint       : Gtk_Box;
      Positive_By_Default : Gtk_Radio_Button;
      Negative_Edit       : Gtk_Entry;
      Negative_Hint       : Gtk_Box;
      Negative_By_Default : Gtk_Radio_Button;
      Full_Edit           : Gtk_Entry;
      Full_Hint           : Gtk_Box;
      Full_By_Default     : Gtk_Radio_Button;
      Undefined_Edit      : Gtk_Entry;
      Undefined_Hint      : Gtk_Box;
      Pane                : Gtk_Paned;
      Delimiter_Frame     : Gtk_Fuzzy_Catalogue_Deilimiter_Frame;
         -- Configuration data
      Features            : Unbounded_Array;
      Delimiter           : Unbounded_String;
      File_Name           : Unbounded_String;
      Lesson              : Lecture_Handle;
      Keywords            : File_Tokens.Table;
      Encoding            : Code_Set;
      Default_Example     : Example_Type;
      Default             : Fuzzy_Boolean;
      Line_No             : Positive;
      Position_No         : Positive;
      Allow_Empty         : Boolean;
      Quote_Units         : Boolean;
      Get_Units           : Boolean;
   end record;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             );
--
-- Aborted -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Aborted
             (  Item  : not null access
                        Lecture_Create_From_Text_Box_Record;
                Fault : Exception_Occurrence
             );
--
-- Completed-- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             );
--
-- File_Changed -- Changed event
--
   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
             );
--
-- Name_Changed -- Name changed event
--
   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             );
--
-- Show_Location -- Clicked event
--
   procedure Show_Location
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
             );

   package Create_Handles is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Lecture_Create_From_Text_Box
           );

   package Reader_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Lecture_Create_From_Text_Box
           );

end Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text;
