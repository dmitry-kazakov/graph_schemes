--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Lecture_Save_As_Text                     Winter, 2008       --
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

with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Fuzzy_Features_List;  use Gtk.Fuzzy_Features_List;
with Gtk.GEntry;               use Gtk.GEntry;

with Fuzzy.Feature.Handle.Unbounded_Arrays;
with Gtk.Fuzzy_Catalogue.Delimiter_Frame;
with Gtk.Fuzzy_Catalogue.File_Selection;

private package Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text);
   use Gtk.Fuzzy_Catalogue.Delimiter_Frame;
--
-- Lecture_Save_As_Text_Box_Record -- Box of feature creation
--
   type Lecture_Save_As_Text_Box_Record is
      new Gtk_Item_Box_Record with private;
   type Lecture_Save_As_Text_Box is
      access all Lecture_Save_As_Text_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Lecture_Save_As_Text_Box;
                Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--
   procedure Initialize
             (  Item : access Lecture_Save_As_Text_Box_Record'Class
             );

private
   use Fuzzy.Feature.Handle.Unbounded_Arrays;
   use Gtk.Fuzzy_Catalogue.File_Selection;
--
-- Lecture_Save_As_Text_Box_Record -- Box of feature creation
--
   type Lecture_Save_As_Text_Box_Record is
      new Gtk_Item_Box_Record with
   record
         -- Configuration data
      Features          : Unbounded_Array;
      Lecture_Name      : Gtk_Picker_Box;
      Lecture_Name_Hint : Gtk_Box;
      Lesson            : Lecture_Handle;
      From_Edit         : Gtk_Entry;
      From_Hint         : Gtk_Box;
      To_Edit           : Gtk_Entry;
      To_Hint           : Gtk_Box;
      Total             : Gtk_Label;
      Negative_Edit     : Gtk_Entry;
      Negative_Use      : Gtk_Check_Button;
      Positive_Edit     : Gtk_Entry;
      Positive_Use      : Gtk_Check_Button;
      File_Name_Hint    : Gtk_Box;
      File_Name_Edit    : Gtk_Entry;
      List              : Gtk_Fuzzy_Features_List;
      Directory         : Gtk_File_Selection;
      Delimiter_Frame   : Gtk_Fuzzy_Catalogue_Deilimiter_Frame;
         -- Data
      Delimiter         : Unbounded_String;
      File_Name         : Unbounded_String;
      Positive_Example  : Unbounded_String;
      Negative_Example  : Unbounded_String;
      From              : Integer;
      To                : Integer;
      Write_Positive    : Boolean;
      Write_Negative    : Boolean;
   end record;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Commit
             (  Item : not null access Lecture_Save_As_Text_Box_Record
             );
--
-- File_Changed -- Changed event
--
   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             );
--
-- From_Changed -- Changed event
--
   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   procedure Service
             (  Item : not null access Lecture_Save_As_Text_Box_Record
             );
--
-- Toggled -- Toggled event
--
   procedure Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             );
--
-- Lecture_Name_Changed -- Name changed event
--
   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             );
--
-- To_Changed -- Changed event
--
   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             );

   package Save_As_Text_Handles is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Lecture_Save_As_Text_Box
           );

   package Writer_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Lecture_Save_As_Text_Box
           );

end Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text;
