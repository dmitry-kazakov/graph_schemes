--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Copy           Luebeck            --
--  Interface                                      Summer, 2009       --
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

with Gtk.GEntry;  use Gtk.GEntry;
with Gtk.Table;   use Gtk.Table;

private package Gtk.Fuzzy_Catalogue.Lecture_Copy is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Lecture_Copy);

   type Lecture_Copy_Box_Record;
   type Lecture_Copy_Box is access all Lecture_Copy_Box_Record'Class;
--
-- Lecture_Copy_Box_Record -- Box of feature creation
--
   type Lecture_Copy_Box_Record is new Gtk_Item_Box_Record with record
      Grid             : Gtk_Table;
      Folder_Name      : Folder_Selection;
      Name_Edit        : Gtk_Entry;
      Name_Hint        : Gtk_Box;
      Source_Name      : Gtk_Picker_Box;
      Source_Name_Hint : Gtk_Box;
      From_Edit        : Gtk_Entry;
      From_Hint        : Gtk_Box;
      To_Edit          : Gtk_Entry;
      To_Hint          : Gtk_Box;
      Total            : Gtk_Label;
         -- Copy data
      Store            : Storage_Handle;
      Folder           : Deposit_Handle;
      Source           : Lecture_Handle;
      Result           : Lecture_Handle;
      From             : Integer;
      To               : Integer;
   end record;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Store   - Storage, where both trainign set exist
--    Lesson  - To copy
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Lecture_Copy_Box;
                Store   : Storage_Handle;
                Lesson  : Lecture_Handle;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Cancel -- Button click event
--
   procedure Cancel
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Lecture_Copy_Box_Record);
--
-- Completed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed (Item : not null access Lecture_Copy_Box_Record);
--
-- Initialize -- To be called by derived types
--
--    Item   - To construct
--    Store  - Storage, where both trainign set exist
--    Lesson - To copy
--
   procedure Initialize
             (  Item   : not null access Lecture_Copy_Box_Record'Class;
                Store  : Storage_Handle;
                Lesson : Lecture_Handle
             );
--
-- From_Changed -- From field changed event
--
   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             );
--
-- Name_Changed -- Name changed event
--
   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service (Item : not null access Lecture_Copy_Box_Record);
--
-- Source_Name_Changed -- Name changed event
--
   procedure Source_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             );
--
-- To_Changed -- To field changed event
--
   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             );

   package Lecture_Copy_Boxes is
      new GLib.Object.Weak_References (Lecture_Copy_Box_Record);

   package Copy_Handles is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Lecture_Copy_Box
           );
end Gtk.Fuzzy_Catalogue.Lecture_Copy;
