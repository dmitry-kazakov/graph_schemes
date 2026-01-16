--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.File_Views              Luebeck            --
--  Interface                                      Winter, 2009       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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

with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Source_Buffer;    use Gtk.Source_Buffer;
with Gtk.Source_Language;  use Gtk.Source_Language;
with Gtk.Source_View;      use Gtk.Source_View;
with Gtk.Text_Tag;         use Gtk.Text_Tag;
with Units;                use Units;

private package Gtk.Fuzzy_Catalogue.File_Views is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.File_Views);

   Buffer_Size : constant := 1024 * 32;
--
-- File_View_Record -- Box of training
--
   type File_View_Record (<>) is new Gtk_Item_Box_Record with private;
   type File_View is access all File_View_Record'Class;
--
-- Finalize -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Finalize (Item : not null access File_View_Record);
--
-- Gtk_New -- Construction
--
--    Item      - The result
--    File_Name - The file name
--    Mode      - Encoding
--    New_File  - Flag if the file exists
--    Language  - Source file language
--    Browser   - The parent widget
--
   procedure Gtk_New
             (  Item      : out File_View;
                File_Name : UTF8_String;
                Mode      : Code_Set;
                New_File  : Boolean;
                Language  : Gtk_Source_Language;
                Browser   : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Gtk_New -- Construction
--
--    Item        - The result
--    File_Name   - The file name
--    Mode        - Encoding
--    New_File    - Flag if the file exists
--    Language    - Source file language
--    Line_No     - The line number to scroll to
--    Position_No - The position number within the line
--    Browser     - The parent widget
--
   procedure Gtk_New
             (  Item        : out File_View;
                File_Name   : UTF8_String;
                Mode        : Code_Set;
                New_File    : Boolean;
                Language    : Gtk_Source_Language;
                Line_No     : Positive;
                Position_No : Positive;
                Browser     : not null access
                              Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Gtk_New -- Construction
--
--    Item             - The result
--    File_Name        - The file name
--    Mode             - Encoding
--    New_File         - Flag if the file exists
--    Language         - Source file language
--    Line_From_No     - The line number to scroll to
--    Position_From_No - The position number within the line
--    Line_To_No       - The line number to mark as error end
--    Position_To_No   - The position number within the line
--    Browser          - The parent widget
--
   procedure Gtk_New
             (  Item             : out File_View;
                File_Name        : UTF8_String;
                Mode             : Code_Set;
                New_File         : Boolean;
                Language         : Gtk_Source_Language;
                Line_From_No     : Positive;
                Position_From_No : Positive;
                Line_To_No       : Natural;
                Position_To_No   : Natural;
                Browser          : not null access
                                   Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item      - To construct
--    File_Name - The file name
--    New_File  - Flag if the file exists
--    Language  - Source file language
--
   procedure Initialize
             (  Item      : not null access File_View_Record'Class;
                File_Name : UTF8_String;
                New_File  : Boolean;
                Language  : Gtk_Source_Language
             );
private
   type File_View_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class;
           Mode    : Code_Set
        )  is new Gtk_Item_Box_Record (Browser) with
   record
      View             : Gtk_Source_View;
      Buffer           : Gtk_Source_Buffer;
      Tool_Box         : Gtk_HBox;
      Save_Button_Box  : Gtk_HBox;
      Edit             : Edit_Buttons.Gtk_Style_Button;
      Redo             : Redo_Buttons.Gtk_Style_Button;
      Undo             : Undo_Buttons.Gtk_Style_Button;
      Save             : Save_Buttons.Gtk_Style_Button;
      Cursor           : Gtk_Label;
      Directory        : Gtk_Label;
      Name             : Gtk_GEntry;
      File_Name        : Unbounded_String;
      Error_Tag        : Gtk_Text_Tag;
      Left_Tag         : Gtk_Text_Tag;
      Middle_Tag       : Gtk_Text_Tag;
      Right_Tag        : Gtk_Text_Tag;
      Line_From_No     : Positive := 1;
      Position_From_No : Positive := 1;
      Line_To_No       : Positive := 1;
      Position_To_No   : Natural  := 1;
      Marked           : Boolean  := False;
      Input_Last       : Natural  := 0;
      New_File         : Boolean  := False;
      Input_Buffer     : UTF8_String (1..Buffer_Size);
   end record;
--
-- Check_Encoding-- Check buffer content against encoding mode
--
--    Item - The widget
--
-- Returns :
--
--    True if no encoding error
--
   function Check_Encoding
            (  Item : not null access File_View_Record
            )  return Boolean;
--
-- Cancel-- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   function Cancel
            (  Item : not null access File_View_Record
            )  return Boolean;
--
-- Completed-- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed (Item : not null access File_View_Record);
--
-- Changed -- Notification event
--
   procedure Changed
             (  Object : access GObject_Record'Class;
                Item   : File_View
             );
--
-- Edit -- clicked event
--
   procedure Edit
             (  Object : access GObject_Record'Class;
                Item   : File_View
             );
--
-- Mark_Set -- mark-set event
--
   procedure Mark_Set
             (  Object : access GObject_Record'Class;
                Params : GValues;
                Item   : File_View
             );
--
-- Redo -- clicked event
--
   procedure Redo
             (  Object : access GObject_Record'Class;
                Item   : File_View
             );
--
-- Save -- clicked event
--
   procedure Save
             (  Object : access GObject_Record'Class;
                Item   : File_View
             );
--
-- Write -- A file
--
--    Item - The widget
--    Name - File name to write
--
   procedure Write
             (  Item : not null access File_View_Record;
                Name : UTF8_String
             );
--
-- Undo -- clicked event
--
   procedure Undo
             (  Object : access GObject_Record'Class;
                Item   : File_View
             );

   procedure Show_Error (Item : not null access File_View_Record);
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service (Item : not null access File_View_Record);
--
-- Updated -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Updated (Item : not null access File_View_Record);

   package Buffer_Handlers is
      new Gtk.Handlers.User_Callback (GObject_Record, File_View);

end Gtk.Fuzzy_Catalogue.File_Views;
