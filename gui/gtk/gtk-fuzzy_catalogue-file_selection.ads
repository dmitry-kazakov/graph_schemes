--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.File_Selection          Luebeck            --
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

with Gtk.Combo_Box;   use Gtk.Combo_Box;
with Gtk.GEntry;      use Gtk.GEntry;
with Gtk.List_Store;  use Gtk.List_Store;

with Gtk.Wildcard_Directory_Browser;

package Gtk.Fuzzy_Catalogue.File_Selection is
   use Gtk.Wildcard_Directory_Browser;
--
-- Gtk_File_Selection_Record -- A directory browser widget with wildcard
--                              files  filter.  The  filter  is a string
-- pattern  that  can  contain the * wildcard symbol to match file names
-- which  may  appear  in  the  files  view  pane.  Wildcard matches any
-- sequence of UTF-8 encoded characters. An empty pattern string patches
-- anything and is equivalent to "*".
--
   type Gtk_File_Selection_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_Widget_Record with private;
   type Gtk_File_Selection is
      access all Gtk_File_Selection_Record'Class;
--
-- Get_Filter_Entry -- Get current directory
--
--    Widget - The widget
--
-- Returns :
--
--    The filter entry
--
   function Get_Filter_Entry
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_Entry;
--
-- Get_Directory_Browser -- Get browser
--
--    Widget - The widget
--
-- Returns :
--
--    The browser widget
--
   function Get_Directory_Browser
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_Wildcard_Directory_Browser;
--
-- Get_Directory -- Get current directory
--
--    Widget - The widget
--
-- Returns :
--
--    The current directory path
--
  function Get_Directory
           (  Widget : not null access Gtk_File_Selection_Record
           )  return UTF8_String;
--
-- Get_Selection -- Get current directory
--
--    Widget - The widget
--
-- Returns :
--
--    The file name selected or empty string
--
   function Get_Selection
            (  Widget : not null access Gtk_File_Selection_Record
            )  return UTF8_String;
--
-- Get_Title_Box -- Get title box of buttons
--
--    Widget - The widget
--
-- Returns :
--
--    The button box
--
   function Get_Title_Box
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_HBox;
--
-- Gtk_New -- Factory
--
--    Widget  - The result
--    Browser - The parent catalogue widget
--    Key     - The key of the recent resource management
--
   procedure Gtk_New
             (  Widget  : out Gtk_File_Selection;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Key     : UTF8_String
             );
--
-- Initialize -- To be called by derived type
--
--    Widget  - The widget to initialize
--    Key     - The key of the recent resource management
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_File_Selection_Record'Class;
                Key    : UTF8_String
             );

private
   type Gtk_File_Selection_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_VBox_Record with
   record
      Directories : Gtk_Wildcard_Directory_Browser;
      Title_Box   : Gtk_HBox;
      Filter      : Gtk_Entry;
      Add         : Bookmark_Add_Buttons.Gtk_Style_Button;
      Delete      : Bookmark_Delete_Buttons.Gtk_Style_Button;
      Backward    : Backward_Buttons.Gtk_Style_Button;
      Bookmarks   : Gtk_Combo_Box;
      Forward     : Forward_Buttons.Gtk_Style_Button;
      Switch_To   : Bookmark_GoTo_Buttons.Gtk_Style_Button;
      Home        : Home_Buttons.Gtk_Style_Button;
      Parent      : Parent_Buttons.Gtk_Style_Button;
      Refresh     : Refresh_Buttons.Gtk_Style_Button;
      List        : Gtk_List_Store;
      Changing    : Boolean := False; -- Directory changed event pending
   end record;
--
-- Add_Bookmark -- Button click
--
   procedure Add_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Filter_Changed -- Change event
--
   procedure Filter_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Backward -- Button click
--
   procedure Backward
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Bookmark_Changed -- Combo changed handler
--
   procedure Bookmark_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Delete_Bookmark -- Button click
--
   procedure Delete_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Directory_Changed -- Directory changed handler
--
   procedure Directory_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Forward -- Button click
--
   procedure Forward
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Go_To_Bookmark -- Button click
--
   procedure Go_To_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Go_To_Parent -- Button click
--
   procedure Go_To_Parent
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Home -- Button click
--
   procedure Home
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Refreshed -- Button click
--
   procedure Refreshed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             );
--
-- Set_Button_States -- Make buttons sensitive / unsensitive
--
--    Widget - The widget
--
   procedure Set_Button_States
             (  Widget : not null access Gtk_File_Selection_Record
             );

   package File_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_File_Selection
          );

end Gtk.Fuzzy_Catalogue.File_Selection;
