--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Projects                Luebeck            --
--  Interface                                      Spring, 2008       --
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

with Gtk.Check_Button;  use Gtk.Check_Button;
with Gtk.Combo_Box;     use Gtk.Combo_Box;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.List_Store;    use Gtk.List_Store;
with Gtk.Table;         use Gtk.Table;

with Gtk.Fuzzy_Catalogue.File_Selection;
use  Gtk.Fuzzy_Catalogue.File_Selection;

private package Gtk.Fuzzy_Catalogue.Projects is
--
-- Create -- Button  clicked event for a button that activates a project
--           creation or credentials item.  The  item  is  created  when
--           necessary
--
   procedure Create
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
   type ODBC_Page is record
      Grid           : Gtk_Table;
      Delete         : Delete_Folder_Buttons.Gtk_Style_Button;
      Name_Entry     : Gtk_Combo_Box;
      Name_Hint      : Gtk_Box;
      User_Entry     : Gtk_Entry;
      User_Hint      : Gtk_Box;
      Password_Entry : Gtk_Entry;
      Password_Hint  : Gtk_Box;
      DSN_List       : Gtk_List_Store;
      Store_Password : Gtk_Check_Button;
   end record;

   type SQLite_Page is record
      Grid       : Gtk_Table;
      Delete     : Delete_Folder_Buttons.Gtk_Style_Button;
      Name_Entry : Gtk_Entry;
      Name_Hint  : Gtk_Box;
      File_Entry : Gtk_Entry;
      File_Hint  : Gtk_Box;
      Directory  : Gtk_File_Selection;
   end record;

   type FDB_Page is record
      Grid       : Gtk_Table;
      Delete     : Delete_Folder_Buttons.Gtk_Style_Button;
      Name_Entry : Gtk_Entry;
      Name_Hint  : Gtk_Box;
      File_Entry : Gtk_Entry;
      File_Hint  : Gtk_Box;
      Directory  : Gtk_File_Selection;
   end record;
--
-- Project_Box_Record -- Box of new project creation
--
   type Project_Box_Record is new Gtk_Item_Box_Record with record
      Pages  : Gtk_Notebook;
      ODBC   : ODBC_Page;
      SQLite : SQLite_Page;
      FDB    : FDB_Page;
   end record;
   type Project_Box is access all Project_Box_Record'Class;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Project_Box_Record);
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Project_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--
   procedure Initialize
             (  Item : not null access Project_Box_Record'Class
             );
--
-- Query -- Creates a new query
--
--    Browser - The parent widget
--
   function Query
            (  Browser : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class
            )  return Query_Handles.Handle;

private
--
-- Delete_DSN -- Button clicked event handler
--
   procedure Delete_DSN
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             );
--
-- Delete_SQLite -- Button clicked event handler
--
   procedure Delete_SQLite
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             );
--
-- Project_Query -- Credentials query
--
   type Project_Query
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Abstract_Credentials_Query with
   record
      Storage  : Storage_Handle;
      Name     : Unbounded_String;
      File     : Unbounded_String;
      User     : Unbounded_String;
      Password : Unbounded_String;
      Scheme   : Scheme_Type;
      Stored   : Boolean;
   end record;
   type Project_Query_Ptr is access all Project_Query;
--
-- Create -- Overrides Gtk.Persistent_Storage_Browser...
--
   overriding
   procedure Create
             (  Query           : in out Project_Query;
                Scheme          : out Scheme_Type;
                Name            : out Unbounded_String;
                User            : out Unbounded_String;
                Password        : out Unbounded_String;
                Stored_Password : out Boolean;
                Storage         : out Storage_Handle
             );
--
-- Get -- Overrides Gtk.Persistent_Storage_Browser...
--
   overriding
   procedure Get
             (  Query           : in out Project_Query;
                Scheme          : Scheme_Type;
                Name            : UTF8_String;
                User            : in out Unbounded_String;
                Password        : in out Unbounded_String;
                Stored_Password : in out Boolean;
                Storage         : out Storage_Handle
             );
--
-- FDB_File_Changed -- Changed event
--
   procedure FDB_File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             );
--
-- SQLite_File_Changed -- Changed event
--
   procedure SQLite_File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             );
--
-- Page_Changed -- Changed page
--
   procedure Page_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Params : GValues;
                Item   : Project_Box
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Project_Box
          );
end Gtk.Fuzzy_Catalogue.Projects;
