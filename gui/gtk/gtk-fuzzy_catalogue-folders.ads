--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Folders                 Luebeck            --
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

with Gtk.Fuzzy_Encoding_Combo;  use Gtk.Fuzzy_Encoding_Combo;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Table;                 use Gtk.Table;
with Indicator;                 use Indicator;
with Parsers.FCL.Compiler;      use Parsers.FCL.Compiler;
with Units;                     use Units;

with Ada.Text_IO;
with Fuzzy.Feature.Handle.Container;
with Gtk.Fuzzy_Catalogue.File_Selection;
with Parsers.Multiline_Source.Text_IO;
with Parsers.Multiline_Source.Latin1_Text_IO;

private package Gtk.Fuzzy_Catalogue.Folders is
--
-- Create -- Button clicked event for a button that activates  a  folder
--           creation item. The item is created when necessary
--
   procedure Create
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- From_FCL -- Button clicked event for a button that activates a folder
--             creation item that reads a FCL file.
--
   procedure From_FCL
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Delete -- Button  clicked event for a button that removes folders and
--           projects
--
   procedure Delete
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             );
--
-- Folder_Create_Box_Record -- Box of folder creation
--
   type Folder_Create_Box_Record is
      new Gtk_Item_Box_Record with private;
   type Folder_Create_Box is access all Folder_Create_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Folder_Create_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--
   procedure Initialize
             (  Item : not null access Folder_Create_Box_Record'Class
             );
--
-- Folder_From_FCL_Box_Record -- Box of folder creation
--
   type Folder_From_FCL_Box_Record is
      new Gtk_Item_Box_Record with private;
   type Folder_From_FCL_Box is
      access all Folder_From_FCL_Box_Record'Class;
--
-- Gtk_New -- Construction
--
--    Item    - The result
--    Browser - The parent widget
--
   procedure Gtk_New
             (  Item    : out Folder_From_FCL_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Initialize -- To be called by derived types
--
--    Item - To construct
--
   procedure Initialize
             (  Item : not null access Folder_From_FCL_Box_Record'Class
             );
--
-- Finalize -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Finalize
             (  Item : not null access Folder_From_FCL_Box_Record
             );
private
   use Gtk.Fuzzy_Catalogue.File_Selection;

   type Folder_Create_Box_Record is new Gtk_Item_Box_Record with record
      Name_Entry : Gtk_Entry;
      Name_Hint  : Gtk_Box;
   end record;
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit (Item : not null access Folder_Create_Box_Record);
--
-- Latin1_Source_File -- Multiline source file with an indicator
--
   type Latin1_Source_File
        (  File   : not null access Ada.Text_IO.File_Type;
           Viewer : not null access Indicator_Object'Class
        )  is new Parsers.Multiline_Source.Latin1_Text_IO.Source (File)
              with null record;
--
-- Get_Line -- Overrides Parsers.Multiline_Source...
--
   overriding
   procedure Get_Line (Code : in out Latin1_Source_File);
--
-- UTF8_Source_File -- Multiline source file with an indicator
--
   type UTF8_Source_File
        (  File   : not null access Ada.Text_IO.File_Type;
           Viewer : not null access Indicator_Object'Class
        )  is new Parsers.Multiline_Source.Text_IO.Source (File)
              with null record;
--
-- Get_Line -- Overrides Parsers.Multiline_Source...
--
   overriding
   procedure Get_Line (Code : in out UTF8_Source_File);

   type Folder_Program
        (  Item : not null access Folder_From_FCL_Box_Record
        )  is new Program with
   record
      Features : Fuzzy.Feature.Handle.Container.Set;
   end record;
   type Folder_Program_Ptr is access Folder_Program;
--
-- Created_Feature -- Overrides Parsers.FCL.Compiler...
--
   overriding
   procedure Created_Feature
             (  Compiler : in out Folder_Program;
                Feature  : in out Feature_Handle
             );
--
-- Create_Rules -- Overrides Parsers.FCL.Compiler...
--
   overriding
   function Create_Rules
            (  Compiler : Folder_Program;
               Name     : String
            )  return Lecture_Handle;

   type Folder_From_FCL_Box_Record is
      new Gtk_Item_Box_Record with
   record
      Grid             : Gtk_Table;
      Name_Entry       : Gtk_Entry;
      Name_Hint        : Gtk_Box;
      Directory_Frame  : Gtk_Frame;
      File_Name_Label  : Gtk_Label;
      File_Name_Hint   : Gtk_Box;
      File_Name_Edit   : Gtk_Entry;
      Directory        : Gtk_File_Selection;
      Encoding_Box     : Gtk_Fuzzy_Encoding_Combo;
      View_Button      : View_File_Buttons.Gtk_Style_Button;
      New_Button       : New_Buttons.Gtk_Style_Button;
         -- Data
      File_Name        : Unbounded_String;
      Storage          : Storage_Handle;
      Parent           : Deposit_Handle;
      Line_From_No     : Positive;
      Line_To_No       : Positive;
      Position_From_No : Positive;
      Position_To_No   : Natural;
      Encoding         : Code_Set;
      Code             : Folder_Program_Ptr;
   end record;
--
-- Aborted -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Aborted
             (  Item  : not null access Folder_From_FCL_Box_Record;
                Fault : Exception_Occurrence
             );
--
-- Commit -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Commit
             (  Item : not null access Folder_From_FCL_Box_Record
             );
--
-- Completed -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Completed
             (  Item : not null access Folder_From_FCL_Box_Record
             );
--
-- Create_Indicator -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   function Create_Indicator
            (  Item : not null access Folder_From_FCL_Box_Record
            )  return Gtk_Indicator;
--
-- File_Changed -- Changed event
--
   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             );
--
-- New_File -- Clicked event
--
   procedure New_File
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             );
--
-- Service -- Overrides Gtk.Fuzzy_Catalogue...
--
   overriding
   procedure Service
             (  Item : not null access Folder_From_FCL_Box_Record
             );
--
-- Show_Location -- Clicked event
--
   procedure Show_Location
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             );
--
-- View_File -- Clicked event
--
   procedure View_File
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             );

   package FCL_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Folder_From_FCL_Box
           );

end Gtk.Fuzzy_Catalogue.Folders;
