--                                                                    --
--  separate body                   Copyright (c)  Dmitry A. Kazakov  --
--     Install_Class_Properties                    Luebeck            --
--                                                 Winter, 2008       --
--                                                                    --
--                                Last revision :  10:08 22 Nov 2014  --
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

with GLib.Types;  use GLib.Types;

separate (Gtk.Fuzzy_Catalogue) procedure Install_Class_Properties is
begin
   Install_Hints_Style_Properties (Class_Record);
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_UInt
      (  Name    => "button-spacing",
         Nick    => "Button spacing",
         Blurb   => "Spacing in the button boxes",
         Minimum => 0,
         Maximum => GUInt'Last,
         Default => 3
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_UInt
      (  Name    => "column-spacing",
         Nick    => "Column spacing",
         Blurb   => "Column spacing in the grid of elements",
         Minimum => 0,
         Maximum => GUInt'Last,
         Default => 3
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "copy-file-error-begin",
         Nick    => "Copy error prefix",
         Default => "Unable to copy '",
         Blurb   =>
            "The first part of the message on copy file error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "copy-file-error-middle",
         Nick    => "Copy error middle",
         Default => "' to '",
         Blurb   =>
            "The second part of the message on copy file error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "copy-file-error-end",
         Nick    => "Copy error end",
         Default => "': ",
         Blurb   =>
            "The third part of the message on copy file error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "creation-error",
         Nick    => "Onject create error",
         Default => "Unable to create a new object: ",
         Blurb   => "The message on failed object creation"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "delete-name-query",
         Nick    => "Deleted objects' names",
         Blurb   => "The message of object deletion",
         Default => "Are you sure you want to delete object(s) "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "delete-query-title",
         Nick    => "Confirm objects to delete",
         Default => "Confirm object(s) to delete",
         Blurb   => "The title of objects deletion dialog"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "duplicated-error-begin",
         Nick    => "Duplicated object prefix",
         Blurb   => "The message prefix on duplicated name",
         Default => "An object with the name '"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "duplicated-error-end",
         Nick    => "Duplicated object suffix",
         Blurb   => "The message suffix on duplicated name",
         Default => "' already exists."
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "files-filter",
         Nick    => "Filter",
         Blurb   => "The files filter label",
         Default => "Filter"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "encoding-error",
         Nick    => "Encoding error",
         Blurb   => "The message on encoding error",
         Default =>
            (  "File encoding does not support some glyphs "
            &  "used in the editor. Unsuported glyphs are "
            &  "marked in the source"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_Boxed
      (  Name       => "error-code-foregound-color",
         Boxed_Type => Gdk_Color_Type,
         Nick       => "Error code color",
         Blurb      => "Illegal encoding code color"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      GLib.Properties.Icon_Size.Property.Gnew_Enum
      (  Name    => "error-icon-size",
         Nick    => "Size",
         Blurb   => "Error icon size",
         Default =>
            Gtk_Icon_Size_Enum'Val (Icon_Size_Dialog)
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "error-icon",
         Nick    => "Error icon",
         Blurb   => "The stock ID of error icon",
         Default => Stock_Dialog_Error
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_Boxed
      (  Name       => "error-text-color",
         Boxed_Type => Gdk_Color_Type,
         Nick       => "Error color",
         Blurb      => "The error text color"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "error-title",
         Nick    => "Error dialog title",
         Blurb   => "The title of the error dialog",
         Default => "Error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "empty-name-error",
         Nick    => "Empty name",
         Blurb   => "The message when name is empty",
         Default => "An object name may not be empty"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "empty-file-name-error",
         Nick    => "Empty file name",
         Blurb   => "The message when file name is empty",
         Default => "No file specified"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "features-sequence-title",
         Nick    => "Read features sequence",
         Blurb   => "The title of the features sequence",
         Default => "Read features sequence"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "file-error-begin",
         Nick    => "Write file prefix",
         Default => "Error writing file '",
         Blurb   =>
            "The first part of the message on file write error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "file-error-end",
         Nick    => "Write file suffix",
         Default => "': ",
         Blurb   =>
            "The second part of the message on file write error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "inconsistent-storage-error",
         Nick    => "Inconsistent storage",
         Blurb   => "The message of inconsistent storage",
         Default => "Inconsistent storage: "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "input-file-label",
         Nick    => "Input file",
         Blurb   => "The input file name label",
         Default => "Input file name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "invalid-file-name-error",
         Nick    => "Invalid file name",
         Blurb   => "The message when file name is illegal",
         Default => "Invalid file name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_UInt
      (  Name    => "lecture-cache",
         Nick    => "Lecture cache",
         Minimum => 0,
         Maximum => GUInt'Last,
         Default => 1024,
         Blurb   =>
            (  "The number of training set examples "
            &  "cached [er feature when the training set "
            &  "is visualized or trained on."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "lines-read",
         Nick    => "Lines",
         Blurb   => "The message suffix lines counter",
         Default => " lines read"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "mixed-storage-error",
         Nick    => "Mixed storage error",
         Blurb   => "The message on mixed storage error",
         Default =>
            (  "References to different projects are mixed in "
            &  "a context restricted to only one project"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "name-error",
         Nick    => "Wrong name",
         Blurb   => "The message on name error",
         Default =>
            (  "The name is illegal. A legal name starts "
            &  "with a letter. It can contain letters, digits, "
            &  "punctuation connectors, blank characters. "
            &  "The latter two may appear only as singletons. "
            &  "They also may not appear at the at of a name."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "new-name-label",
         Nick    => "New name",
         Blurb   => "The new name label",
         Default => "New name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-directory-error",
         Nick    => "No directory",
         Blurb   => "The message when no directory exist",
         Default =>
            (  "The directory specified in the file path "
            &  "does not exist."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-file-error-begin",
         Nick    => "No such file prefix",
         Default => "No file '",
         Blurb   =>
            "The first part of the message on non-existent file"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-file-error-end",
         Nick    => "No such file suffix",
         Default => "' exists",
         Blurb   =>
            "The second part of the message on non-existent file"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-object-error-begin",
         Nick    => "Missing object",
         Blurb   => "The prefix message on missing object error",
         Default => "The path "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-object-error-end",
         Nick    => "Missing object",
         Blurb   => "The suffix message on missing object error",
         Default => " does not specify any existing object"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "open-file-error-begin",
         Nick    => "Open error file prefix",
         Default => "Failed to open file '",
         Blurb   =>
            "The first part of the message on open file error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "open-file-error-end",
         Nick    => "Open error file suffix",
         Default => "'",
         Blurb   =>
            "The second part of the message on open file error"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "output-file-label",
         Nick    => "Output file",
         Blurb   => "The output file name label",
         Default => "Output file name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "overwrite-query",
         Nick    => "Overwite",
         Blurb   => "The message to confirm file to overwrite",
         Default =>
            "Are you sure you want to overwrite existing file "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "overwrite-query-title",
         Nick    => "Confirm file to overwrite",
         Default => "Confirm file to overwrite",
         Blurb   => "The title of file overwrite dialog"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "overwrite-from-query",
         Nick    => "From",
         Default => " from ",
         Blurb   =>
            (  "The message part introducing the containing "
            &  "directory of the file waiting for overwriting "
            &  "confirmation"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      GLib.Properties.Icon_Size.Property.Gnew_Enum
      (  Name    => "query-icon-size",
         Nick    => "Icon size",
         Blurb   => "Query icon size",
         Default =>
            Gtk_Icon_Size_Enum'Val (Icon_Size_Dialog)
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "query-icon",
         Nick    => "Query icon",
         Blurb   => "The stock ID of query icon",
         Default => Stock_Dialog_Question
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "replace-name-query",
         Nick    => "Replaced object name",
         Blurb   => "The message of object replacement",
         Default => "Are you sure you want to replace existing "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_UInt
      (  Name    => "row-spacing",
         Nick    => "Row spacing",
         Blurb   => "Row spacing in the grid of elements",
         Minimum => 0,
         Maximum => GUInt'Last,
         Default => 3
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_Boxed
      (  Name       => "scanned-text-backgound-color",
         Boxed_Type => Gdk_Color_Type,
         Nick       => "Scanned color",
         Blurb      => "The background of scanned text"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "show-location-label",
         Nick    => "Show location",
         Blurb   => "The label of the show location button",
         Default => "Show location"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      GLib.Properties.Icon_Size.Property.Gnew_Enum
      (  Name    => "tab-icon-size",
         Nick    => "Size",
         Blurb   => "Tab icon size",
         Default =>
            Gtk_Icon_Size_Enum'Val (Icon_Size_Small_Toolbar)
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_UInt
      (  Name    => "tab-label-spacing",
         Nick    => "Tab spacing",
         Blurb   => "Spacing in the tab labels",
         Minimum => 0,
         Maximum => GUInt'Last,
         Default => 3
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "storage-browse-error",
         Nick    => "Wrong storage",
         Blurb   => "The message storage browsing error",
         Default => "No persistent storage specified"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_Boxed
      (  Name       => "unscanned-text-backgound-color",
         Boxed_Type => Gdk_Color_Type,
         Nick       => "Unscanned color",
         Blurb      => "The background of text to scan"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "unsupported-error",
         Nick    => "Unsuported object",
         Blurb   => "The message on unsupported object",
         Default =>
            (  "The object type is not supported by the "
            &  "persistent storage."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-file-icon",
         Nick    => "File icon",
         Blurb   => "The stock ID of a file view icon",
         Default => Stock_File
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "text-view-font",
         Nick    => "Text view font",
         Blurb   => "The font used for text viewing",
         Default => "monospace 10"
   )  );
   --
   -- Styles of menu items
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "class-label",
         Nick    => "Class",
         Default => "Class",
         Blurb   =>
            (  "The label that appears in the menu before "
            &  "the class name"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "dsn-label",
         Nick    => "DSN",
         Default => "Data source",
         Blurb   =>
            (  "The label that appears in the menu before "
            &  "the data source name"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-classify",
         Nick    => "Classify",
         Default => "Classify",
         Blurb   => "Classify item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-classify-input",
         Nick    => "Classify input",
         Default => "Classify interactively",
         Blurb   => "Classify input item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-copy",
         Nick    => "Copy",
         Default => "Copy",
         Blurb   => "Copy item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-delete",
         Nick    => "Delete",
         Default => "Delete",
         Blurb   => "Delete item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-learn",
         Nick    => "Learn",
         Default => "Learn",
         Blurb   => "Learn item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-move",
         Nick    => "Move",
         Default => "Move",
         Blurb   => "Move item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-rename",
         Nick    => "Rename",
         Default => "Rename",
         Blurb   => "Rename item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-save-as-file",
         Nick    => "Save as text",
         Default => "Save as text",
         Blurb   => "Save as text item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-verify",
         Nick    => "Verify",
         Default => "Verify",
         Blurb   => "Verify item name of the menu"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "menu-view",
         Nick    => "View",
         Default => "View",
         Blurb   => "View item name of the menu"
   )  );
   --
   -- Styles of folder queries
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-creation-error",
         Nick    => "Folder create error",
         Default => "Unable to create a new folder: ",
         Blurb   => (  "The message on failed folder creation "
                    &  "by exception message"
   )  )             );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-delete-name-query",
         Nick    => "Folder delete name",
         Blurb   => "The message of folder deletion confirmation",
         Default => "Are you sure you want to delete folder "
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-delete-from-query",
         Nick    => "From",
         Default => " from ",
         Blurb   =>
            (  "The message part introducing the parent of "
            &  "a folder waiting for deletion confirmation"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-delete-query-title",
         Nick    => "Confirm to delete",
         Default => "Confirm to delete",
         Blurb   => "The title of folder deletion dialog"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-empty-name-error",
         Nick    => "Empty name",
         Blurb   => "The message when folder name is empty",
         Default => "A folder name may not be empty"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-name-error",
         Nick    => "Wrong folder name",
         Blurb   => "The message on folder name error",
         Default =>
            (  "The folder name is illegal. A legal name starts "
            &  "with a letter. It can contain letters, digits, "
            &  "punctuation connectors, blank characters. "
            &  "The latter two may appear only as singletons. "
            &  "They also may not appear at the at of a name."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-name-label",
         Nick    => "Folder name",
         Blurb   => "The label of the folder name entry",
         Default => "Folder name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-object-error",
         Nick    => "Not a folder",
         Blurb   => "The message when the object is not a folder",
         Default => "The object specified is not a folder"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-folder-label",
         Nick    => "New folder",
         Blurb   => "The label of a folder creation tab",
         Default => "New folder"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-folder-icon",
         Nick    => "New folder icon",
         Blurb   => "The stock ID of a folder creation icon",
         Default => New_Folder_Icon
   )  );
   --
   -- Styles of credentials queries
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-credentials-error",
         Nick    => "Credentials error",
         Blurb   => "The message on data source access error",
         Default =>
            (  "The data source cannot be connected to. Check "
            &  "the name of the data source, the user name, "
            &  "the user password in the ODBC settings."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-empty-dsn-error",
         Nick    => "Empty DSN name",
         Blurb   => "The message when data source name is empty",
         Default => "The data source name may not be empty"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-empty-file-error",
         Nick    => "Empty file name",
         Blurb   => "The message when file name is empty",
         Default => "The data base file name must be specified"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-file-label",
         Nick    => "File",
         Blurb   => "The label of the data base file name entry",
         Default => "Data base file"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-name-label",
         Nick    => "Name",
         Blurb   => "The label of the data source name entry",
         Default => "Data source name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-open-error",
         Nick    =>
            "Open error message followed by exception message",
         Blurb   => "The message on data source open error",
         Default =>
            (  "Error connecting to the data source. "
            &  "Please, check the ODBC settings: "
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-password-label",
         Nick    => "Password",
         Blurb   => "The label of the password entry",
         Default => "Password"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-stored-password-label",
         Nick    => "Remember password",
         Blurb   => "The label of the password storing check box",
         Default => "Remember password"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-tab-dsn",
         Nick    => "DSN",
         Blurb   => "The label of the DSN tab",
         Default => "ODBC"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-tab-sqlite",
         Nick    => "SQLite",
         Blurb   => "The label of the SQLite tab",
         Default => "SQLite"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-tab-single-file",
         Nick    => "fdb",
         Blurb   => "The label of the single file database tab",
         Default => "Single-file database"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "project-user-label",
         Nick    => "User",
         Blurb   => "The label of the user name entry",
         Default => "User"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-project-label",
         Nick    => "New project",
         Blurb   => "The label of a project creation tab",
         Default => "New project"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-project-icon",
         Nick    => "New project icon",
         Blurb   => "The stock ID of a project creation icon",
         Default => Stock_Connect
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-move-label",
         Nick    => "Move",
         Blurb   => "The label of the move tab",
         Default => "Move"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-move-icon",
         Nick    => "Move icon",
         Blurb   => "The stock ID of move box icon",
         Default => Rename_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-rename-label",
         Nick    => "Rename",
         Blurb   => "The label of the rename tab",
         Default => "Rename"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-rename-icon",
         Nick    => "Renaming icon",
         Blurb   => "The stock ID of renaming box icon",
         Default => Rename_Item_Icon
   )  );
   --
   -- Styles of feature the panel
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "feature-name-label",
         Nick    => "Feature name",
         Blurb   => "The label of the feature name field",
         Default => "Feature name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "folder-path-label",
         Nick    => "Folder path",
         Blurb   => "The label of the folder path field",
         Default => "Created in"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-feature-label",
         Nick    => "Features",
         Blurb   => "The label of the features tab",
         Default => "Features"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-feature-label",
         Nick    => "New feature",
         Blurb   => "The label of a feature creation tab",
         Default => "New feature"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-feature-icon",
         Nick    => "New feature icon",
         Blurb   => "The stock ID of the feature creation icon",
         Default => New_Feature_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-view-feature-icon",
         Nick    => "Feature properties icon",
         Blurb   => "The stock ID of the feature properties icon",
         Default => Feature_View_Icon
   )  );
   --
   -- Styles of the classifiers panel
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classifier-name-label",
         Nick    => "Classifier name",
         Blurb   => "The label of the classifier name field",
         Default => "Classifier name"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classifier-label",
         Nick    => "Classifiers",
         Blurb   => "The label of the classifiers tab",
         Default => "Classifiers"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classifier-icon",
         Nick    => "Classifier icon",
         Blurb   => "The stock ID of the classifier icon",
         Default => Classifier_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-classifier-label",
         Nick    => "New classifier",
         Blurb   => "The label of a classifier creation tab",
         Default => "New classifier"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-classifier-icon",
         Nick    => "New classifier icon",
         Blurb   => "The stock ID of classifier creation icon",
         Default => Stock_Execute
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-view-classifier-icon",
         Nick    => "Classifier view icon",
         Blurb   => "The stock ID of the lecture view icon",
         Default => Classifier_Icon
   )  );
   --
   -- Styles of lectures panel
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "allow-empty-fields-label",
         Nick    => "Allow empty",
         Blurb   => "The label of allow empty fields check box",
         Default => "Allow empty fields"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "blank-delimiter-label",
         Nick    => "Blank",
         Default => "None, only blanks",
         Blurb   => "Blanks as delimiter label"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "by-default-label",
         Nick    => "By default",
         Blurb   => "The label of by default radio buttons",
         Default => "by default"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "comment-examples-prefix-label",
         Nick    => "Comment",
         Blurb   => "The label of comments prefix",
         Default => "Comment prefix"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "copy-lecture-name-label",
         Nick    => "Copy name",
         Blurb   => "The label of the source lecture name field",
         Default => "Training set to copy"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "default-necessity-label",
         Nick    => "Default necessity",
         Blurb   => "The label of default necessity check box",
         Default => "Default necessity to possibility"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "delimiter-frame-label",
         Nick    => "Delimiter",
         Blurb   => "The label of delimiter selection framebox",
         Default => "Delimiter of feature values"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "delimiters-list-frame-label",
         Nick    => "Delimiters list",
         Blurb   => "The label of delimiters selection framebox",
         Default => "Delimiters of feature values"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "duplicated-example-keyword-error",
         Nick    => "Duplicated keywords",
         Blurb   => "The message shown on duplicated keywords",
         Default =>
            (  "The keywords recognized as special cases of "
            &  "examples may not duplicated each other. "
            &  "Some of duplicating keywords are marked."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "encoding-label",
         Nick    => "Encoding",
         Blurb   => "The label of the encoding selection box",
         Default => "Encoding"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "invalid-delimiters-error",
         Nick    => "Invalid delimiters",
         Blurb   => "The message shown when delimiters invalid",
         Default =>
            "The delimiters must be valid Latin-1 code points"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "lecture-name-label",
         Nick    => "Lecture name",
         Blurb   => "The label of the training set name field",
         Default => "Training set"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "modified-query",
         Nick    => "Unsaved",
         Blurb   => "The message of unsaved changes",
         Default => "Are you sure you don't want to save changes?"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "modified-query-title",
         Nick    => "Confirm unsaved",
         Default => "Confirm dropped changes",
         Blurb   => "The title of unsaved changes dialog"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "deselected-features-to-save-title",
         Nick    => "Training set features",
         Default => "Training set features",
         Blurb =>
            (  "The title of the column containing the list "
            &  "of features in the training set"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "examples-write-check-button-label",
         Nick    => "Write",
         Blurb   => "The label of examples write check button",
         Default => "Write"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "full-examples-prefix-label",
         Nick    => "Full",
         Blurb   => "The label of full examples prefix",
         Default => "Full examples' prefix"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "negative-examples-prefix-label",
         Nick    => "Negative",
         Blurb   => "The label of negative examples prefix",
         Default => "Negatives' prefix"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-examples-error",
         Nick    => "No examples to write",
         Default =>
            (  "Neither positive nor negative training set "
            &  "examples are chosen to write"
            ),
         Blurb =>
            (  "The message shown when neither positive nor "
            &  "negative examples are to write"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-delimiter-error",
         Nick    => "No delimiter specified",
         Default => "Delimiter shall not be empty",
         Blurb   => "The message shown when delimiter is empty"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "other-delimiter-label",
         Nick    => "Other",
         Default => "Other",
         Blurb   => "User-specified delimiter label"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "overwrite-query-begin",
         Nick    => "Overwrite prefix",
         Blurb   => "The message prefix of file overwriting",
         Default => "Do you want to overwrite file '"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "overwrite-query-end",
         Nick    => "Overwrite suffix",
         Blurb   => "The message suffix of file overwriting",
         Default => "'?"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "positive-examples-prefix-label",
         Nick    => "Positive",
         Blurb   => "The label of positive examples prefix",
         Default => "Positives' prefix"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "quote-units-label",
         Nick    => "Quote units",
         Blurb   => "The label of quoted units check box",
         Default => "Units put in square brackets"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "require-units-label",
         Nick    => "Require units",
         Blurb   => "The label of require units check box",
         Default => "Require unit specification"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "replace-lecture-title",
         Nick    => "Confirm training set to overwrite",
         Default => "Confirm training set to overwrite",
         Blurb   => "The title of lecture overriding dialog"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "same-examples-prefix-error",
         Nick    => "Same perfix",
         Blurb   =>
            "The message shown when examples prefices are same",
         Default =>
            (  "Positive and negative examples shall have "
            &  "different prefixes to become distinguishable"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "selected-features-to-save-title",
         Nick    => "Features to write",
         Default => "Features to write",
         Blurb =>
            (  "The title of the column containing the list "
            &  "of features to save in the file"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "semicolon-delimiter-label",
         Nick    => "Semicolon",
         Default => "Semicolon",
         Blurb   => "The semicolon delimiter label"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "space-delimiter-label",
         Nick    => "Space",
         Default => "Space",
         Blurb   => "The space delimiter label"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-difference-lecture-label",
         Nick    => "Lectures difference view",
         Blurb   => "The label of a lectures difference tab",
         Default => "Training sets difference"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-difference-lecture-icon",
         Nick    => "Lectures difference icon",
         Blurb   => "The stock ID of the lecture difference icon",
         Default => Compare_Lectures_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-edit-lecture-icon",
         Nick    => "Lecture edit icon",
         Blurb   => "The stock ID of the lecture editor icon",
         Default => Lecture_Edit_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-edited-lecture-icon",
         Nick    => "Lecture edited icon",
         Default => Lecture_Edited_Icon,
         Blurb   => (  "The stock ID of the icon of the lecture "
                    &  "editor with a modified set"
   )  )             );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-lecture-icon",
         Nick    => "New lecture icon",
         Default => New_Lecture_Icon,
         Blurb   => (  "The stock ID of the icon of a new "
                    &  "lecture"
   )  )             );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-lecture-label",
         Nick    => "Training sets",
         Blurb   => "The label of the lectures tab",
         Default => "Training sets"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-new-lecture-label",
         Nick    => "New training set",
         Blurb   => "The label of the new lecture tab",
         Default => "New training set"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-save-lecture-label",
         Nick    => "Save training set",
         Blurb   => "The label of a training set save tab",
         Default => "Save training set as"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-save-lecture-icon",
         Nick    => "Save training set icon",
         Blurb   => "The stock ID of training set save icon",
         Default => Stock_Save_As
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-view-lecture-icon",
         Nick    => "Lecture view icon",
         Blurb   => "The stock ID of the lecture view icon",
         Default => Lecture_View_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tabulator-delimiter-label",
         Nick    => "Tabulator",
         Default => "Tabulator",
         Blurb   => "The tabulator delimiter label"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "undefined-example-label",
         Nick    => "Undefined",
         Blurb   => "The label of undefined example value",
         Default => "Undefined value"
   )  );
   --
   -- Styles of the classification box
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classify-input-feature-name-title",
         Nick    => "Feature",
         Blurb   => "The title of the feature name column",
         Default => "Feature"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classify-input-feature-value-title",
         Nick    => "Value",
         Blurb   => "The title of the feature value column",
         Default => "Value"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classify-lecture-name-label",
         Nick    => "Lecture name",
         Blurb   => "The label of the training set name field",
         Default => "Set to classify"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classify-source-name-label",
         Nick    => "Source name",
         Blurb   => "The label of the source name field",
         Default => "Trained on (optional)"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "classify-threshold-label",
         Nick    => "Threshold",
         Blurb   => "The label of the classification threshold",
         Default => "Threshold"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "generalization-label",
         Nick    => "Generalization",
         Blurb   => "The generalization mode label",
         Default => "Generalization"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classify-label",
         Nick    => "Classify",
         Blurb   => "The label of the classification tab",
         Default => "Classify"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classify-icon",
         Nick    => "Classification icon",
         Blurb   => "The stock ID of classification box icon",
         Default => Classify_Icon
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classify-input-label",
         Nick    => "Classify one",
         Blurb   => "The label of the input classification tab",
         Default => "Classify one"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-classify-input-icon",
         Nick    => "Input classification icon",
         Default => Classify_Input_Icon,
         Blurb   =>
            "The stock ID of input classification box icon"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-verify-label",
         Nick    => "Verify",
         Blurb   => "The label of the verification tab",
         Default => "Verify"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-verify-icon",
         Nick    => "Verification icon",
         Blurb   => "The stock ID of verification box icon",
         Default => Verify_Icon
   )  );
   --
   -- Styles of the learn box
   --
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "canceled-error",
         Nick    => "Canceled error",
         Blurb   => "The message when training was canceled",
         Default => "Canceled by user request"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "equivalence-error",
         Nick    => "Equivalence error",
         Blurb   => "The message when equivalence is wrong",
         Default =>
            (  "The equivalence level of the training set "
            &  "features must be specified as a truth value "
            &  "in the range 0..1. Two features are considered "
            &  "of same separation quality when the separation "
            &  "estimation is under the value specified. No "
            &  "features selection happens when the value is 1"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "examples-range-error",
         Nick    => "Range error",
         Blurb   => "The message when the examples range error",
         Default =>
            "The range of training examples (from..to) is empty"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "from-example-label",
         Nick    => "From example",
         Blurb   => "The label of the first example field",
         Default => "From example"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "from-example-error",
         Nick    => "From error",
         Blurb   => "The message when the example from is wrong",
         Default =>
            (  "The field from, specifying the first training "
            &  "example, must contain a number within the range"
            &  "of available training examples"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "from-example-range-error",
         Nick    => "From range error",
         Blurb   =>
            "The message when the example from is not in range",
         Default =>
            (  "The first training example (from) is outside the "
            &  "training set"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "no-features-error",
         Nick    => "No features error",
         Blurb   => "The message when no features was selected",
         Default =>
            (  "There must be at least two features selected for "
            &  "training. The last feature in the list specifies "
            &  "the class is the classifier's output. "
            &  "Other features, at least one, are used by the "
            &  "classifier in order to take decision."
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "out-of-examples-label",
         Nick    => "Out of",
         Blurb   => "The label of the out of examples field",
         Default => "out of"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "tab-learn-label",
         Nick    => "Learn",
         Blurb   => "The label of the learn tab",
         Default => "Learn"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "learn-threshold-label",
         Nick    => "Threshold",
         Blurb   => "The label of the training threshold",
         Default => "Threshold"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "learn-equivalence-label",
         Nick    => "Equivalence",
         Default => "Equivalence",
         Blurb   =>
            "The label of the feature equivalence threshold"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "learn-unused-title",
         Nick    => "Features",
         Blurb   => "The title of the features unused",
         Default => "Features"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "learn-used-title",
         Nick    => "Used features",
         Blurb   => "The title of the features used",
         Default => "Features to learn on"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "to-example-error",
         Nick    => "To error",
         Blurb   => "The message when the example to is wrong",
         Default =>
            (  "The field to, specifying the last training "
            &  "example, must contain a number within the range"
            &  "of available training examples"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "to-example-label",
         Nick    => "To example",
         Blurb   => "The label of the last example field",
         Default => "To example"
   )  );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "threshold-error",
         Nick    => "Threshold error",
         Blurb   => "The message when threshold is wrong",
         Default =>
            (  "The threshold of confidence levels of the "
            &  "training set examples must be a truth value "
            &  "in the range 0..1. The truth values of the "
            &  "examples under the threshold are considered 0"
   )  )     );
   Install_Style_Property
   (  Class_Ref (Class_Record.The_Type),
      Gnew_String
      (  Name    => "to-example-range-error",
         Nick    => "To range error",
         Blurb   =>
            "The message when the example to is not in range",
         Default =>
            (  "The last training example (to) is outside the "
            &  "training set"
   )  )     );
end Install_Class_Properties;
