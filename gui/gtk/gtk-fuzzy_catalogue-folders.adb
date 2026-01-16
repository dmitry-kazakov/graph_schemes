--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Folders                 Luebeck            --
--  Implementation                                 Spring, 2008       --
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

with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Deposit_Handles;                use Deposit_Handles;
with Fuzzy.Feature.Handle.Container; use Fuzzy.Feature.Handle.Container;
with Fuzzy.Lecture.Handle.Factory;   use Fuzzy.Lecture.Handle.Factory;
with Fuzzy.Persistence;              use Fuzzy.Persistence;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Indicators.Counter;         use Gtk.Indicators.Counter;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Source_Language_Manager;    use Gtk.Source_Language_Manager;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;    use Gtk.Widget.Styles.Icon_Size;
with Indicator.Handle;               use Indicator.Handle;
with Name_Tables;                    use Name_Tables;
with Persistent;                     use Persistent;
with Persistent.Handle;              use Persistent.Handle;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.UTF8.Mapping;      use Strings_Edit.UTF8.Mapping;

with Ada.Directories;
with Gtk.Fuzzy_Catalogue.File_Views;
with GLib.Object.Checked_Destroy;
with Gtk.Wildcard_Directory_Browser;
with Persistent.Directory;

package body Gtk.Fuzzy_Catalogue.Folders is
   use Gtk.Fuzzy_Catalogue.File_Views;
   use Gtk.Wildcard_Directory_Browser;
   use Rules_Lists;

   Read_From_FCL_Key : constant String := "read from FCL";
   Abort_Error       : exception;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Folders." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Folder_Program,
             Folder_Program_Ptr
          );

   procedure Aborted
             (  Item  : not null access Folder_From_FCL_Box_Record;
                Fault : Exception_Occurrence
             )  is
      Button : Show_Location_Buttons.Gtk_Style_Button;
      Box    : Gtk_VBox;
   begin
      Gtk.Fuzzy_Catalogue.Aborted
      (  Gtk_Item_Box_Record (Item.all)'Unchecked_Access,
         Fault
      );
      if Item.Code /= null then
         Erase (Item.Code.Features);
      end if;
      if Exception_Identity (Fault) = Parsers.Syntax_Error'Identity then
         declare
            Text     : constant String := Exception_Message (Fault);
            Pointer  : Integer;
            Has_Line : Boolean := False;
         begin
            for Index in reverse Text'First..Text'Last - 1 loop
               if Text (Index..Index + 1) = "at" then
                  Pointer := Index + 2;
                  Get (Text, Pointer);
                  Get (Text, Pointer, Item.Line_From_No);
                  Get (Text, Pointer);
                  if Pointer > Text'Last or else Text (Pointer) /= ':'
                  then
                     Aborted
                     (  Gtk_Item_Box_Record (Item.all)'Access,
                        Fault
                     );
                     return;
                  end if;
                  Pointer := Pointer + 1;
                  Get (Text, Pointer);
                  Get (Text, Pointer, Item.Position_From_No);
                  Get (Text, Pointer);
                  if Pointer > Text'Last then
                     Item.Line_To_No := Item.Line_From_No;
                     Item.Position_To_No := Item.Position_From_No - 1;
                     Has_Line := True;
                     exit;
                  end if;
                  if (  Pointer >= Text'Last
                     or else
                        Text (Pointer..Pointer + 1) /= ".."
                     )
                  then
                     Aborted
                     (  Gtk_Item_Box_Record (Item.all)'Access,
                        Fault
                     );
                     return;
                  end if;
                  Pointer := Pointer + 2;
                  Get (Text, Pointer);
                  Get (Text, Pointer, Item.Line_To_No);
                  Get (Text, Pointer);
                  if Pointer >= Text'Last then
                     Item.Position_To_No := Item.Line_To_No;
                     Item.Line_To_No     := Item.Line_From_No;
                     Has_Line := True;
                     exit;
                  end if;
                  if Pointer > Text'Last or else Text (Pointer) /= ':'
                  then
                     Aborted
                     (  Gtk_Item_Box_Record (Item.all)'Access,
                        Fault
                     );
                     return;
                  end if;
                  Pointer := Pointer + 1;
                  Get (Text, Pointer);
                  Get (Text, Pointer, Item.Position_To_No);
                  Get (Text, Pointer);
                  Has_Line := True;
                  exit;
               end if;
            end loop;
            if not Has_Line then
               Aborted (Gtk_Item_Box_Record (Item.all)'Access, Fault);
               return;
            end if;
         exception
            when others =>
              Aborted (Gtk_Item_Box_Record (Item.all)'Access, Fault);
              return;
         end;
         Gtk_New_VBox (Box);
         Pack_Start (Item.Error_Box, Box, False, False);
         Gtk_New (Button);
         Pack_Start (Box, Button, False, False);
         Show_All (Box);
         FCL_Handlers.Connect
         (  Button,
            "clicked",
            Show_Location'Access,
            Item.all'Unchecked_Access
         );
      else
         Aborted (Gtk_Item_Box_Record (Item.all)'Access, Fault);
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Aborted")
         )  );
   end Aborted;

   procedure Commit (Item : not null access Folder_Create_Box_Record) is
      Raw_Name : constant UTF8_String := Get_Text (Item.Name_Entry);
   begin
      if Raw_Name'Length = 0 then
         -- Wrong name
         Error
         (  Item,
            Style_Get (Item.Browser, "folder-empty-name-error")
         );
         Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
         return;
      end if;
      begin
         Check_Name (Raw_Name);
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "folder-name-error")
            );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
      end;
      declare
         Folder  : Deposit_Handle;
         Path    : constant Item_Path :=
                            Get_Current_Directory (Item.Browser.Tree);
         Storage : Storage_Handle;
         Parent  : Deposit_Handle;
         Name    : constant UTF8_String :=
                            Name_Maps.Canonize (Raw_Name);
      begin
         Browse
         (  Store   => Get_Cache (Item.Browser),
            Path    => Item_Path (Path),
            Storage => Storage,
            Object  => Parent,
            Partial => False
         );
         if not Is_Valid (Storage) then
            Error
            (  Item,
               Style_Get (Item.Browser, "storage-browse-error")
            );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         end if;
         if Catalogue.Is_In
            (  Get_List
               (  Storage     => Storage,
                  Equivalence => To_Lowercase'Access,
                  Parent      => Parent
               ),
               Name
            )
         then
            Error_Duplicated
            (  Item    => Item,
               Name    => Name,
               Storage => Storage,
               Parent  => Parent
            );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         end if;
         begin
            Persistent.Directory.Create
            (  Storage   => Storage,
               Directory => Folder,
               Name      => Name,
               Parent    => Parent
            );
            Created
            (  Store     => Get_Cache (Item.Browser),
               Directory => Path,
               Item      =>
               (  Name_Length => Name'Length,
                  Kind_Length => Persistent.Directory.
                                    Directory_Class'Length,
                  Policy      => Cache_Expanded,
                  Directory   => True,
                  Name        => Item_Name (Name),
                  Kind        => Item_Type
                                 (  Persistent.Directory.Directory_Class
            )  )                 );
            Delete (Item);
         exception
            when Name_Error =>
               Error_Duplicated
               (  Item    => Item,
                  Name    => Name,
                  Storage => Storage,
                  Parent  => Parent
               );
               Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            when Reason : Data_Error =>
               Error
               (  Item,
                  (  Style_Get (Item.Browser, "folder-creation-error")
                  &  Exception_Message (Reason)
               )  );
         end;
      exception
         when Reason : others =>
            Error (Item, Exception_Message (Reason));
      end;
   exception
      when Name_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   procedure Commit
             (  Item : not null access Folder_From_FCL_Box_Record
             )  is
      Wait      : Wait_Cursor (Item);
      Directory : Unbounded_String;
   begin
      if Item.Directory = null then
         -- Folder name check
         declare
            Raw_Name : constant UTF8_String :=
                          Get_Text (Item.Name_Entry);
         begin
            if Raw_Name'Length = 0 then
               -- Wrong name
               Error
               (  Item,
                  Style_Get (Item.Browser, "folder-empty-name-error")
               );
               return;
            end if;
            begin
               Check_Name (Raw_Name);
            exception
               when Constraint_Error =>
                  Error
                  (  Item,
                     Style_Get (Item.Browser, "folder-name-error")
                  );
                  return;
            end;
               -- Folder creation
            declare
               Folder  : Deposit_Handle;
               Lesson  : Lecture_Handle;
               Feature : Feature_Handle;
               Name    : constant UTF8_String :=
                            Name_Maps.Canonize (Raw_Name);
            begin
               if Catalogue.Is_In
                  (  Get_List
                     (  Storage     => Item.Storage,
                        Equivalence => To_Lowercase'Access,
                        Parent      => Item.Parent
                     ),
                     Name
                  )
               then
                  Error_Duplicated
                  (  Item    => Item,
                     Name    => Name,
                     Storage => Item.Storage,
                     Parent  => Folder
                  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               end if;
               Persistent.Directory.Create
               (  Storage   => Item.Storage,
                  Directory => Folder,
                  Name      => Name,
                  Parent    => Item.Parent
               );
               -- Adding features
               for Index in 1..Get_Size (Item.Code.Features) loop
                  Feature := Ref (Item.Code.Features, Index);
                  Rename
                  (  Storage    => Item.Storage,
                     Object     => Feature,
                     New_Name   => Get_Name (Feature),
                     New_Parent => Folder
                  );
               end loop;
               -- Adding lectures
               for Index in 1..GetSize (Item.Code.Rules) loop
                  Lesson := GetTag (Item.Code.Rules, Index).Lesson;
                  Rename
                  (  Storage    => Item.Storage,
                     Object     => Lesson,
                     New_Name   => GetName (Item.Code.Rules, Index),
                     New_Parent => Folder
                  );
               end loop;
               Erase (Item.Code.Features);
               -- Update cache and switch to the created folder
               Created
               (  Store     => Get_Cache (Item.Browser),
                  Directory =>
                     Get_Path
                     (  Store   => Item.Browser.Cache,
                        Storage => Item.Storage,
                        Object  => Item.Parent
                     ),
                  Item      =>
                  (  Name_Length => Name'Length,
                     Kind_Length => Persistent.Directory.
                                       Directory_Class'Length,
                     Policy      => Cache_Expanded,
                     Directory   => True,
                     Name        => Item_Name (Name),
                     Kind        => Item_Type
                                    (  Persistent.
                                       Directory.Directory_Class
               )   )                );
               Set_Current_Directory
               (  Get_Tree_View (Item.Browser),
                  Get_Path
                  (  Store   => Item.Browser.Cache,
                     Storage => Item.Storage,
                     Object  => Folder
               )  );
               Delete (Item);
            exception
               when Name_Error =>
                  Error_Duplicated
                  (  Item    => Item,
                     Name    => Name,
                     Storage => Item.Storage,
                     Parent  => Item.Parent
                  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
               when Reason : Data_Error =>
                  Error
                  (  Item,
                     (  Style_Get
                        (  Item.Browser,
                           "folder-creation-error"
                        )
                     &  Exception_Message (Reason)
                  )  );
                  if Is_Valid (Folder) then
                     Unname (Item.Storage, Folder);
                  end if;
               when others =>
                  if Is_Valid (Folder) then
                     Unname (Item.Storage, Folder);
                  end if;
                  raise;
            end;
         end;
      else
         -- Determining the path
         declare
            Folder : Deposit_Handle;
            Path   : constant Item_Path :=
                              Get_Current_Directory (Item.Browser.Tree);
         begin
            Browse
            (  Store   => Get_Cache (Item.Browser),
               Path    => Item_Path (Path),
               Storage => Item.Storage,
               Object  => Item.Parent,
               Partial => False
            );
            if not Is_Valid (Item.Storage) then
               Error
               (  Item,
                  Style_Get (Item.Browser, "storage-browse-error")
               );
               return;
            end if;
         end;
            -- File name
         Item.File_Name :=
            To_Unbounded_String (Trim (Get_Text (Item.File_Name_Edit)));
         if Length (Item.File_Name) = 0 then
            Set_Hint
            (  Item.File_Name_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            Error
            (  Item,
               Style_Get (Item.Browser, "empty-file-name-error")
            );
            return;
         end if;
         if Is_Absolute (To_String (Item.File_Name)) then
            Append
            (  Directory,
               Get_Dirname (To_String (Item.File_Name))
            );
         else
            Append (Directory, Get_Directory (Item.Directory));
            Item.File_Name :=
               To_Unbounded_String
               (  Build_Filename
                  (  To_String (Directory),
                     To_String (Item.File_Name)
               )  );
         end if;
         begin
            if not Ada.Directories.Exists (To_String (Item.File_Name))
            then
               Set_Hint
               (  Item.File_Name_Hint,
                  Item.Browser,
                  Erroneous,
                  True
               );
               Error
               (  Item,
                  (  Style_Get (Item.Browser, "no-file-error-begin")
                  &  To_String (Item.File_Name)
                  &  Style_Get (Item.Browser, "no-file-error-end")
               )  );
               return;
            end if;
         exception
            when others =>
               Set_Hint
               (  Item.File_Name_Hint,
                  Item.Browser,
                  Erroneous,
                  True
               );
               Error
               (  Item,
                  Style_Get (Item.Browser, "invalid-file-name-error")
               );
               return;
         end;
         Item.Encoding := Get (Item.Encoding_Box);
         Clean (Item);
         begin
            Store
            (  Widget => Item.Browser,
               Key    => Read_From_FCL_Key,
               Value  => Get_Directory (Item.Directory)
            );
         exception
            when Name_Error =>
               null;
         end;
         Start_Servicing (Item);
      end if;
   exception
      when Reason : others =>
         Error (Item, Exception_Information (Reason));
   end Commit;

   procedure Completed
             (  Item : not null access Folder_From_FCL_Box_Record
             )  is
   begin
      Remove (Item.Grid, Item.File_Name_Label);
      Remove (Item.Grid, Item.File_Name_Hint);
      Remove (Item.Grid, Item.File_Name_Edit);
      Remove (Item, Item.Directory_Frame);
      Item.Directory := null;
      if Get_Text (Item.Name_Entry)'Length > 0 then
         -- The name was specified already
         Commit (Item);
      else
         -- Take name from the code and let user confirm it
         Set_Text (Item.Name_Entry, To_String (Item.Code.Name));
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Completed")
         )  );
   end Completed;

   procedure Create
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      if Get_Current_Directory (Browser.Tree)'Length /= 0 then
         if Is_Valid (Browser.Folder) then
            Browser.Set_Item (Browser.Folder.Get);
         else
            declare
               Item : Folder_Create_Box;
            begin
               Gtk_New (Item, Browser);
            end;
         end if;
      end if;
   exception
      when Name_Error =>
         null;
   end Create;

   function Create_Indicator
            (  Item : not null access Folder_From_FCL_Box_Record
            )  return Gtk_Indicator is
      Progress : Gtk_Counter;
      Button   : Abort_Buttons.Gtk_Style_Button;
   begin
      Gtk_New (Button);
      Gtk_New
      (  Widget          => Progress,
         Button          => Button.all'Unchecked_Access,
         Button_Position => (2, 3, 0, 1),
         Size            => (3, 1),
         Label_Text      => Style_Get (Item.Browser, "lines-read"),
         Spacing =>
            (  Width  => Style_Get (Item.Browser, "row-spacing"),
               Height => Style_Get (Item.Browser, "column-spacing")
      )     );
      return Progress.all'Unchecked_Access;
   end Create_Indicator;

   procedure Created_Feature
             (  Compiler : in out Folder_Program;
                Feature  : in out Feature_Handle
             )  is
   begin
      Put (Compiler.Item.Storage, Feature);
      Add (Compiler.Features, Feature);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Created_Feature")
         )  );
   end Created_Feature;

   function Create_Rules
            (  Compiler : Folder_Program;
               Name     : String
            )  return Lecture_Handle is
   begin
      return Create_Persistent (Compiler.Item.Storage);
   end Create_Rules;

   procedure Delete
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Path : constant Item_Path := Get_Current_Directory (Browser.Tree);
   begin
      if Path'Length /= 0 then
         declare
            Title : constant String :=
                Style_Get (Browser, "folder-delete-query-title");
            Icon  : constant String :=
               Style_Get (Browser, "query-icon");
            Size  : constant Gtk_Icon_Size_Enum :=
               Style_Get (Browser, "query-icon-size");
            Message : constant String :=
               (  Style_Get (Browser, "folder-delete-name-query")
               &  String (Get_Name (Path))
               );
            Result : Gtk_Response_Type;
         begin
            if Is_Root (Path) then
               Result :=
                  Query
                  (  Browser,
                     Title,
                     Icon,
                     Size,
                     Message
                  );
            else
               Result :=
                  Query
                  (  Browser,
                     Title,
                     Icon,
                     Size,
                     (  Message
                     &  Style_Get (Browser, "folder-delete-from-query")
                     &  String (Get_Directory (Path))
                 )  );
            end if;
            if Result = Gtk_Response_OK then
               if not Is_Root (Path) then
                  --
                  -- Moving the current directory to the  parent  before
                  -- we remove the current one
                  --
                  Set_Current_Directory
                  (  Get_Tree_View (Browser),
                     Get_Directory (Path)
                  );
               end if;
               Delete (Get_Cache (Browser), Path);
            end if;
         end;
      end if;
   exception
      when Name_Error =>
         null;
      when Reason : others =>
         Fault (Browser, Exception_Message (Reason));
   end Delete;

   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             )  is
      File : constant UTF8_String := Get_Selection (Item.Directory);
   begin
      if File'Length > 0 then
         Set_Text (Item.File_Name_Edit, File);
         Set_Hint (Item.File_Name_Hint, Item.Browser, Checked, True);
         Set_Sensitive
         (  Item.View_Button,
            (  File_Test (File, File_Test_Exists)
            and then
               File_Test (File, File_Test_Is_Regular)
         )  );
      else
         Set_Sensitive (Item.View_Button, False);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("File_Changed")
         )  );
   end File_Changed;

   procedure Finalize
             (  Item : not null access Folder_From_FCL_Box_Record
             )  is
      Wait : Wait_Cursor (Item.Browser);
   begin
      Free (Item.Code);
      Finalize (Gtk_Item_Box_Record (Item.all)'Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   procedure From_FCL
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      if Get_Current_Directory (Browser.Tree)'Length /= 0 then
         declare
            Item : Folder_From_FCL_Box;
         begin
            Gtk_New (Item, Browser);
         end;
      end if;
   exception
      when Name_Error =>
         null;
   end From_FCL;

   procedure Get_Line (Code : in out Latin1_Source_File) is
      use Parsers.Multiline_Source.Latin1_Text_IO;
   begin
      Get_Line (Source (Code));
      begin
         Check (Code.Viewer.all);
      exception
         when End_Error =>
            raise Abort_Error;
      end;
   end Get_Line;

   procedure Get_Line (Code : in out UTF8_Source_File) is
      use Parsers.Multiline_Source.Text_IO;
   begin
      Get_Line (Source (Code));
      begin
         Check (Code.Viewer.all);
      exception
         when End_Error =>
            raise Abort_Error;
      end;
   end Get_Line;

   procedure Gtk_New
             (  Item    : out Folder_Create_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Folder_Create_Box_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Folders.Initialize (Item);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Folder_Create_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Item    : out Folder_From_FCL_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Folder_From_FCL_Box_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Folders.Initialize (Item);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Folder_From_FCL_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item : not null access Folder_Create_Box_Record'Class
             )  is
      Grid  : Gtk_Table;
      Label : Gtk_Label;
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "folder create box", True);
      Gtk_New (Grid, 1, 3, False);
      Set_Col_Spacings (Grid, Column_Spacing);
      Set_Row_Spacings (Grid, Row_Spacing);

      Gtk_New (Label, Style_Get (Item.Browser, "folder-name-label"));
      Attach (Grid, Label, 0, 1, 0, 1, Fill, 0);
      Set_Justify (Label, Justify_Right);
      Set_Alignment (Label, 1.0, 0.5);

      Gtk_New (Item.Name_Entry);
      Attach
      (  Grid,
         Item.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );
      Gtk_New_HBox (Item.Name_Hint);
      Attach
      (  Grid,
         Item.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Name_Hint, Item.Browser, None, True);

      Pack_Start (Item, Grid, False, False);
      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-new-folder-label"),
         Style_Get (Item.Browser, "tab-new-folder-icon"),
         Item
      );
      Set (Item.Browser.Folder, Item);
   end Initialize;

   procedure Initialize
             (  Item : not null access Folder_From_FCL_Box_Record'Class
             )  is
      Label     : Gtk_Label;
      Separator : Gtk_Separator;
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "FCL folder create box", True);
      Gtk_New (Item.Grid, 3, 3, False);
      Set_Col_Spacings (Item.Grid, Column_Spacing);
      Set_Row_Spacings (Item.Grid, Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "folder-name-label"));
      Attach (Item.Grid, Label, 0, 1, 0, 1, Fill, 0);
      Set_Justify (Label, Justify_Right);
      Set_Alignment (Label, 1.0, 0.5);

      Gtk_New (Item.Name_Entry);
      Attach
      (  Item.Grid,
         Item.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );
      Gtk_New_HBox (Item.Name_Hint);
      Attach
      (  Item.Grid,
         Item.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Name_Hint, Item.Browser, None, True);
         -- Row 2
      Gtk_New
      (  Item.File_Name_Label,
         Style_Get (Item.Browser, "input-file-label")
      );
      Attach
      (  Item.Grid,
         Item.File_Name_Label,
         0, 1,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Alignment (Item.File_Name_Label, 1.0, 0.5);
      Gtk_New (Item.File_Name_Edit);
      Attach
      (  Item.Grid,
         Item.File_Name_Edit,
         1, 2,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.File_Name_Hint);
      Attach
      (  Item.Grid,
         Item.File_Name_Hint,
         2, 3,
         2, 3,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.File_Name_Hint, Item.Browser, None, True);
         -- Row 3
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "encoding-label")
      );
      Attach
      (  Item.Grid,
         Label,
         0, 1,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Alignment (Label, 1.0, 0.5);
      Gtk_New (Item.Encoding_Box, UTF8_Set);
      Attach
      (  Item.Grid,
         Item.Encoding_Box,
         1, 2,
         3, 4,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );

      Pack_Start (Item, Item.Grid, False, False);

      Gtk_New (Item.Directory_Frame);
      Set_Shadow_Type (Item.Directory_Frame, Shadow_In);
      Gtk_New
      (  Widget  => Item.Directory,
         Browser => Item.Browser,
         Key     => Read_From_FCL_Key
      );
      Set_Pattern (Get_Directory_Browser (Item.Directory), "*.fcl");
      Set_Text (Get_Filter_Entry (Item.Directory), "*.fcl");

      Gtk_New_Vseparator (Separator);
      Pack_Start
      (  Get_Title_Box (Item.Directory),
         Separator,
         False,
         False
      );

      Gtk_New (Item.View_Button);
      Pack_Start
      (  Get_Title_Box (Item.Directory),
         Item.View_Button,
         False,
         False
      );
      Set_Sensitive (Item.View_Button, False);
      FCL_Handlers.Connect
      (  Item.View_Button,
         "clicked",
         View_File'Access,
         Item.all'Access
      );

      Gtk_New (Item.New_Button);
      Pack_Start
      (  Get_Title_Box (Item.Directory),
         Item.New_Button,
         False,
         False
      );
      Set_Sensitive (Item.View_Button, False);
      FCL_Handlers.Connect
      (  Item.New_Button,
         "clicked",
         New_File'Access,
         Item.all'Access
      );

      Add (Item.Directory_Frame, Item.Directory);
      Pack_Start (Item, Item.Directory_Frame);

      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-new-folder-label"),
         Style_Get (Item.Browser, "tab-new-folder-icon"),
         Item
      );
      Set (Item.Browser.Folder, Item);
      FCL_Handlers.Connect
      (  Get_Files_View
         (  Gtk_Directory_Browser_Record'Class
            (  Get_Directory_Browser
               (  Item.Directory
               ) .all
            ) 'Unchecked_Access
         ),
         "selection-changed",
         File_Changed'Access,
         Item.all'Access
      );
   end Initialize;

   procedure New_File
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             )  is
      View : File_View;
   begin
      Gtk_New
      (  Item      => View,
         Mode      => Get (Item.Encoding_Box),
         New_File  => True,
         Language  => Get_Language (Get_Default, "fcl"),
         Browser   => Item.Browser,
         File_Name => Build_Filename
                      (  Get_Directory (Item.Directory),
                         "new.fcl"
      )               );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("New_File")
         )  );
   end New_File;

   procedure Service
             (  Item : not null access Folder_From_FCL_Box_Record
             )  is
      use Ada.Text_IO;
      File : aliased Ada.Text_IO.File_Type;
   begin
      Open (File, In_File, To_String (Item.File_Name));
      if Item.Code = null then
         Item.Code := new Folder_Program (Item.all'Unchecked_Access);
      else
         Erase (Item.Code.Features);
      end if;
      declare
         Viewer : constant Indicator_Handle :=
                           Get_Indicator (Item.Indicator);
      begin
         Reset (Viewer);
         case Item.Encoding is
            when Latin1_Set =>
               declare
                  Source : Latin1_Source_File
                              (File'Access, Ptr (Viewer));
               begin
                  Compile (Item.Code.all, Source);
               end;
            when ASCII_Set | UTF8_Set =>
               declare
                  Source : UTF8_Source_File (File'Access, Ptr (Viewer));
               begin
                  Compile (Item.Code.all, Source);
               end;
         end case;
         Done (Viewer);
         Close (File);
      exception
         when Error : Abort_Error =>
            Close (File);
            raise Ada.IO_Exceptions.End_Error with
                  Exception_Message (Error);
         when others =>
            Close (File);
            raise;
      end;
   end Service;

   procedure Show_Location
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             )  is
      View : File_View;
   begin
      Gtk_New
      (  Item             => View,
         File_Name        => To_String (Item.File_Name),
         Mode             => Item.Encoding,
         New_File         => False,
         Language         => Get_Language (Get_Default, "fcl"),
         Line_From_No     => Item.Line_From_No,
         Line_To_No       => Item.Line_To_No,
         Position_From_No => Item.Position_From_No,
         Position_To_No   => Item.Position_To_No,
         Browser          => Item.Browser
      );
   end Show_Location;

   procedure View_File
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Folder_From_FCL_Box
             )  is
      File : constant UTF8_String := Get_Selection (Item.Directory);
      View : File_View;
   begin
      if (  File'Length > 0
         and then
            File_Test (File, File_Test_Exists)
         and then
            File_Test (File, File_Test_Is_Regular)
         )
      then
         Gtk_New
         (  Item      => View,
            File_Name => File,
            New_File  => False,
            Mode      => Get (Item.Encoding_Box),
            Language  => Get_Language (Get_Default, "fcl"),
            Browser   => Item.Browser
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("View_File")
         )  );
   end View_File;

end Gtk.Fuzzy_Catalogue.Folders;
