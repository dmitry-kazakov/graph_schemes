--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.Projects                Luebeck            --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Glib.Messages;                  use Glib.Messages;
with GLib.Properties;                use GLib.Properties;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Gtk.Wildcard_Directory_Browser; use Gtk.Wildcard_Directory_Browser;
with Name_Tables;                    use Name_Tables;
with ODBC.API;                       use ODBC.API;
with Persistent.Native_ODBC;         use Persistent.Native_ODBC;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;
with Persistent.SQLite;
with Persistent.Single_File;

package body Gtk.Fuzzy_Catalogue.Projects is

   SQLite_DB_Key : constant String := "sqlite database";
   FDB_DB_Key   : constant String := "single file database";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Projects." & Name;
   end Where;

   procedure Commit_DSN (Item : access Project_Box_Record) is
      Storage : Storage_Handle;
      Name    : constant UTF8_String :=
                   Gtk_Entry_Record'Class
                   (  Item.ODBC.Name_Entry.Get_Child.all
                   ) .Get_Text;
      Query   : Project_Query'Class renames
                   Project_Query'Class
                   (  Query_Handles.Ptr
                      (  Get_Query (Item.Browser.Cache)
                      ) .all
                   );
   begin
      if Name'Length = 0 then
         -- Wrong name
         Error
         (  Item,
            Style_Get (Item.Browser, "project-empty-dsn-error")
         );
         Set_Hint (Item.ODBC.Name_Hint, Item.Browser, Erroneous, True);
         Set_Hint (Item.ODBC.User_Hint,     Item.Browser, None, True);
         Set_Hint (Item.ODBC.Password_Hint, Item.Browser, None, True);
         return;
      end if;
      begin
         Check_Name (Name);
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "name-error")
            );
            Set_Hint
            (  Item.ODBC.Name_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            Set_Hint (Item.ODBC.User_Hint, Item.Browser, None, True);
            Set_Hint
            (  Item.ODBC.Password_Hint,
               Item.Browser,
               None,
               True
            );
            return;
      end;
      Query.Scheme := DSN_Scheme;
      Query.Storage :=
         Persistent.Native_ODBC.Create
         (  Name,
            Get_Text (Item.ODBC.User_Entry),
            Get_Text (Item.ODBC.Password_Entry)
         );
      Query.Name := To_Unbounded_String (Name);
      Query.User :=
         To_Unbounded_String (Get_Text (Item.ODBC.User_Entry));
      Query.Stored := Get_Active (Item.ODBC.Store_Password);
      Query.Password :=
         To_Unbounded_String (Get_Text (Item.ODBC.Password_Entry));
      --
      -- Calling Add_Storage  to  add  storage,  since  the  storage  is
      -- already  connected  and  this will resolve to Create that would
      -- just pass Item.Browser.Storage further.
      --
      Add_Storage (Get_Cache (Item.Browser), Storage);
      Delete (Item);
   exception
      when Ada.IO_Exceptions.Use_Error =>
         Error
         (  Item,
            Style_Get (Item.Browser, "project-credentials-error")
         );
         Set_Hint (Item.ODBC.Name_Hint, Item.Browser, Erroneous, True);
         Set_Hint (Item.ODBC.User_Hint, Item.Browser, Erroneous, True);
         Set_Hint
         (  Item.ODBC.Password_Hint,
            Item.Browser,
            Erroneous,
            True
         );
      when Reason : Ada.IO_Exceptions.Data_Error =>
         Error
         (  Item,
            (  Style_Get (Item.Browser, "project-open-error")
            &  Exception_Message (Reason)
         )  );
         Set_Hint (Item.ODBC.Name_Hint, Item.Browser, Erroneous, True);
         Set_Hint (Item.ODBC.User_Hint, Item.Browser, None, True);
         Set_Hint (Item.ODBC.Password_Hint, Item.Browser, None, True);
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
         Set_Hint (Item.ODBC.Name_Hint, Item.Browser, Erroneous, True);
         Set_Hint (Item.ODBC.User_Hint, Item.Browser, Erroneous, True);
         Set_Hint
         (  Item.ODBC.Password_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Commit_DSN;

   procedure Commit_FDB (Item : access Project_Box_Record) is
      Storage : Storage_Handle;
      Name    : constant UTF8_String := Get_Text (Item.FDB.Name_Entry);
      Query   : Project_Query'Class renames
                   Project_Query'Class
                   (  Query_Handles.Ptr
                      (  Get_Query (Item.Browser.Cache)
                      ) .all
                   );
      function Get_File return UTF8_String is
         File : constant UTF8_String :=
                         Get_Text (Item.FDB.File_Entry);
      begin
         if Is_Absolute (File) then
            return File;
         else
            return
               Build_Filename
               (  Get_Directory (Item.FDB.Directory),
                  File
               );
         end if;
      end Get_File;
   begin
      if Name'Length = 0 then
         -- Wrong name
         Error
         (  Item,
            Style_Get (Item.Browser, "project-empty-dsn-error")
         );
         Set_Hint
         (  Item.FDB.Name_Hint,
            Item.Browser,
            Erroneous,
            True
         );
         return;
      end if;
      begin
         Check_Name (Name);
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "name-error")
            );
            Set_Hint
            (  Item.FDB.Name_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
      end;
      declare
         File : constant UTF8_String := Get_File;
      begin
         if File'Length = 0 then
            -- Wrong name
            Error
            (  Item,
               Style_Get (Item.Browser, "project-empty-file-error")
            );
            Set_Hint
            (  Item.FDB.File_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
         end if;
         Query.Scheme := FDB_Scheme;
         Query.Name   := To_Unbounded_String (Name);
         Query.File   := To_Unbounded_String (File);
         Query.Storage :=
            Persistent.Single_File.Create
            (  File,
               not File_Test (File, File_Test_Exists)
            );
      end;
      --
      -- Calling Add_Storage  to  add  storage,  since  the  storage  is
      -- already  connected  and  this will resolve to Create that would
      -- just pass Item.Browser.Storage further.
      --
      Add_Storage (Get_Cache (Item.Browser), Storage);
      Delete (Item);
   exception
      when Ada.IO_Exceptions.Use_Error |
           Ada.IO_Exceptions.Status_Error =>
         Error
         (  Item,
            Style_Get (Item.Browser, "project-credentials-error")
         );
         Set_Hint
         (  Item.FDB.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
      when Reason : Ada.IO_Exceptions.Data_Error =>
         Error
         (  Item,
            (  Style_Get (Item.Browser, "project-open-error")
            &  Exception_Message (Reason)
         )  );
         Set_Hint
         (  Item.FDB.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
         Set_Hint
         (  Item.FDB.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Commit_FDB;

   procedure Commit_SQLite (Item : access Project_Box_Record) is
      Storage : Storage_Handle;
      Name    : constant UTF8_String :=
                   Get_Text (Item.SQLite.Name_Entry);
      Query   : Project_Query'Class renames
                   Project_Query'Class
                   (  Query_Handles.Ptr
                      (  Get_Query (Item.Browser.Cache)
                      ) .all
                   );
      function Get_File return UTF8_String is
         File : constant UTF8_String :=
                         Get_Text (Item.SQLite.File_Entry);
      begin
         if Is_Absolute (File) then
            return File;
         else
            return
               Build_Filename
               (  Get_Directory (Item.SQLite.Directory),
                  File
               );
         end if;
      end Get_File;
   begin
      if Name'Length = 0 then
         -- Wrong name
         Error
         (  Item,
            Style_Get (Item.Browser, "project-empty-dsn-error")
         );
         Set_Hint
         (  Item.SQLite.Name_Hint,
            Item.Browser,
            Erroneous,
            True
         );
         return;
      end if;
      begin
         Check_Name (Name);
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "name-error")
            );
            Set_Hint
            (  Item.SQLite.Name_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
      end;
      declare
         File : constant UTF8_String := Get_File;
      begin
         if File'Length = 0 then
            -- Wrong name
            Error
            (  Item,
               Style_Get (Item.Browser, "project-empty-file-error")
            );
            Set_Hint
            (  Item.SQLite.File_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
         end if;
         Query.Scheme  := SQLite_Scheme;
         Query.Storage := Persistent.SQLite.Create (File);
         Query.Name    := To_Unbounded_String (Name);
         Query.File    := To_Unbounded_String (File);
      end;
      --
      -- Calling Add_Storage  to  add  storage,  since  the  storage  is
      -- already  connected  and  this will resolve to Create that would
      -- just pass Item.Browser.Storage further.
      --
      Add_Storage (Get_Cache (Item.Browser), Storage);
      Delete (Item);
   exception
      when Ada.IO_Exceptions.Use_Error |
           Ada.IO_Exceptions.Status_Error =>
         Error
         (  Item,
            Style_Get (Item.Browser, "project-credentials-error")
         );
         Set_Hint
         (  Item.SQLite.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
      when Reason : Ada.IO_Exceptions.Data_Error =>
         Error
         (  Item,
            (  Style_Get (Item.Browser, "project-open-error")
            &  Exception_Message (Reason)
         )  );
         Set_Hint
         (  Item.SQLite.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
         Set_Hint
         (  Item.SQLite.File_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Commit_SQLite;

   procedure Commit (Item : not null access Project_Box_Record) is
   begin
      case Scheme_Type'Val (Get_Current_Page (Item.Pages)) is
         when DSN_Scheme =>
            Commit_DSN (Item);
         when FDB_Scheme =>
            Commit_FDB (Item);
         when SQLite_Scheme =>
            Commit_SQLite (Item);
      end case;
   end Commit;

   procedure Create
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      if Browser.Project.Is_Valid then
         Browser.Set_Item (Browser.Project.Get);
      else
         declare
            Item : Project_Box;
            procedure Add (DSN : DSN_Description) is
               Row : Gtk_Tree_Iter;
            begin
               Item.ODBC.DSN_List.Append (Row);
               Item.ODBC.DSN_List.Set (Row, 0, DSN.Name);
               Item.ODBC.DSN_List.Set (Row, 1, DSN.Description);
            end Add;
         begin
            Gtk_New (Item, Browser);
            begin
               Item.ODBC.DSN_List.Clear;
               Add (Get_First_DSN (Environment, Any_DSN));
               loop
                  Add (Get_Next_DSN (Environment));
               end loop;
            exception
               when Ada.IO_Exceptions.End_Error =>
                  null;
            end;
            Item.ODBC.Name_Entry.Set_Sensitive (True);
            Gtk_Entry_Record'Class
            (  Item.ODBC.Name_Entry.Get_Child.all
            ) .Set_Text ("");
            Item.ODBC.User_Entry.Set_Text ("");
            Item.ODBC.Password_Entry.Set_Text ("");
            Item.ODBC.Store_Password.Set_Active (False);

            Item.FDB.Name_Entry.Set_Sensitive (True);
            Item.FDB.Name_Entry.Set_Text ("");
            Item.FDB.File_Entry.Set_Sensitive (True);
            Item.FDB.File_Entry.Set_Text ("");

            Item.SQLite.Name_Entry.Set_Sensitive (True);
            Item.SQLite.Name_Entry.Set_Text ("");
            Item.SQLite.File_Entry.Set_Sensitive (True);
            Item.SQLite.File_Entry.Set_Text ("");
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Create")
         )  );
   end Create;

   procedure Create
             (  Query           : in out Project_Query;
                Scheme          : out Scheme_Type;
                Name            : out Unbounded_String;
                User            : out Unbounded_String;
                Password        : out Unbounded_String;
                Stored_Password : out Boolean;
                Storage         : out Storage_Handle
             )  is
   begin
      Storage := Query.Storage;
      if Is_Valid (Storage) then
         Scheme := Query.Scheme;
         case Scheme is
            when DSN_Scheme =>
               Name     := Query.Name;
               User     := Query.User;
               Password := Query.Password;
               Stored_Password := Query.Stored;
                  -- Erase the password string for security reasons
               for Index in 1..Length (Password) loop
                  Replace_Element (Query.Password, Index, ' ');
               end loop;
               Invalidate (Query.Storage);
            when FDB_Scheme =>
               Name     := Query.Name;
               User     := Query.File;
               Password := Null_Unbounded_String;
               Stored_Password := False;
               Invalidate (Query.Storage);
            when SQLite_Scheme =>
               Name     := Query.Name;
               User     := Query.File;
               Password := Null_Unbounded_String;
               Stored_Password := False;
               Invalidate (Query.Storage);
         end case;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Create")
         )  );
         raise;
   end Create;

   procedure Delete_DSN
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             )  is
      Path : constant Item_Path :=
                      Item_Path
                      (  Gtk_Entry_Record'Class
                         (  Item.ODBC.Name_Entry.Get_Child.all
                         ) .Get_Text
                      );
   begin
      Delete (Item);
      Delete (Get_Cache (Item.Browser), Path);
   exception
      when others =>
         null;
   end Delete_DSN;

   procedure Delete_FDB
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             )  is
      Path : constant Item_Path :=
                      Item_Path (Get_Text (Item.FDB.File_Entry));
   begin
      Delete (Item);
      Delete (Get_Cache (Item.Browser), Path);
   exception
      when others =>
         null;
   end Delete_FDB;

   procedure Delete_SQLite
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             )  is
      Path : constant Item_Path :=
                      Item_Path (Get_Text (Item.SQLite.File_Entry));
   begin
      Delete (Item);
      Delete (Get_Cache (Item.Browser), Path);
   exception
      when others =>
         null;
   end Delete_SQLite;

   procedure FDB_File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             )  is
      File : constant UTF8_String :=
                      Get_Selection (Item.FDB.Directory);
   begin
      if File'Length > 0 then
         Set_Text (Item.FDB.File_Entry, File);
         Set_Hint (Item.FDB.File_Hint, Item.Browser, Checked, True);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("FDB_File_Changed")
         )  );
   end FDB_File_Changed;

   procedure SQLite_File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Project_Box
             )  is
      File : constant UTF8_String :=
                      Get_Selection (Item.SQLite.Directory);
   begin
      if File'Length > 0 then
         Set_Text (Item.SQLite.File_Entry, File);
         Set_Hint (Item.SQLite.File_Hint, Item.Browser, Checked, True);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("SQLite_File_Changed")
         )  );
   end SQLite_File_Changed;

   procedure Get
             (  Query           : in out Project_Query;
                Scheme          : Scheme_Type;
                Name            : UTF8_String;
                User            : in out Unbounded_String;
                Password        : in out Unbounded_String;
                Stored_Password : in out Boolean;
                Storage         : out Storage_Handle
             )  is
   begin
      Query.Scheme := Scheme;
      case Scheme is
         when DSN_Scheme =>
            if Stored_Password then
               --
               -- The password is stored, so we just try to connect  and
               -- if that succeeds return the result to the caller.
               --
               begin
                  Storage :=
                     Persistent.Native_ODBC.Create
                     (  Name,
                        To_String (User),
                        To_String (Password)
                     );
                  Query.Storage  := Storage;
                  Query.Name     := To_Unbounded_String (Name);
                  Query.User     := User;
                  Query.Password := Password;
                  Query.Stored   := Stored_Password;
                  return;
               exception
                  when Ada.IO_Exceptions.Use_Error |
                       Ada.IO_Exceptions.Data_Error =>
                     null;
               end;
            end if;
         when FDB_Scheme =>
            begin
               Storage :=
                  Persistent.Single_File.Create (To_String (User));
               Query.Name     := To_Unbounded_String (Name);
               Query.User     := User;
               Query.Password := Null_Unbounded_String;
               Query.Stored   := False;
               return;
            exception
               when Ada.IO_Exceptions.Data_Error   |
                    Ada.IO_Exceptions.Status_Error |
                    Ada.IO_Exceptions.Use_Error =>
                  null;
            end;
         when SQLite_Scheme =>
            begin
               Storage := Persistent.SQLite.Create (To_String (User));
               Query.Name     := To_Unbounded_String (Name);
               Query.User     := User;
               Query.Password := Null_Unbounded_String;
               Query.Stored   := False;
               return;
            exception
               when Ada.IO_Exceptions.Data_Error   |
                    Ada.IO_Exceptions.Status_Error |
                    Ada.IO_Exceptions.Use_Error =>
                  null;
            end;
      end case;
      if not Is_Valid (Query.Browser.Project) then
         Create (Query.Browser, Query.Browser.all'Unchecked_Access);
      end if;
      declare
         Item : Project_Box_Record renames
                   Project_Box_Record (Query.Browser.Project.Get.all);
      begin
         case Scheme is
            when DSN_Scheme =>
               Gtk_Entry_Record'Class
               (  Item.ODBC.Name_Entry.Get_Child.all
               )  .Set_Text (Name);
               Item.ODBC.User_Entry.Set_Text (To_String (User));
               Item.ODBC.Password_Entry.Set_Text ("");
               Item.ODBC.Store_Password.Set_Active (Stored_Password);
               Item.Pages.Set_Current_Page (Scheme_Type'Pos (Scheme));
               if Item.ODBC.Delete = null then
                  Gtk_New (Item.ODBC.Delete);
                  Item.ODBC.Grid.Attach
                  (  Item.ODBC.Delete,
                     2, 3, 3, 4,
                     Xoptions => Shrink,
                     YOptions => Shrink
                  );
                  Handlers.Connect
                  (  Item.ODBC.Delete,
                     "clicked",
                     Delete_DSN'Access,
                     Item'Unchecked_Access
                  );
               end if;
            when FDB_Scheme =>
               Item.FDB.Name_Entry.Set_Text (Name);
               Item.FDB.File_Entry.Set_Text (To_String (User));
               Item.Pages.Set_Current_Page (Scheme_Type'Pos (Scheme));
               Item.FDB.Directory.Get_Directory_Browser.
                  Get_Tree_View.Set_Current_Directory
                  (  Item_Path (To_String (User))
                  );
               if Item.FDB.Delete = null then
                  Gtk_New (Item.FDB.Delete);
                  Item.FDB.Grid.Attach
                  (  Item.FDB.Delete,
                     2, 3, 3, 4,
                     Xoptions => Shrink,
                     YOptions => Shrink
                  );
                  Handlers.Connect
                  (  Item.FDB.Delete,
                     "clicked",
                     Delete_FDB'Access,
                     Item'Unchecked_Access
                  );
               end if;
            when SQLite_Scheme =>
               Item.SQLite.Name_Entry.Set_Text (Name);
               Item.SQLite.File_Entry.Set_Text (To_String (User));
               Item.Pages.Set_Current_Page (Scheme_Type'Pos (Scheme));
               Item.SQLite.Directory.Get_Directory_Browser.
                  Get_Tree_View.Set_Current_Directory
                  (  Item_Path (To_String (User))
                  );
               if Item.SQLite.Delete = null then
                  Gtk_New (Item.SQLite.Delete);
                  Item.SQLite.Grid.Attach
                  (  Item.SQLite.Delete,
                     2, 3, 3, 4,
                     Xoptions => Shrink,
                     YOptions => Shrink
                  );
                  Handlers.Connect
                  (  Item.SQLite.Delete,
                     "clicked",
                     Delete_SQLite'Access,
                     Item'Unchecked_Access
                  );
               end if;
         end case;
      end;
      Invalidate (Storage);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
   end Get;

   procedure Gtk_New
             (  Item    : out Project_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item := new Project_Box_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Projects.Initialize (Item);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item : not null access Project_Box_Record'Class
             )  is
      Label : Gtk_Label;
      Frame : Gtk_Frame;
      Column_Spacing : constant GUInt :=
                       Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                       Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "project box", True);
      Gtk_New (Item.Pages);
      Item.Pack_Start (Item.Pages);
   --
   -- DSN page
   --
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_None);
      Frame.Set_Border_Width (Column_Spacing);
      Item.Pages.Append_Page (Frame);
      Item.Pages.Set_Tab_Label_Text
      (  Frame,
         Style_Get (Item.Browser, "project-tab-dsn")
      );

      Gtk_New (Item.ODBC.Grid, 4, 3, False);
      Frame.Add (Item.ODBC.Grid);
      Item.ODBC.Grid.Set_Col_Spacings (Column_Spacing);
      Item.ODBC.Grid.Set_Row_Spacings (Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "project-name-label"));
      Item.ODBC.Grid.Attach (Label, 0, 1, 0, 1, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.ODBC.DSN_List, (GType_String, GType_String));
      Gtk_New_With_Model_And_Entry
      (  Item.ODBC.Name_Entry,
         To_Interface (Item.ODBC.DSN_List)
      );
      Item.ODBC.DSN_List.Unref;
      Item.ODBC.Name_Entry.Set_Entry_Text_Column (0);
      Item.ODBC.Name_Entry.Set_Sensitive (False);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.ODBC.Name_Hint);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.ODBC.Name_Hint, Item.Browser, None, True);
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "project-user-label"));
      Item.ODBC.Grid.Attach (Label, 0, 1, 1, 2, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.ODBC.User_Entry);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.User_Entry,
         1, 2, 1, 2,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.ODBC.User_Hint);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.User_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.ODBC.User_Hint, Item.Browser, None, True);
         -- Row 3
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "project-password-label")
      );
      Item.ODBC.Grid.Attach (Label, 0, 1, 2, 3, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.ODBC.Password_Entry);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.Password_Entry,
         1, 2, 2, 3,
         Yoptions => 0
      );
      Set_Property
      (  Item.ODBC.Password_Entry,
         Visibility_Property,
         False
      );

      Gtk_New_HBox (Item.ODBC.Password_Hint);
      Item.ODBC.Grid.Attach
      (  Item.ODBC.Password_Hint,
         2, 3,
         2, 3,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.ODBC.Password_Hint, Item.Browser, None, True);
         -- Row 4
      Gtk_New
      (  Item.ODBC.Store_Password,
         Style_Get (Item.Browser, "project-stored-password-label")
      );
      Item.ODBC.Grid.Attach
      (  Item.ODBC.Store_Password,
         0, 2, 3, 4,
         Xoptions => Fill,
         YOptions => 0
      );
   --
   -- SQLite page
   --
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_None);
      Frame.Set_Border_Width (Column_Spacing);
      Item.Pages.Append_Page (Frame);
      Item.Pages.Set_Tab_Label_Text
      (  Frame,
         Style_Get (Item.Browser, "project-tab-sqlite")
      );

      Gtk_New (Item.SQLite.Grid, 4, 3, False);
      Frame.Add (Item.SQLite.Grid);
      Item.SQLite.Grid.Set_Col_Spacings (Column_Spacing);
      Item.SQLite.Grid.Set_Row_Spacings (Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "project-name-label"));
      Item.SQLite.Grid.Attach (Label, 0, 1, 0, 1, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.SQLite.Name_Entry);
      Item.SQLite.Name_Entry.Set_Sensitive (False);
      Item.SQLite.Grid.Attach
      (  Item.SQLite.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.SQLite.Name_Hint);
      Item.SQLite.Grid.Attach
      (  Item.SQLite.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.SQLite.Name_Hint, Item.Browser, None, True);
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "project-file-label"));
      Item.SQLite.Grid.Attach (Label, 0, 1, 1, 2, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.SQLite.File_Entry);
      Item.SQLite.File_Entry.Set_Sensitive (False);
      Item.SQLite.Grid.Attach
      (  Item.SQLite.File_Entry,
         1, 2, 1, 2,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.SQLite.File_Hint);
      Item.SQLite.Grid.Attach
      (  Item.SQLite.File_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.SQLite.File_Hint, Item.Browser, None, True);
   --
   -- Single file page
   --
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_None);
      Frame.Set_Border_Width (Column_Spacing);
      Item.Pages.Append_Page (Frame);
      Item.Pages.Set_Tab_Label_Text
      (  Frame,
         Style_Get (Item.Browser, "project-tab-single-file")
      );

      Gtk_New (Item.FDB.Grid, 4, 3, False);
      Frame.Add (Item.FDB.Grid);
      Item.FDB.Grid.Set_Col_Spacings (Column_Spacing);
      Item.FDB.Grid.Set_Row_Spacings (Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "project-name-label"));
      Item.FDB.Grid.Attach (Label, 0, 1, 0, 1, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.FDB.Name_Entry);
      Item.FDB.Name_Entry.Set_Sensitive (False);
      Item.FDB.Grid.Attach
      (  Item.FDB.Name_Entry,
         1, 2, 0, 1,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.FDB.Name_Hint);
      Item.FDB.Grid.Attach
      (  Item.FDB.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.FDB.Name_Hint, Item.Browser, None, True);
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "project-file-label"));
      Item.FDB.Grid.Attach (Label, 0, 1, 1, 2, Fill, 0);
      Label.Set_Justify (Justify_Right);
      Label.Set_Alignment (1.0, 0.5);

      Gtk_New (Item.FDB.File_Entry);
      Item.FDB.File_Entry.Set_Sensitive (False);
      Item.FDB.Grid.Attach
      (  Item.FDB.File_Entry,
         1, 2, 1, 2,
         Yoptions => 0
      );

      Gtk_New_HBox (Item.FDB.File_Hint);
      Item.FDB.Grid.Attach
      (  Item.FDB.File_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.FDB.File_Hint, Item.Browser, None, True);
   --
   --
      Item.Show_All;
      Item.Browser.Add_Item
      (  Style_Get (Item.Browser, "tab-new-project-label"),
         Style_Get (Item.Browser, "tab-new-project-icon"),
         Item
      );
      Item.Browser.Project.Set (Item);
      Handlers.Connect
      (  Item.Pages,
         "switch_page",
         Page_Changed'Access,
         Item.all'Unchecked_Access
      );
   end Initialize;

   procedure Page_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Params : GValues;
                Item   : Project_Box
             )  is
      Frame : Gtk_Frame;
   begin
      case Get_UInt (Nth (Params, 2)) is
         when Scheme_Type'Pos (FDB_Scheme) =>
            if Item.FDB.Directory = null then
               Gtk_New (Frame);
               Set_Shadow_Type (Frame, Shadow_Etched_In);
               Attach
               (  Item.FDB.Grid,
                  Frame,
                  0, 3,
                  2, 3,
                  Xoptions => Fill or Expand,
                  Yoptions => Fill or Expand
               );
               Gtk_New
               (  Widget  => Item.FDB.Directory,
                  Browser => Item.Browser,
                  Key     => FDB_DB_Key
               );
               Set_Pattern
               (  Get_Directory_Browser (Item.FDB.Directory),
                  "*.fdb"
               );
               Set_Text
               (  Get_Filter_Entry (Item.FDB.Directory),
                  "*.fdb"
               );
               Add (Frame, Item.FDB.Directory);
               Show_All (Frame);
               Handlers.Connect
               (  Get_Files_View
                  (  Gtk_Directory_Browser_Record'Class
                     (  Get_Directory_Browser
                        (  Item.FDB.Directory
                        ) .all
                     ) 'Unchecked_Access
                  ),
                  "selection-changed",
                  FDB_File_Changed'Access,
                  Item.all'Access
               );
            end if;
         when Scheme_Type'Pos (SQLite_Scheme) =>
            if Item.SQLite.Directory = null then
               Gtk_New (Frame);
               Set_Shadow_Type (Frame, Shadow_Etched_In);
               Attach
               (  Item.SQLite.Grid,
                  Frame,
                  0, 3,
                  2, 3,
                  Xoptions => Fill or Expand,
                  Yoptions => Fill or Expand
               );
               Gtk_New
               (  Widget  => Item.SQLite.Directory,
                  Browser => Item.Browser,
                  Key     => SQLite_DB_Key
               );
               Set_Pattern
               (  Get_Directory_Browser (Item.SQLite.Directory),
                  "*.db"
               );
               Set_Text
               (  Get_Filter_Entry (Item.SQLite.Directory),
                  "*.db"
               );
               Add (Frame, Item.SQLite.Directory);
               Show_All (Frame);
               Handlers.Connect
               (  Get_Files_View
                  (  Gtk_Directory_Browser_Record'Class
                     (  Get_Directory_Browser
                        (  Item.SQLite.Directory
                        ) .all
                     ) 'Unchecked_Access
                  ),
                  "selection-changed",
                  SQLite_File_Changed'Access,
                  Item.all'Access
               );
            end if;
         when others =>
            null;
      end case;
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Page_Changed")
         )  );
   end Page_Changed;

   function Query
            (  Browser : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class
            )  return Query_Handles.Handle is
      Ptr : constant Abstract_Credentials_Query_Ptr :=
                     new Project_Query (Browser);
   begin
      return Query_Handles.Ref (Ptr);
   end Query;

end Gtk.Fuzzy_Catalogue.Projects;
