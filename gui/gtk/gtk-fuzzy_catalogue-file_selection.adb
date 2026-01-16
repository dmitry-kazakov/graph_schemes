--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.File_Selection          Luebeck            --
--  Implementation                                 Winter, 2009       --
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

with Ada.Directories;         use Ada.Directories;
with Glib.Messages;           use Glib.Messages;
with Glib.Properties;         use Glib.Properties;
with Gtk.Cell_Layout;         use Gtk.Cell_Layout;
with Gtk.Cell_Renderer;       use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;  use Gtk.Cell_Renderer_Text;
with Gtk.Missed;              use Gtk.Missed;
with Gtk.Separator;           use Gtk.Separator;
with Gtk.Tree_Model;          use Gtk.Tree_Model;
with Gtk.Widget.Styles;       use Gtk.Widget.Styles;

with Strings_Edit.Integers;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.File_Selection is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.File_Selection." & Name;
   end Where;

   function Bookmark_Name (No : Positive) return String is
   begin
      return "bookmark" & Strings_Edit.Integers.Image (No);
   end Bookmark_Name;

   function "/"
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return String is
      Value : GValue;
   begin
      Store.Get_Value (Row, 0, Value);
      return Result : constant String := Get_String (Value) do
         Unset (Value);
      end return;
   end "/";

   procedure Add_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      declare
         Directory : constant UTF8_String :=
                        UTF8_String
                        (  Get_Current_Directory
                           (  Get_Tree_View
                              (  Selection.Directories
                        )  )  );
         Count : Positive := 1;
         Iter  : Gtk_Tree_Iter := Get_Iter_First (Selection.List);
      begin
         while Iter /= Null_Iter loop
            if Selection.List / Iter = Directory then
               Set_Active_Iter (Selection.Bookmarks, Iter);
               return;
            end if;
            Count := Count + 1;
            Next (Selection.List, Iter);
         end loop;
         Append (Selection.List, Iter);
         Gtk.Missed.Set (Selection.List, Iter, 0, Directory);
         Store (Selection.Browser, Bookmark_Name (Count), Directory);
         Store (Selection.Browser, Bookmark_Name (Count + 1), "");
         Set_Active_Iter (Selection.Bookmarks, Iter);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Bookmark")
         )  );
   end Add_Bookmark;

   procedure Backward
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      if not Selection.Changing then
         Selection.Changing := True;
         begin
            Set_Current_Directory
            (  Get_Tree_View (Selection.Directories),
               Previous (Selection.Browser.Files)
            );
         exception
            when Constraint_Error =>
               null;
         end;
         Selection.Changing := False;
         Set_Button_States (Selection);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Backward")
         )  );
   end Backward;

   procedure Bookmark_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      Set_Button_States (Selection);
   end Bookmark_Changed;

   procedure Delete_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
      Iter : Gtk_Tree_Iter := Get_Active_Iter (Selection.Bookmarks);
      No   : Natural := Natural (Get_Active (Selection.Bookmarks) + 1);
   begin
      if Iter = Null_Iter or else No = 0 then
         return;
      end if;
      loop
         declare
            Directory : constant String :=
               Restore (Selection.Browser, Bookmark_Name (No + 1), "");
         begin
            Store (Selection.Browser, Bookmark_Name (No), Directory);
            exit when Directory'Length = 0;
         end;
         No := No + 1;
      end loop;
      Remove (Selection.List, Iter);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete_Bookmark")
         )  );
   end Delete_Bookmark;

   procedure Directory_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      if not Selection.Changing then
         Push
         (  Selection.Browser.Files,
            Get_Current_Directory
            (  Get_Tree_View
               (  Selection.Directories
         )  )  );
         Set_Button_States (Selection);
      end if;
   exception
      when Name_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Changed")
         )  );
   end Directory_Changed;

   procedure Filter_Changed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      Set_Pattern (Selection.Directories, Get_Text (Selection.Filter));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Filter_Changed")
         )  );
   end Filter_Changed;

   procedure Forward
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      if not Selection.Changing then
         Selection.Changing := True;
         begin
            Set_Current_Directory
            (  Get_Tree_View (Selection.Directories),
               Next (Selection.Browser.Files)
            );
         exception
            when Constraint_Error =>
               null;
         end;
         Selection.Changing := False;
         Set_Button_States (Selection);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Forward")
         )  );
   end Forward;

   function Get_Filter_Entry
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_Entry is
   begin
      return Widget.Filter;
   end Get_Filter_Entry;

   function Get_Directory
            (  Widget : not null access Gtk_File_Selection_Record
            )  return UTF8_String is
   begin
      return
         UTF8_String
         (  Get_Directory
            (  Get_Files_View (Widget.Directories)
         )  );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Directory")
         )  );
         return "";
   end Get_Directory;

   function Get_Directory_Browser
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_Wildcard_Directory_Browser is
   begin
      return Widget.Directories;
   end Get_Directory_Browser;

   function Get_Selection
            (  Widget : not null access Gtk_File_Selection_Record
            )  return UTF8_String is
      Files : Selection renames
                 Get_Selection (Get_Files_View (Widget.Directories));
   begin
      if (  Files'Length = 1
         and then
            (  Item_Type (Stock_Directory)
            /= Get_Type
               (  Get_Files_View (Widget.Directories),
                  Files (Files'First)
         )  )  )
      then
         return
            UTF8_String
            (  Get_Path
               (  Get_Files_View (Widget.Directories),
                  Files (Files'First)
            )  );
      else
         return "";
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Selection")
         )  );
         return "";
   end Get_Selection;

   function Get_Title_Box
            (  Widget : not null access Gtk_File_Selection_Record
            )  return Gtk_HBox is
   begin
      return Widget.Title_Box;
   end Get_Title_Box;

   procedure Go_To_Bookmark
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
      Iter : constant Gtk_Tree_Iter :=
                      Get_Active_Iter (Selection.Bookmarks);
   begin
      if Iter /= Null_Iter then
         declare
            Directory : constant UTF8_String := Selection.List / Iter;
         begin
            if Directory'Length > 0 then
               Set_Current_Directory
               (  Get_Tree_View (Selection.Directories),
                  Item_Path (Directory)
               );
            end if;
         exception
            when others =>
               null;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Go_To_Bookmark")
         )  );
   end Go_To_Bookmark;

   procedure Go_To_Parent
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      Set_Current_Directory
      (  Get_Tree_View (Selection.Directories),
         Get_Directory
         (  Get_Cache (Selection.Directories),
            Get_Current_Directory
            (  Get_Tree_View (Selection.Directories)
      )  )  );
   exception
      when Name_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Go_To_Parent")
         )  );
   end Go_To_Parent;

   procedure Gtk_New
             (  Widget  : out Gtk_File_Selection;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Key     : UTF8_String
             )  is
      Wait : Wait_Cursor (Browser);
   begin
      Widget := new Gtk_File_Selection_Record (Browser);
      begin
         Initialize (Widget, Key);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Home
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      Set_Current_Directory
      (  Get_Tree_View (Selection.Directories),
         Item_Path (Get_User_Special_Dir (User_Directory_Desktop))
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Home")
         )  );
   end Home;

   procedure Initialize
             (  Widget : not null access
                         Gtk_File_Selection_Record'Class;
                Key    : UTF8_String
             )  is
      Label          : Gtk_Label;
      Frame          : Gtk_Frame;
      Alignment      : Gtk_Alignment;
      Separator      : Gtk_Hseparator;
      Text           : Gtk_Cell_Renderer_Text;
      Button_Spacing : constant GUInt :=
                          Style_Get (Widget.Browser, "button-spacing");
   begin
      Initialize_VBox (Widget);
         -- Title box
      Gtk_New_HBox (Widget.Title_Box);
      Set_Spacing (Widget.Title_Box, GInt (Button_Spacing));
      Gtk_New (Widget.Home);
      Pack_Start (Widget.Title_Box, Widget.Home, False, False);
      Gtk_New (Widget.Backward);
      Pack_Start (Widget.Title_Box, Widget.Backward, False, False);
      Gtk_New (Widget.Forward);
      Pack_Start (Widget.Title_Box, Widget.Forward, False, False);
      Gtk_New (Widget.Refresh);
      Pack_Start (Widget.Title_Box, Widget.Refresh, False, False);
      Gtk_New (Widget.Parent);
      Pack_Start (Widget.Title_Box, Widget.Parent, False, False);
      Gtk_New (Label, Style_Get (Widget.Browser, "files-filter"));
      Pack_Start (Widget.Title_Box, Label, False, False);
      Gtk_New (Widget.Filter);
      Pack_Start (Widget.Title_Box, Widget.Filter, False, False);
      Gtk_New_Vseparator (Separator);
      Pack_Start (Widget.Title_Box, Separator, False, False);
      Gtk_New (Widget.Add);
      Pack_Start (Widget.Title_Box, Widget.Add, False, False);
      Gtk_New (Widget.Delete);
      Pack_Start (Widget.Title_Box, Widget.Delete, False, False);
      Gtk_New (Widget.Bookmarks);
      Set_Property (Widget.Bookmarks, Border_Width_Property, 0);
      Set_Border_Width (Widget.Bookmarks, 0);
      Set_Property (Widget.Bookmarks, Can_Focus_Property, False);
      Gtk_New (Text);
      Set_Property (Text, Ypad_Property, 0);
      Pack_Start (+Widget.Bookmarks, Text, False);
      Add_Attribute (+Widget.Bookmarks, Text, "text", 0);
      Gtk_New (Alignment, 0.0, 0.5, 1.0, 0.0);
      Add (Alignment, Widget.Bookmarks);
      Pack_Start (Widget.Title_Box, Alignment, True, True);
      Gtk_New (Widget.Switch_To);
      Pack_Start (Widget.Title_Box, Widget.Switch_To, False, False);

      Pack_Start (Widget, Widget.Title_Box, False, False);
         -- Directories
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      if Widget.Browser.Files.Store = null then
         Widget.Browser.Files.Store :=
            new Catalogue_Directory_Record (Widget.Browser);
         Initialize
         (  Widget.Browser.Files.Store,
            Cache_Expanded,
            Get_Tracing (Widget.Browser.Cache)
         );
      end if;
         -- Restoring bookmarks
      Gtk_New (Widget.List, (1 => GType_String));
      for Index in Positive'Range loop
         declare
            Directory : constant UTF8_String :=
               Restore (Widget.Browser, Bookmark_Name (Index), "");
            Iter : Gtk_Tree_Iter;
         begin
            exit when Directory'Length = 0;
            Append (Widget.List, Iter);
            Gtk.Missed.Set (Widget.List, Iter, 0, Directory);
         end;
      end loop;
      Set_Model (Widget.Bookmarks, To_Interface (Widget.List));
      Unref (Widget.List);

      Gtk_New
      (  Widget    => Widget.Directories,
         Tree_Size => (Width => 220, Height => 200),
         List_Size => (Width => 500, Height => 200),
         File =>
            Restore
            (  Widget  => Widget.Browser,
               Key     => Key,
               Default => Current_Directory
            ),
         Store   => Widget.Browser.Files.Store.all'Unchecked_Access,
         Tracing => Widget.Browser.Tracing
      );
      Set_Selection_Mode
      (  Get_Files_View (Widget.Directories),
         Selection_Single
      );
      Add (Frame, Widget.Directories);
      Pack_Start (Widget, Frame);

      Set_Button_States (Widget);

      File_Handlers.Connect
      (  Widget.Add,
         "clicked",
         Add_Bookmark'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Backward,
         "clicked",
         Backward'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Delete,
         "clicked",
         Delete_Bookmark'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Forward,
         "clicked",
         Forward'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Home,
         "clicked",
         Home'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Refresh,
         "clicked",
         Refreshed'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Switch_To,
         "clicked",
         Go_To_Bookmark'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Parent,
         "clicked",
         Go_To_Parent'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Filter,
         "changed",
         Filter_Changed'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Get_Files_View (Widget.Directories),
         "directory-changed",
         Directory_Changed'Access,
         Widget.all'Unchecked_Access
      );
      File_Handlers.Connect
      (  Widget.Bookmarks,
         "changed",
         Bookmark_Changed'Access,
         Widget.all'Unchecked_Access
      );
      begin
         Push
         (  Widget.Browser.Files,
            Get_Current_Directory (Get_Tree_View (Widget.Directories))
         );
      exception
         when Name_Error =>
            null;
      end;
      Set_Button_States (Widget);
   end Initialize;

   procedure Refreshed
             (  Widget    : access Gtk_Widget_Record'Class;
                Selection : Gtk_File_Selection
             )  is
   begin
      Changed
      (  Get_Cache (Selection.Directories),
         Get_Current_Directory (Get_Tree_View (Selection.Directories))
      );
   exception
      when Name_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Refreshed")
         )  );
   end Refreshed;

   procedure Set_Button_States
             (  Widget : not null access Gtk_File_Selection_Record
             )  is
      Current : constant GInt := Get_Active (Widget.Bookmarks);
   begin
      Set_Sensitive
      (  Widget.Forward,
         not On_Top (Widget.Browser.Files)
      );
      Set_Sensitive
      (  Widget.Backward,
         not At_Bottom (Widget.Browser.Files)
      );
      Set_Sensitive (Widget.Delete,    Current >= 0);
      Set_Sensitive (Widget.Switch_To, Current >= 0);
      begin
         declare
            Directory : constant Item_Path :=
               Get_Directory
               (  Get_Cache (Widget.Directories),
                  Get_Current_Directory
                  (  Get_Tree_View (Widget.Directories)
               )  );
         begin
            Set_Sensitive (Widget.Add,    Directory /= "");
            Set_Sensitive (Widget.Parent, True);
         end;
      exception
         when others =>
            Set_Sensitive (Widget.Add,    False);
            Set_Sensitive (Widget.Parent, False);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Button_States")
         )  );
   end Set_Button_States;

end Gtk.Fuzzy_Catalogue.File_Selection;
