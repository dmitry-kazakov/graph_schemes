--                                                                    --
--  package Gtk.Fuzzy_Catalogue     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2008       --
--                                                                    --
--                                Last revision :  08:56 08 Apr 2022  --
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

with Ada.Calendar.Formatting;      use Ada.Calendar.Formatting;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Deposit_Handles;              use Deposit_Handles;
with Fuzzy.Feature;                use Fuzzy.Feature;
with Fuzzy.Lecture;                use Fuzzy.Lecture;
with Fuzzy.Classifier;             use Fuzzy.Classifier;
with Gdk.RGBA;                     use Gdk.RGBA;
with Gdk.Types;                    use Gdk.Types;
with GLib.Messages;                use GLib.Messages;
with GLib.Properties;              use GLib.Properties;
with GLib.Properties.Creation;     use GLib.Properties.Creation;
with Gtk.Bin;                      use Gtk.Bin;
with Gtk.Button;                   use Gtk.Button;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Icon_Set;                 use Gtk.Icon_Set;
with Gtk.Image;                    use Gtk.Image;
with Gtk.Indicators.Progress;      use Gtk.Indicators.Progress;
with Gtk.Scrolled_Window;          use Gtk.Scrolled_Window;
with Gtk.Separator_Menu_Item;      use Gtk.Separator_Menu_Item;
with Gtk.Style_Context;            use Gtk.Style_Context;
with Gtk.Text_Buffer;              use Gtk.Text_Buffer;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Tree_View_Column;         use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;  use Gtk.Widget.Styles.Icon_Size;
with Indicator.Handle;             use Indicator.Handle;
with Name_Tables;                  use Name_Tables;
with Object.Archived;              use Object.Archived;
with Parsers;                      use Parsers;
with Persistent.Directory;         use Persistent.Directory;
with Strings_Edit;                 use Strings_Edit;
with System;                       use System;

with Gtk.Fuzzy_Catalogue.Classifier_Pane;
with Gtk.Fuzzy_Catalogue.Feature_Pane;
with Gtk.Fuzzy_Catalogue.Folders;
with Gtk.Fuzzy_Catalogue.Lecture_Pane;
with Gtk.Fuzzy_Catalogue.Projects;
with Gtk.Icon_Factory;
with GLib.Object.Checked_Destroy;
with Persistent.SQLite;

package body Gtk.Fuzzy_Catalogue is
   use Ada.Exceptions;
   use Fuzzy.Gtk_Icon_Factory;
   use GLib.Properties.Icon_Size;
   use Persistent;
   use Persistent.Handle;

   Button_Style_Updated : Boolean := False;
   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue." & Name;
   end Where;

   function "<" (Left, Right : Gtk_Widget) return Boolean is
   begin
      return
      (  Right /= null
      and then
         (  Left = null
         or else
            Left.all'Address < Right.all'Address
      )  );
   end "<";

   procedure Free is
      new Ada.Unchecked_Deallocation (Worker_Task, Worker_Task_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Item_Path, Item_Path_Ptr);

   procedure Aborted
             (  Item  : not null access Gtk_Item_Box_Record;
                Fault : Exception_Occurrence
             )  is
   begin
      if Exception_Identity (Fault) = Data_Error'Identity then
         Error (Item, Exception_Message (Fault));
      elsif Exception_Identity (Fault) = Syntax_Error'Identity then
         Error (Item, Exception_Message (Fault));
      elsif Exception_Identity (Fault) = End_Error'Identity then
         Error (Item, Style_Get (Item.Browser, "canceled-error"));
      else
         Error (Item, "Fault: " & Exception_Information (Fault));
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Aborted")
         )  );
   end Aborted;

   procedure Add_Close (Item : not null access Gtk_Item_Box_Record) is
   begin
      if Item.Button_Box /= null and then Item.Close = null then
         Gtk_New (Item.Close);
         Item.Button_Box.Pack_End (Item.Close, False, False);
         Box_Handlers.Connect
         (  Item.Close,
            "clicked",
            Close'Access,
            Item.all'Access
         );
         Show_All (Item.Close);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Close")
         )  );
   end Add_Close;

   procedure Add_Buttons (Item : not null access Gtk_Item_Box_Record) is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
   begin
      if Item.Button_Box = null then
         Gtk_New_HBox (Item.Button_Box);
         Set_Spacing (Item.Button_Box, GInt (Button_Spacing));
         Item.Pack_End (Item.Button_Box, False, False);
         Gtk_New (Item.Commit);
         Item.Button_Box.Pack_End (Item.Commit, False, False);
         Gtk_New_HSeparator (Item.Button_Separator);
         Item.Pack_End (Item.Button_Separator, False, False);
         Box_Handlers.Connect
         (  Item.Commit,
            "clicked",
            Commit'Access,
            Item.all'Access
         );
         Item.Button_Box.Show_All;
         Item.Button_Separator.Show_All;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Buttons")
         )  );
   end Add_Buttons;

   procedure Add_Item
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Title  : UTF8_String;
                Icon   : UTF8_String;
                Page   : not null access Gtk_Item_Box_Record'Class
             )  is
      Spacing   : constant GUInt :=
                           Style_Get (Widget, "tab-label-spacing");
      Tab_Label : Gtk_HBox;
   begin
      Page.Icon := To_Unbounded_String (Icon);
      if Widget.Item_Tabs = null then
         Gtk_New (Widget.Item_Tabs);
         Set_Property (Widget.Item_Tabs, Tab_Pos_Property, Pos_Bottom);
         Widget.Dock.Add (Widget.Item_Tabs);
         Widget.Item_Tabs.Show_All;
         Handlers.Connect
         (  Widget.Item_Tabs,
            "switch_page",
            Switch_Items_Page'Access,
            Widget.all'Access
         );
      end if;
      Gtk_New_HBox (Tab_Label);
      Tab_Label.Set_Spacing (GInt (Spacing));

      Gtk_New_HBox (Page.Image_Box);
      Tab_Label.Pack_Start (Page.Image_Box, False, False);
      Gtk_New
      (  Page.Image,
         Icon,
         Gtk_Icon_Size_Enum'Pos (Style_Get (Widget, "tab-icon-size"))
      );
      Page.Image_Box.Pack_Start (Page.Image, False, False);

      Gtk_New (Page.Text, Title);
      Tab_Label.Pack_Start (Page.Text);
      Gtk_New_HBox (Page.Tab_Buttons);
      Tab_Label.Pack_Start (Page.Tab_Buttons, Padding => 5);

      Widget.Item_Tabs.Append_Page (Page.Frame, Tab_Label);
      Tab_Label.Show_All;
      Page.Frame.Show_All;

      Widget.Item_Tabs.Set_Current_Page
      (  Widget.Item_Tabs.Page_Num (Page.Frame)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Item")
         )  );
   end Add_Item;

   procedure Add_Next (Item : not null access Gtk_Item_Box_Record) is
   begin
      if Item.Button_Box /= null and then Item.Next = null then
         Gtk_New (Item.Next);
         Item.Button_Box.Pack_End (Item.Next, False, False);
         Box_Handlers.Connect
         (  Item.Next,
            "clicked",
            Next'Access,
            Item.all'Access
         );
         Show_All (Item.Next);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Next")
         )  );
   end Add_Next;

   procedure Add_OK (Item : not null access Gtk_Item_Box_Record) is
   begin
      if Item.Button_Box /= null and then Item.Commit = null then
         Gtk_New (Item.Commit);
         Item.Button_Box.Pack_End (Item.Commit, False, False);
         Box_Handlers.Connect
         (  Item.Commit,
            "clicked",
            Commit'Access,
            Item.all'Access
         );
         Show_All (Item.Commit);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_OK")
         )  );
   end Add_OK;

   procedure Add_Previous
             (  Item : not null access Gtk_Item_Box_Record
             )  is
   begin
      if Item.Button_Box /= null and then Item.Previous = null then
         Gtk_New (Item.Previous);
         Item.Button_Box.Pack_Start (Item.Previous, False, False);
         Box_Handlers.Connect
         (  Item.Previous,
            "clicked",
            Previous'Access,
            Item.all'Access
         );
         Item.Previous.Show_All;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Previous")
         )  );
   end Add_Previous;

   function At_Bottom (Browsing : Files_Browsing_Ptr) return Boolean is
   begin
      return Browsing.Current = 1;
   end At_Bottom;

   function Cancel
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Boolean is
      Item : Gtk_Item_Box;
   begin
      if Widget.Item_Tabs /= null then
         for Index in reverse 1..Get_N_Pages (Widget.Item_Tabs) loop
            Item := Get_Item (Widget, Index);
            if Item /= null then
               Set_Current_Page (Widget.Item_Tabs, Index);
               if not Cancel (Item) then
                  return False;
               end if;
            end if;
         end loop;
      end if;
      return True;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Cancel")
         )  );
         return True;
   end Cancel;

   function Cancel
            (  Item : not null access Gtk_Item_Box_Record
            )  return Boolean is
   begin
      return True;
   end Cancel;

   procedure Cancel
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      if Cancel (Item) then
         if Item.Indicator /= null then
            Cancel (Get_Indicator (Item.Indicator));
         end if;
         Delete (Item);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Cancel")
         )  );
   end Cancel;

   procedure Check
             (  View       : not null access Folder_Picker_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      View.Browser.Cache.Browse (Path, Store, Object);
      if Is_Valid (Store) then
         if not Check (Constraint, Store) then
            raise Picking_Error;
         end if;
         if Object = Root_Directory then
            return;
         end if;
         declare
            Name : constant String := Get_Name (Store, Object);
         begin
            if View.Browser.Cache.Is_Directory
               (  Name,
                  Get_Class (Store, Name, Get_Parent (Store, Object))
               )
            then
               return;
            end if;
         end;
      end if;
      Store.Invalidate;
      Invalidate (Object);
   exception
      when Picking_Error =>
         Store.Invalidate;
         Invalidate (Object);
         raise;
      when others =>
         Store.Invalidate;
         Invalidate (Object);
   end Check;

   function Check_New_Name
            (  Item    : not null access Gtk_Item_Box_Record;
               Storage : Storage_Handle;
               Parent  : Deposit_Handle;
               Name    : UTF8_String;
               Unique  : Boolean := True
            )  return String is
   begin
      if Name'Length = 0 then -- Wrong name
         Error
         (  Item,
            Style_Get (Item.Browser, "empty-name-error")
         );
         return "";
      end if;
      begin
         Check_Name (Name);
      exception
         when Constraint_Error =>
            Error (Item, Style_Get (Item.Browser, "name-error"));
            return "";
      end;
      declare
         Canonical_Name : constant UTF8_String :=
                          Name_Maps.Canonize (Name);
      begin
         if Unique and then Is_In (Storage, Canonical_Name, Parent) then
            Error_Duplicated (Item, Storage, Parent, Canonical_Name);
         else
            return Canonical_Name;
         end if;
      exception
         when Fault : Data_Error =>
            Error (Item, Exception_Message (Fault));
         when Fault : others =>
            Error (Item, Exception_Information (Fault));
      end;
      return "";
   exception
      when Data_Error =>
         Error (Item, Style_Get (Item.Browser, "name-error"));
         return "";
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Check_New_Name")
         )  );
         return "";
   end Check_New_Name;

   procedure Clean (Item : not null access Gtk_Item_Box_Record) is
   begin
      if Item.Error_Label /= null then
         Item.Remove (Item.Error_Box);
         Item.Remove (Item.Separator);
         Item.Error_Label := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Clean")
         )  );
   end Clean;

   procedure Close (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Close;

   procedure Commit (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Commit;

   procedure Commit
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      Commit (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   procedure Completed (Item : not null access Gtk_Item_Box_Record) is
   begin
      Delete (Item);
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

   function Complete (Item : Gtk_Item_Box) return Boolean is
   begin
      Completed (Item);
      Unref (Item);
      return False;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Completion fault: "
            &  Exception_Information (Error)
            &  Where ("Complete")
         )  );
         Unref (Item);
         return False;
   end Complete;

   procedure Close
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      Close (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Close")
         )  );
   end Close;

   function Create_Indicator
            (  Item : not null access Gtk_Item_Box_Record
            )  return Gtk_Indicator is
      Progress : Gtk_Progress;
      Button   : Abort_Buttons.Gtk_Style_Button;
   begin
      Gtk_New (Button);
      Gtk_New
      (  Widget          => Progress,
         Button          => Button,
         Button_Position => (1, 2, 0, 1),
         Size            => (2, 1),
         Spacing =>
            (  Width  => Style_Get (Item.Browser, "row-spacing"),
               Height => Style_Get (Item.Browser, "column-spacing")
      )     );
      return Progress.all'Unchecked_Access;
   end Create_Indicator;

   function Current_Item
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Item_Box is
   begin
      if (  Widget.Item_Tabs /= null
         and then
            Get_N_Pages (Widget.Item_Tabs) > 0
         )
      then
         return
            Get_Item (Widget, Get_Current_Page (Widget.Item_Tabs));
      else
         return null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Current_Item")
         )  );
         return null;
   end Current_Item;

   procedure Delete (Item : not null access Gtk_Item_Box_Record) is
      Window  : constant Gtk_Window := Item.Window;
      Browser : Gtk_Fuzzy_Catalogue_Record'Class renames
                   Item.Browser.all;
   begin
      Item.Commit := null;
      if (  Browser.Item_Tabs /= null
         and then
            Is_In (Browser.Item_Tabs, Item.Frame)
         )
      then
         Remove (Browser.Item_Tabs, Item.Frame);
         if Browser.Item_Tabs.Get_N_Pages = 0 then
            Browser.Dock.Remove (Browser.Item_Tabs);
            Browser.Item_Tabs := null;
         end if;
      end if;
      if Window /= null then
         GLib.Object.Checked_Destroy (Window);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete")
         )  );
   end Delete;

   procedure Delete
             (  Button  : not null access Gtk_Widget_Record'Class;
                View    : Panel;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Selected : constant Selection := View.List.Get_Selection;
   begin
      if Selected'Length > 0 then
         declare
            Title     : constant String :=
                           Style_Get (Browser, "delete-query-title");
            Icon      : constant String :=
                           Style_Get (Browser, "query-icon");
            Size      : constant Gtk_Icon_Size_Enum :=
                           Style_Get (Browser, "query-icon-size");
            Path      : constant Item_Path := Get_Directory (View.List);
            Message   : Unbounded_String;
            Delimiter : Character := ' ';
            Result    : Gtk_Response_Type;
         begin
            Append
            (  Message,
               Style_Get (Browser, "delete-name-query")
            );
            for Index in Selected'Range loop
               Append (Message, Delimiter);
               Append
               (  Message,
                  UTF8_String (Get_Name (View.List, Selected (Index)))
               );
               Delimiter := ',';
            end loop;
            Result :=
               Query
               (  Browser,
                  Title,
                  Icon,
                  Size,
                  (  To_String (Message)
                  &  Style_Get (Browser, "folder-delete-from-query")
                  &  UTF8_String (Path)
              )  );
            if Result = Gtk_Response_OK then
               for Index in reverse Selected'Range loop
                  Browser.Cache.Delete
                  (  Get_Path
                     (  Browser.Cache,
                        Path,
                        Get_Name (View.List, Selected (Index))
                  )  );
               end loop;
               Set_Sensitive (Button, False);
            end if;
         end;
      end if;
   exception
      when Reason : others =>
         Fault (Browser, Exception_Message (Reason));
   end Delete;

   function Destroy_Event
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Item   : Gtk_Item_Box
            )  return Boolean is
   begin
      return Cancel (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy_Event")
         )  );
         return True;
   end Destroy_Event;

   function Delete_Floating
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Item   : Gtk_Item_Box
            )  return Boolean is
   begin
      if Item.Window /= null then
         Ref (Item.Frame);
         Remove (Item.Window, Item.Frame);
         Add_Item
         (  Item.Browser,
            Get_Title (Item.Window),
            To_String (Item.Icon),
            Item
         );
         Item.Window := null;
         Item.Frame.Unref;
      end if;
      return False;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete_Floating")
         )  );
         return False;
   end Delete_Floating;

   procedure Destroy
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Files_Browsing,
                Files_Browsing_Ptr
             );
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Move_Data,
                Move_Data_Ptr
             );
   begin
      if Browser.Item_Tabs /= null then
         Erase (Browser.Item_Tabs);
      end if;
      Browser.Dock.Unref;
      if Browser.Directories /= null then
         Browser.Directories.Unref;
      end if;
      if Browser.Features /= null then
         Browser.Features.Ref_Sink;
         Browser.Features.Unref;
      end if;
      if Browser.Classifiers /= null then
         Browser.Classifiers.Ref_Sink;
         Browser.Classifiers.Unref;
      end if;
      if Browser.Lectures /= null then
         Browser.Lectures.Ref_Sink;
         Browser.Lectures.Unref;
      end if;
      while not Is_Empty (Browser.Floating) loop
         GLib.Object.Checked_Destroy (Get (Browser.Floating, 1));
      end loop;
      if Browser.Files /= null then
         if Browser.Files.Store /= null then
            Browser.Files.Store.Unref;
         end if;
         Free (Browser.Files);
      end if;
      Free (Browser.Renaming);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy Gtk_Fuzzy_Catalogue")
         )  );
   end Destroy;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      Finalize (Item);
      if Item.Cancel /= null then
         Item.Cancel.Unref;
      end if;
      if Item.Undock /= null then
         Item.Undock.Unref;
      end if;
      Free (Item.Path);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy Gtk_Item_Box")
         )  );
   end Destroy;

   procedure Destroy_Floating
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      Remove (Browser.Floating, Widget.all'Unchecked_Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy_Floating")
         )  );
   end Destroy_Floating;

   procedure Directory_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             )  is
   begin
      Root_Changed (View);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Directory_Changed")
         )  );
   end Directory_Changed;

   procedure Do_Abort (Item : in out Gtk_Item_Box) is
   begin
      if Item.Indicator /= null then
         Item.Remove (Item.Indicator);
         Item.Indicator := null;
      end if;
      Free (Item.Worker);
      Clean (Item);
      Aborted (Item, Item.Error.all);
      if Item.Commit /= null then
         Item.Commit.Set_Sensitive (True);
      end if;
      Unref (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Abort")
         )  );
         Unref (Item);
   end Do_Abort;

   procedure Do_Complete (Item : in out Gtk_Item_Box) is
   begin
      if Item.Indicator /= null then
         Item.Remove (Item.Indicator);
         Item.Indicator := null;
      end if;
      Free (Item.Worker);
      Clean (Item);
      Completed (Item);
      if Item.Commit /= null then
         Item.Commit.Set_Sensitive (True);
      end if;
      Unref (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Do_Complete")
         )  );
         Unref (Item);
   end Do_Complete;

   function Get_BG
            (  Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Color is
      Result : Gdk_RGBA;
   begin
      Get_Style_Context (Widget).Get_Background_Color
      (  Gtk_State_Flag_Normal,
         Result
      );
      return From_RGBA (Result);
   end Get_BG;

   procedure Error
             (  Item : not null access Gtk_Item_Box_Record;
                Text : UTF8_String
             )  is
      use Gtk.Image;
      Icon    : Gtk_Image;
      Spacing : constant GUInt :=
                         Style_Get (Item.Browser, "column-spacing");
   begin
      Clean (Item);
      Gtk_New_HSeparator (Item.Separator);
      Item.Pack_Start (Item.Separator, False, False);
      Gtk_New_HBox (Item.Error_Box);
      Set_Spacing (Item.Error_Box, GInt (Spacing));
      Item.Pack_Start (Item.Error_Box, False, False);
      Gtk_New
      (  Icon,
         Style_Get (Item.Browser, "error-icon"),
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Item.Browser, "error-icon-size")
      )  );
      Item.Error_Box.Pack_Start (Icon, False, False);
      Gtk_New (Item.Error_Label);
      Item.Error_Label.Set_Editable (False);
      Item.Error_Label.Set_Wrap_Mode (Wrap_Word);
      Item.Error_Box.Pack_Start (Item.Error_Label);
      Item.Error_Label.Modify_Base
      (  State_Normal,
         Get_BG (Item.Browser)
      );
      Item.Error_Label.Get_Buffer.Set_Text (Text);
      Item.Show_All;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Error")
         )  );
   end Error;

   procedure Error_Duplicated
             (  Item    : not null access Gtk_Item_Box_Record;
                Storage : Storage_Handle;
                Parent  : Deposit_Handle;
                Name    : UTF8_String
             )  is
      Button : Show_Location_Buttons.Gtk_Style_Button;
      Box    : Gtk_VBox;
   begin
      Error
      (  Item,
         (  Style_Get (Item.Browser, "duplicated-error-begin")
         &  Name
         &  Style_Get (Item.Browser, "duplicated-error-end")
      )  );
      Free (Item.Path);
      Item.Path :=
         new Item_Path'
             (  Get_Path
                (  Name      => Item_Name (Name),
                   Store     => Gtk_Abstract_Directory_Record'Class
                                (  Item.Browser.Cache.all
                                ) 'Access,
                   Directory => Get_Folder
                                (  Item.Browser,
                                   Storage,
                                   Parent
             )  )               );
      Gtk_New_VBox (Box);
      Item.Error_Box.Pack_Start (Box, False, False);
      Gtk_New (Button);
      Box.Pack_Start (Button, False, False);
      Box.Show_All;
      Box_Handlers.Connect
      (  Button,
         "clicked",
         Show_Object'Access,
         Item.all'Unchecked_Access
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Browsing fault: "
            &  Exception_Information (Error)
            &  Where ("Error_Duplicated")
         )  );
   end Error_Duplicated;

   procedure Fault
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Text   : UTF8_String
             )  is
      Item   : constant Gtk_Item_Box := Current_Item (Widget);
      Result : Gtk_Response_Type;
   begin
      if Item = null then
         Result :=
            Query
            (  Widget     => Widget,
               Title      => Style_Get (Widget, "error-title"),
               Message    => Text,
               Icon_Stock => Style_Get (Widget, "error-icon"),
               Icon_Size  => Style_Get (Widget, "error-icon-size"),
               Cancel     => False
            );
      else
         Error (Item, Text);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Fault")
         )  );
   end Fault;

   procedure Finalize (Item : not null access Gtk_Item_Box_Record) is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Exception_Occurrence,
                Exception_Occurrence_Ptr
             );
   begin
      Free (Item.Constraint);
      Free (Item.Error);
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

   function Filter
            (  Widget    : not null access
                           Gtk_Fuzzy_Objects_View_Record;
               Directory : Boolean;
               Name      : Item_Name;
               Kind      : Item_Type
            )  return Boolean is
   begin
      if Directory then
         return True;
      else
         declare
            Kind_Of : constant String := String (Kind);
         begin
            return
            (  0 /= (Widget.Mode and Full_List)
            or else
               (  0 /= (Widget.Mode and Features_List)
               and then
                  Is_Prefix (Feature_Class, Kind_Of)
               )
            or else
               (  0 /= (Widget.Mode and Sets_List)
               and then
                  Is_Prefix (Lecture_Class, Kind_Of)
               )
            or else
               (  0 /= (Widget.Mode and Classifiers_List)
               and then
                  Is_Prefix (Classifier_Class, Kind_Of)
            )  );
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Filter")
         )  );
         return False;
   end Filter;

   procedure Finalize (Set : in out Move_Set) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Item_Path, Path_Ptr);
   begin
      Free (Set.From);
      Free (Set.To);
      for Index in Set.List'Range loop
         Free (Set.List (Index));
      end loop;
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

   procedure Folder_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Browser   : Gtk_Fuzzy_Catalogue
             )  is
   begin
      if Count_Selected_Rows (Get_Selection (Browser.Tree)) = 1 then
         if Browser.Buttons /= null then
            Browser.Delete_Folder.Set_Sensitive (True);
            Browser.New_Folder.Set_Sensitive    (True);
            Browser.New_From_FCL.Set_Sensitive  (True);
         end if;
      else
         if Browser.Buttons /= null then
            Browser.Delete_Folder.Set_Sensitive (False);
            Browser.New_Folder.Set_Sensitive    (False);
            Browser.New_From_FCL.Set_Sensitive  (False);
         end if;
         if Browser.Folder.Is_Valid then
            Delete (Browser.Folder.Get);
         end if;
      end if;
      if Browser.Features /= null then
         Browser.Features.Creation_Time.Set_Text ("");
      end if;
      if Browser.Lectures /= null then
         Browser.Lectures.Creation_Time.Set_Text ("");
      end if;
      if Browser.Classifiers /= null then
         Browser.Classifiers.Creation_Time.Set_Text ("");
      end if;
      State_Changed (Browser.Directories);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Folder_Changed")
         )  );
   end Folder_Changed;

   function Get
            (  Store  : Gtk_Tree_Model;
               Row    : Gtk_Tree_Iter;
               Column : GInt := 1
            )  return String is
      Value : GValue;
   begin
      Get_Value (Store, Row, Column, Value);
      return Result : constant String := Get_String (Value) do
         Unset (Value);
      end return;
   end Get;

   function Get
            (  View       : not null access Folder_Picker_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is
      Store  : Storage_Handle;
      Object : Deposit_Handle;
      Result : constant Item_Path :=
                  Get_Current_Directory (View.Browser.Tree);
   begin
      Check
      (  View,
         Result,
         Constraint,
         Store,
         Object
      );
      return Result;
   exception
      when others =>
         return "";
   end Get;

   procedure Get
             (  View       : not null access Folder_Picker_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      Check
      (  View,
         View.Browser.Tree.Get_Current_Directory,
         Constraint,
         Store,
         Object
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
   end Get;

   function Get_Cache
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Persistent_Directory is
   begin
      return Widget.Cache;
   end Get_Cache;

   function Get_Classifier
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Classifier_Handle is
      Classifier : Classifier_Handle;
   begin
      if Widget.Classifiers /= null then
         declare
            use Gtk.Fuzzy_Catalogue.Classifier_Pane;
            View : constant Classifier_Panel :=
                            Classifier_Panel (Widget.Classifiers);
         begin
            Classifier := View.Get_Current (View.List.Get_Selection);
         end;
      end if;
      return Classifier;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Classifier")
         )  );
         return Classifier;
   end Get_Classifier;

   function Get_Classifier
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Classifier_Handle is
      Result : constant Deposit_Handle := Get_Object (Path, View, Hint);
      No_Classifier : Classifier_Handle;
   begin
      if Result.Is_Valid then
         return To_Classifier_Handle (Result);
      else
         return No_Classifier;
      end if;
   exception
      when Reason : others =>
         if Hint /= null then
            Error (View, Exception_Message (Reason));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Classifier;
   end Get_Classifier;

   function Get_Constraint
            (  Widget : not null access Gtk_Item_Box_Record
            )  return Picker_Constraint is
   begin
      return Widget.Constraint;
   end Get_Constraint;

   function Get_Current
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Panel is
   begin
      return Widget.Current;
   end Get_Current;

   function Get_Feature
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Feature_Handle is
      Result : constant Deposit_Handle := Get_Object (Path, View, Hint);
   begin
      if Result.Is_Valid then
         return To_Feature_Handle (Result);
      else
         return No_Feature;
      end if;
   exception
      when Reason : others =>
         if Hint /= null then
            Error (View, Exception_Message (Reason));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Feature;
   end Get_Feature;

   function Get_Folder
            (  Widget  : not null access Gtk_Fuzzy_Catalogue_Record;
               Storage : Storage_Handle;
               Folder  : Deposit_Handle
            )  return Item_Path is
   begin
      if Is_Valid (Folder) then
         return Get_Path (Widget.Cache, Storage, Folder);
      else
         return Item_Path (Get_DSN (Widget.Cache, Storage));
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Folder")
         )  );
         return "";
   end Get_Folder;

   function Get_Icon
            (  Widget       : not null access
                              Gtk_Fuzzy_Objects_View_Record;
               Name         : Item_Name;
               Kind         : Item_Type;
               Directory    : Boolean;
               Has_Children : Boolean
            )  return Icon_Data is
   begin
      if Kind'Length = 0 then
         return (Stock_ID, 0, "");
      elsif Directory then
         declare
            This : constant String := String (Kind);
         begin
            if This = Persistent.Directory.Directory_Class then
               return
               (  Stock_ID,
                  Stock_Directory'Length,
                  Stock_Directory
               );
            else
               return
               (  Stock_ID,
                  This'Length,
                  This
               );
            end if;
         end;
      else
         declare
            This : constant String := Get_Feature_Icon (String (Kind));
         begin
            return
            (  Stock_ID,
               This'Length,
               This
            );
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Icon")
         )  );
         return (Stock_ID, 0, "");
   end Get_Icon;

   function Get_Item
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
               No     : GInt
            )  return Gtk_Item_Box is
   begin
      if Widget.Item_Tabs = null then
         return null;
      end if;
      declare
         Page : constant Gtk_Widget :=
                         Get_Nth_Page (Widget.Item_Tabs, No);
      begin
         if (  Page /= null
            and then
               Page.all in Gtk_Bin_Record'Class
            )
         then
            declare
               Child : constant Gtk_Widget :=
                  Get_Child (Gtk_Bin_Record'Class (Page.all)'Access);
            begin
               if (  Child /= null
                  and then
                     Child.all in Gtk_Item_Box_Record'Class
                  )
               then
                  return Gtk_Item_Box_Record (Child.all)'Access;
               end if;
            end;
         end if;
      end;
      return null;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Item")
         )  );
         return null;
   end Get_Item;

   function Get_Lecture
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )   return Lecture_Handle is
      Store  : Storage_Handle;
      Lesson : Lecture_Handle;
   begin
      if Widget.Lectures /= null then
         declare
            use Gtk.Fuzzy_Catalogue.Lecture_Pane;
            View : constant Lecture_Panel :=
                            Lecture_Panel (Widget.Lectures);
         begin
            View.Get_Current
            (  View.List.Get_Selection,
               Store,
               Lesson
            );
         end;
      end if;
      return Lesson;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Lecture")
         )  );
         return Lesson;
   end Get_Lecture;

   function Get_Lecture
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Lecture_Handle is
      Result : constant Deposit_Handle := Get_Object (Path, View, Hint);
      No_Lesson : Lecture_Handle;
   begin
      if Result.Is_Valid then
         return To_Lecture_Handle (Result);
      else
         return No_Lesson;
      end if;
   exception
      when Reason : others =>
         if Hint /= null then
            Error (View, Exception_Message (Reason));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Lesson;
   end Get_Lecture;

   function Get_Object
            (  Path : Item_Path;
               View : not null access Gtk_Item_Box_Record;
               Hint : Gtk_Box := null
            )  return Deposit_Handle is
      No_Object : Deposit_Handle;
   begin
      if Path'Length = 0 then
         if Hint /= null then
            -- Wrong name
            Error
            (  View,
               Style_Get (View.Browser, "empty-name-error")
            );
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
      end if;
      declare
         Store  : Storage_Handle;
         Object : Deposit_Handle;
      begin
         Browse
         (  Store   => Get_Cache (View.Browser),
            Path    => Path,
            Storage => Store,
            Object  => Object,
            Partial => False
         );
         if Hint /= null then
            Set_Hint (Hint, View.Browser, Checked, True);
         end if;
         return Object;
      end;
   exception
      when Data_Error =>
         if Hint /= null then
            Error
            (  View,
               Style_Get (View.Browser, "storage-browse-error")
            );
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
      when End_Error =>
         if Hint /= null then
            Error
            (  View,
               (  Style_Get (View.Browser, "no-object-error-begin")
               &  UTF8_String (Path)
               &  Style_Get (View.Browser, "no-object-error-end")
            )  );
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
      when Name_Error =>
         if Hint /= null then
            Error (View, Style_Get (View.Browser, "name-error"));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
      when Use_Error =>
         if Hint /= null then
            Error (View, Style_Get (View.Browser, "unsupported-error"));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
      when Reason : others =>
         if Hint /= null then
            Error (View, Exception_Message (Reason));
            Set_Hint (Hint, View.Browser, Erroneous, True);
         end if;
         return No_Object;
   end Get_Object;

   function Get_Parent
            (  View : not null access Panel_Record
            )  return Gtk_Widget is
   begin
      return View.List.Browser.all'Unchecked_Access;
   end Get_Parent;

   function Get_Parent
            (  View : not null access Folder_Picker_Record
            )  return Gtk_Widget is
   begin
      return View.Browser.all'Unchecked_Access;
   end Get_Parent;

   function Get_Path
            (  Picker : not null access Folder_Picker_Record;
               Object : Deposit_Handle
            )  return Item_Path is
   begin
      return
         Get_Path
         (  Picker.Browser.Cache,
            Picker.Browser.Tree.Get_Current_Storage,
            Object
         );
   exception
      when others =>
         return "";
   end Get_Path;

   procedure Get_Folder
             (  Widget : not null access Folder_Selection_Record;
                Store  : out Storage_Handle;
                Folder : out Deposit_Handle
             )  is
      Path : constant Item_Path := Get_Path (Widget);
   begin
      Browse
      (  Store   => Get_Cache (Widget.Item.Browser),
         Path    => Path,
         Storage => Store,
         Object  => Folder,
         Partial => False
      );
      if not Is_Valid (Store) then
         Error
         (  Widget.Item,
            Style_Get (Widget.Item.Browser, "storage-browse-error")
         );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Folder.Invalidate;
      elsif not Check (Get_Constraint (Widget), Store) then
         Error
         (  Widget.Item,
            Style_Get (Widget.Item.Browser, "mixed-storage-error")
         );
         Widget.Set_Hint (Widget.Item.Browser, Conflicting, True);
         Store.Invalidate;
         Folder.Invalidate;
      elsif Is_Valid (Folder) and then not Is_Directory (Folder) then
         Error
         (  Widget.Item,
            Style_Get (Widget.Item.Browser, "folder-object-error")
         );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Store.Invalidate;
         Folder.Invalidate;
      end if;
   exception
      when End_Error =>
         Error
         (  Widget.Item,
            (  Style_Get (Widget.Item.Browser, "no-object-error-begin")
            &  String (Path)
            &  Style_Get (Widget.Item.Browser, "no-object-error-end")
         )  );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Store.Invalidate;
         Folder.Invalidate;
      when Name_Error =>
         Error
         (  Widget.Item,
            Style_Get (Widget.Item.Browser, "folder-name-error")
         );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Store.Invalidate;
         Folder.Invalidate;
      when Reason : Data_Error =>
         Error
         (  Widget.Item,
            (  Style_Get
               (  Widget.Item.Browser,
                  "inconsistent-storage-error"
               )
            &  Exception_Message (Reason)
         )  );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Store.Invalidate;
         Folder.Invalidate;
      when Use_Error =>
         Error
         (  Widget.Item,
            Style_Get (Widget.Item.Browser, "unsupported-error")
         );
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Store.Invalidate;
         Folder.Invalidate;
      when Reason : others =>
         Error (Widget.Item, Exception_Message (Reason));
         Widget.Set_Hint (Widget.Item.Browser, Erroneous, True);
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Reason)
            &  Where ("Get_Folder")
         )  );
         Store.Invalidate;
         Folder.Invalidate;
   end Get_Folder;

   function Get_Path
            (  Picker : not null access Panel_Record;
               Object : Deposit_Handle
            )  return Item_Path is
   begin
      return
         Get_Path
         (  Picker.List.Browser.Cache,
            Picker.List.Browser.Tree.Get_Current_Storage,
            Object
         );
   exception
      when others =>
         return "";
   end Get_Path;

   function Get_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Catalogue_Record
            )  return Gtk_Persistent_Storage_Tree_View is
   begin
      return Widget.Tree.all'Unchecked_Access;
   end Get_Tree_View;

   function Get_Title
            (  Item : not null access Gtk_Item_Box_Record
            )  return UTF8_String is
   begin
      return Get_Text (Item.Text);
   end Get_Title;

   procedure Install_Class_Properties is separate;

   function Get_Type (Vertical : Boolean) return GType is
   begin
      if Vertical then
         if Initialize_Class_Record
            (  Ancestor     => Gtk.Paned.Get_Type_VPaned,
               Class_Record => Class_Record'Access,
               Type_Name    => Class_Name
            )
         then
            Install_Class_Properties;
         end if;
      else
         if Initialize_Class_Record
            (  Ancestor     => Gtk.Paned.Get_Type_HPaned,
               Class_Record => Class_Record'Access,
               Type_Name    => Class_Name
            )
         then
            Install_Class_Properties;
         end if;
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Item    : out Gtk_Item_Box;
                Name    : String;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Confirm : Boolean
             )  is
   begin
      Item := new Gtk_Item_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Name, Confirm);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Item_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget    : out Gtk_Fuzzy_Catalogue;
                Dock      : not null access Gtk_Container_Record'Class;
                Path      : Item_Path := "";
                Mode      : Filter_Mode := Features_List or
                                           Sets_List or
                                           Classifiers_List;
                Columns   : Positive := 4;
                Vertical  : Boolean  := False;
                Buttons   : Boolean  := True;
                Tree_Size : Gtk_Requisition :=
                               (Width => 180, Height => 550);
                List_Size : Gtk_Requisition :=
                               (Width => 600, Height => 550);
                Store     : Gtk_Persistent_Directory := null;
                Manager   : access Gtk_Recent_Manager_Record'Class :=
                               Get_Default;
                Tracing   : Traced_Actions := Trace_Nothing
             )  is
   begin
      Widget := new Gtk_Fuzzy_Catalogue_Record (Mode);
      Initialize
      (  Widget,
         Dock,
         Path,
         Columns,
         Vertical,
         Buttons,
         Tree_Size,
         List_Size,
         Store,
         Manager,
         Tracing
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Gtk_New (Gtk_Fuzzy_Catalogue)")
         )  );
         GLib.Object.Checked_Destroy (Widget);
         Widget := null;
         raise;
   end Gtk_New;

   procedure Gtk_New
             (  Widget  : out Gtk_Image_Menu_Item;
                Name    : UTF8_String;
                Button  : not null access Gtk_Widget_Record'Class;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
      Image : Gtk_Image;
   begin
      Gtk_New (Widget, Style_Get (Browser, Name));
      Gtk_New (Image, Style_Get (Button, "icon-id"), Icon_Size_Menu);
      Set_Image (Widget, Image);
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Folder_Selection;
                Name   : String;
                Hint   : out Gtk_Box;
                Item   : not null access Gtk_Item_Box_Record'Class
             )  is
   begin
      Widget := new Folder_Selection_Record;
      begin
         Initialize (Widget, Name, Hint, Item);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Folder_Selection)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Folder_Selection_Record'Class;
                Name   : String;
                Hint   : out Gtk_Box;
                Item   : not null access Gtk_Item_Box_Record'Class
             )  is
      Any : Storage_Handle;
   begin
      Widget.Item := Item.all'Unchecked_Access;
      Initialize
      (  Widget     => Widget,
         Name       => Name,
         Hint       => Hint,
         Picker     => Item.Browser.Directories,
         Constraint => Any,
         Editable   => True,
         Initialize => True
      );
   end Initialize;

   procedure Initialize
             (  Item    : not null access Gtk_Item_Box_Record'Class;
                Name    : String;
                Confirm : Boolean
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Item.Constraint := Create (Name);
      Initialize_VBox (Item);
      if not Button_Style_Updated then
         Init;
         Set_Button_Style (Cancel_Buttons.Class);
         Set_Button_Style (Windowize_Buttons.Class);
         Button_Style_Updated := True;
      end if;
      Set_Spacing (Item, Style_Get (Item.Browser, "column-spacing"));
      Set_Border_Width (Item, Style_Get (Item.Browser, "row-spacing"));
      if Confirm then
         Gtk_New_HBox (Item.Button_Box);
         Item.Button_Box.Set_Spacing (GInt (Button_Spacing));
         Item.Pack_End (Item.Button_Box, False, False);
         Gtk_New (Item.Commit);
         Item.Button_Box.Pack_End (Item.Commit, False, False);
         Gtk_New_HSeparator (Item.Button_Separator);
         Item.Pack_End (Item.Button_Separator, False, False);
         Box_Handlers.Connect
         (  Item.Commit,
            "clicked",
            Commit'Access,
            Item.all'Access
         );
      end if;
      Gtk_New (Item.Frame);
      Item.Frame.Set_Shadow_Type (Shadow_None);
      Item.Frame.Add (Item);

      Gtk_New (Item.Cancel);
      Item.Cancel.Set_Can_Default (False);
      Item.Cancel.Set_Can_Focus   (False);
      Set_Property (Item.Cancel, Build ("border-width"), GUInt'(0));
      Item.Cancel.Set_Border_Width (0);
      Item.Cancel.Set_Relief (Relief_None);
      Item.Cancel.Ref;
      Item.Cancel.Show_All;

      Gtk_New (Item.Undock);
      Item.Undock.Set_Can_Default (False);
      Item.Undock.Set_Can_Focus   (False);
      Set_Property (Item.Undock, Build ("border-width"), GUInt'(0));
      Item.Undock.Set_Border_Width (0);
      Item.Undock.Set_Relief (Relief_None);
      Item.Undock.Ref;
      Item.Undock.Show_All;

      Box_Handlers.Connect
      (  Item,
         "destroy",
         Destroy'Access,
         Item.all'Access
      );
      Box_Return_Handlers.Connect
      (  Item,
         "destroy_event",
         Box_Return_Handlers.To_Marshaller (Destroy_Event'Access),
         Item.all'Access
      );
      Box_Handlers.Connect
      (  Item.Cancel,
         "clicked",
         Cancel'Access,
         Item.all'Access
      );
      Box_Handlers.Connect
      (  Item.Undock,
         "clicked",
         Undock'Access,
         Item.all'Access
      );
   end Initialize;

   procedure Initialize
             (  Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Dock      : not null access Gtk_Container_Record'Class;
                Path      : Item_Path;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                Tree_Size : Gtk_Requisition;
                List_Size : Gtk_Requisition;
                Store     : Gtk_Persistent_Directory;
                Manager   : not null access
                            Gtk_Recent_Manager_Record'Class;
                Tracing   : Traced_Actions
             )  is
      Scroll : Gtk_Scrolled_Window;
      Box    : Gtk_Box;
      Frame  : Gtk_Frame;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Widget.Cache := Store;
      G_New (Widget, Get_Type (Vertical));
      if Vertical then
         Initialize_VPaned (Widget);
      else
         Initialize_HPaned (Widget);
      end if;
      Widget.Tracing := Tracing;
      Init; -- Load icons
      Widget.Dock  := Dock.all'Access;
      Widget.Files := new Files_Browsing;
      Ref (Widget.Dock);
      if Widget.Cache = null then
         Gtk_New
         (  Widget.Cache,
            Projects.Query (Widget),
            Manager,
            Tracing
         );
      end if;
      if Vertical then
         Gtk_New_HBox (Box);
         if Buttons then
            Gtk_New_VBox (Widget.Buttons);
         end if;
      else
         Gtk_New_VBox (Box);
         if Buttons then
            Gtk_New_HBox (Widget.Buttons);
         end if;
      end if;
      if Buttons then
         Box.Pack_Start (Widget.Buttons, False, False);
      end if;
      Gtk_New (Frame);
      Frame.Add (Box);
      Frame.Set_Shadow_Type (Shadow_Out);
      Widget.Add1 (Frame);
      -- Tree pane
      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      Gtk_New (Frame);
      Frame.Add (Scroll);
      Frame.Set_Shadow_Type (Shadow_In);
      Box.Pack_Start (Frame);
      Widget.Tree := new Gtk_Persistent_Storage_Tree_View_Record;
      Gtk.Persistent_Storage_Browser.Initialize
      (  Widget.Tree,
         Widget.Cache,
         Path
      );
      declare
         Size : Gtk_Requisition;
      begin
         Widget.Tree.Columns_Autosize;   -- Size columns
         Widget.Tree.Size_Request (Size); -- Query the integral size
         Set_Size_Request                 -- Set new size
         (  Widget.Tree,
            GInt'Max (1, GInt'Min (Size.Width,  Tree_Size.Width)),
            GInt'Max (1, GInt'Min (Size.Height, Tree_Size.Height))
         );
      end;
      Scroll.Add (Widget.Tree);
      -- List pane
      Gtk_New (Widget.List_Tabs);
      Widget.List_Tabs.Set_Tab_Pos (Pos_Left);
      Widget.Add2 (Widget.List_Tabs);
      if 0 /= (Widget.Mode and Features_List) then
         -- Features tab
         Feature_Pane.Gtk_New
         (  View      => Feature_Pane.Feature_Panel (Widget.Features),
            Widget    => Widget,
            Columns   => Columns,
            Vertical  => Vertical,
            Buttons   => Buttons,
            List_Size => List_Size
         );
         Widget.Current := Widget.Features;
      end if;
      if 0 /= (Widget.Mode and Sets_List) then
         -- Training sets tab
         Lecture_Pane.Gtk_New
         (  View      => Lecture_Pane.Lecture_Panel (Widget.Lectures),
            Widget    => Widget,
            Columns   => Columns,
            Vertical  => Vertical,
            Buttons   => Buttons,
            List_Size => List_Size
         );
         if Widget.Current = null then
            Widget.Current := Widget.Lectures;
         end if;
      end if;
      if 0 /= (Widget.Mode and Classifiers_List) then
         -- Classifiers tab
         Classifier_Pane.Gtk_New
         (  View      => Classifier_Pane.
                            Classifier_Panel (Widget.Classifiers),
            Widget    => Widget,
            Columns   => Columns,
            Vertical  => Vertical,
            Buttons   => Buttons,
            List_Size => List_Size
         );
         if Widget.Current = null then
            Widget.Current := Widget.Classifiers;
         end if;
      end if;
         -- Picker interface
      Widget.Directories := new Folder_Picker_Record;
      Widget.Directories.Browser := Widget.all'Unchecked_Access;
      Gtk.Fuzzy_Object.Initialize (Widget.Directories);
      Ref (Widget.Directories);
         -- Buttons
      if Buttons then
         Gtk_New (Widget.New_Project);
         Widget.Buttons.Pack_Start (Widget.New_Project, False, False);
         Handlers.Connect
         (  Widget.New_Project,
            "clicked",
            Projects.Create'Access,
            Widget.all'Access
         );

         Gtk_New (Widget.New_Folder);
         Widget.Buttons.Pack_Start (Widget.New_Folder, False, False);
         Handlers.Connect
         (  Widget.New_Folder,
            "clicked",
            Folders.Create'Access,
            Widget.all'Access
         );

         Gtk_New (Widget.New_From_FCL);
         Widget.Buttons.Pack_Start (Widget.New_From_FCL, False, False);
         Handlers.Connect
         (  Widget.New_From_FCL,
            "clicked",
            Folders.From_FCL'Access,
            Widget.all'Access
         );

         Gtk_New (Widget.Delete_Folder);
         Pack_Start
         (  Widget.Buttons,
            Widget.Delete_Folder,
            False,
            False
         );
         Handlers.Connect
         (  Widget.Delete_Folder,
            "clicked",
            Folders.Delete'Access,
            Widget.all'Access
         );
      end if;
      Widget.Renaming := new Move_Data;
         -- Handlers
      Handlers.Connect
      (  Widget,
         "destroy",
         Destroy'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Access
      );
      Selection_Handlers.Connect
      (  Get_Selection (Widget.Tree),
         "changed",
         Folder_Changed'Access,
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.List_Tabs,
         "switch_page",
         Switch_List_Page'Access,
         Widget.all'Access
      );
      Return_Handlers.Connect
      (  Widget.Tree,
         "button_press_event",
         Return_Handlers.To_Marshaller (Key_Press'Access),
         Widget.all'Access
      );
      Style_Updated (Widget, Widget.all'Access);
      Folder_Changed (Get_Selection (Widget.Tree), Widget.all'Access);
   exception
      when others =>
         if Store = null and then Widget.Cache /= null then
            Unref (Widget.Cache);
         end if;
         if Widget.Dock /= null then
            Ref (Widget.Dock);
         end if;
         raise;
   end Initialize;

   procedure Initialize
             (  View      : not null access Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                Mode      : Filter_Mode;
                List_Size : Gtk_Requisition
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Widget, "button-spacing");
      Frame  : Gtk_Frame;
      Label  : Gtk_HBox;
      Scroll : Gtk_Scrolled_Window;
   begin
      Gtk.Fuzzy_Object.Initialize (View);
         -- Create tab
      Gtk_New_VBox (Label);
      Gtk_New (View.Tab_Label);
      Set_Angle (View.Tab_Label, 90.0);
      Pack_Start (Label, View.Tab_Label, False, False);
         -- Create a list for the tab
      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      View.List := new Gtk_Fuzzy_Objects_View_Record;
      View.List.Mode    := Mode;
      View.List.Parent  := View.all'Unchecked_Access;
      View.List.Browser := Widget.all'Unchecked_Access;
      Gtk.Persistent_Storage_Browser.Initialize
      (  View.List,
         Widget.Tree,
         Columns
      );
      Panel_Handlers.Connect
      (  View.List,
         "directory-changed",
         Directory_Changed'Access,
         View.all'Access
      );
      Panel_Handlers.Connect
      (  View.List,
         "selection-changed",
         Selection_Changed'Access,
         View.all'Access
      );
      Panel_Handlers.Connect
      (  View.List.Browser.Tree.Get_Selection,
         "changed",
         Tree_Selection_Changed'Access,
         View.all'Access
      );
      declare
         Size : Gtk_Requisition;
      begin
         Size_Request (View.List, Size); -- Query the integral size
         Set_Size_Request                -- Set new size
         (  View.List,
            GInt'Max (1, GInt'Min (Size.Width,  List_Size.Width)),
            GInt'Max (1, GInt'Min (Size.Height, List_Size.Height))
         );
      end;
      Scroll.Add (View.List);
      if Vertical then
         Gtk_New_HBox (View.Box);
         if Buttons then
            Gtk_New_VBox (View.Buttons);
         end if;
      else
         Gtk_New_VBox (View.Box);
         if Buttons then
            Gtk_New_HBox (View.Buttons);
         end if;
      end if;
      Gtk_New (Frame);
      Frame.Set_Border_Width (Button_Spacing);
      Frame.Set_Shadow_Type (Shadow_None);
      Gtk_New (View.Creation_Time);
      Frame.Add (View.Creation_Time);
      if Buttons then
         View.Buttons.Pack_End (Frame, False, False);
         View.Buttons.Set_Spacing (GInt (Button_Spacing));
         View.Box.Pack_Start (View.Buttons, False, False);
         declare
            Refresh : Refresh_Buttons.Gtk_Style_Button;
         begin
            Gtk_New (Refresh);
            View.Buttons.Pack_Start (Refresh, False, False);
            Panel_Handlers.Connect
            (  Refresh,
               "clicked",
               Refresh_Clicked'Access,
               View.all'Unchecked_Access
            );
         end;
      end if;
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Frame.Add (Scroll);
      View.Box.Pack_Start (Frame);
      Label.Show_All;
      Widget.List_Tabs.Append_Page (View.Box, Label);
   end Initialize;

   function Input_Event
            (  Widget : not null access Gtk_Fuzzy_Objects_View_Record;
               Index  : Positive;
               Event  : Gdk_Event
            )  return Boolean is
      use Menu_References;
      Menu    : Gtk_Menu;
      Changed : Boolean;
   begin
      case Get_Event_Type (Event) is
         when Button_Press =>
            -- Button click
            if Get_Button (Event) = 3 then
               if not Is_Valid (Widget.Parent.Menu) then
                  Gtk_New (Menu);
                  Set (Widget.Parent.Menu, Menu);
               else
                  Menu := Get (Widget.Parent.Menu).all'Unchecked_Access;
                  Erase (Menu);
               end if;
               Move
               (  Widget,
                  Changed,
                  Get_State (Event),
                  Index
               );
               declare
                  Item      : Gtk_Image_Menu_Item;
                  Image     : Gtk_Image;
                  Separator : Gtk_Separator_Menu_Item;
               begin
                  Build_Item_Menu (Widget.Parent, Index);

                  Gtk_New (Separator);
                  Prepend (Menu, Separator);
                  begin
                     Gtk_New
                     (  Item,
                        (  Style_Get (Widget.Browser, "dsn-label")
                        &  ": "
                        &  Get_Class
                           (  Get_Directory_Cache (Widget.Parent.List),
                              Get_Path (Widget.Parent.List, Index)
                     )  )  );
                     Gtk_New (Image, Stock_About, Icon_Size_Menu);
                     Set_Image (Item, Image);
                     Prepend (Menu, Item);
                  exception
                     when Error : others =>
                        Log
                        (  Fuzzy_ML_Domain,
                           Log_Level_Critical,
                           (  "Fault: "
                           &  Exception_Information (Error)
                           &  Where ("Input_Event")
                        )  );
                  end;
               end;
               Show_All (Menu);
               Object_Popup.Popup
               (  Menu   => Gtk_Menu_Record (Menu.all)'Unchecked_Access,
                  Data   => Widget.all'Unchecked_Access,
                  Button => 3
               );
               return not Changed;
            end if;
         when others =>
            null;
      end case;
      return Input_Event
             (  Gtk_Persistent_Storage_Items_View_Record
                (  Widget.all
                ) 'Access,
                Index,
                Event
             );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Input_Event")
         )  );
         return False;
   end Input_Event;

   function Is_Current (Panel : access Panel_Record) return Boolean is
   begin
      return Panel /= null and then Panel.List.Browser.Current = Panel;
   end Is_Current;

   function Key_Press
            (  Widget  : access Gtk_Widget_Record'Class;
               Event   : Gdk_Event;
               Browser : Gtk_Fuzzy_Catalogue
            )  return Boolean is
      function Get_Class return UTF8_String is
         Cell_X : GInt;
         Cell_Y : GInt;
         Column : Gtk_Tree_View_Column;
         Path   : Gtk_Tree_Path;
         Found  : Boolean;
      begin
         declare
            X : GDouble;
            Y : GDouble;
         begin
            Get_Axis (Event, Axis_X, X);
            Get_Axis (Event, Axis_Y, Y);
            if (  X in -32_000.0..32_000.0
               and then
                  Y in -32_000.0..32_000.0
               )
            then
               Get_Path_At_Pos
               (  Browser.Tree,
                  GInt (X),
                  GInt (Y),
                  Path,
                  Column,
                  Cell_X,
                  Cell_Y,
                  Found
               );
            else
               return "-";
            end if;
         exception
            when Constraint_Error =>
               return "-";
         end;
         if Found then
            declare
               Storage : Storage_Handle;
               Name    : constant String :=
                            Get
                            (  Get_Model (Browser.Tree),
                               Get_Iter (Get_Model (Browser.Tree), Path)
                            );
            begin
               Path_Free (Path);
               Storage :=
                  Get_Storage (Browser.Cache, "" & Item_Path (Name));
               if not Is_Valid (Storage) then
                  return "-";
               elsif Persistent.SQLite.Is_SQLite (Storage) then
                  return Persistent.SQLite.Get_File_Name (Storage);
               else
                  return "ODBC";
               end if;
            exception
               when Error : others =>
                  Log
                  (  Fuzzy_ML_Domain,
                     Log_Level_Warning,
                     (  "Fault: "
                     &  Exception_Information (Error)
                     &  Where ("Key_Press")
                  )  );
                  return "-";
            end;
         else
            return "-";
         end if;
      end Get_Class;
      use Menu_References;
      Menu : Gtk_Menu;
   begin
      case Get_Event_Type (Event) is
         when Button_Press =>
            -- Button click
            if Get_Button (Event) = 3 then
               if not Is_Valid (Browser.Menu) then
                  Gtk_New (Menu);
                  Set (Browser.Menu, Menu);
               else
                  Menu := Get (Browser.Menu).all'Unchecked_Access;
                  Erase (Menu);
               end if;
               declare
                  Item      : Gtk_Image_Menu_Item;
                  Image     : Gtk_Image;
                  Separator : Gtk_Separator_Menu_Item;
               begin
                  Gtk_New (Item, Get_Class);
                  Gtk_New (Image, Stock_About, Icon_Size_Menu);
                  Set_Image (Item, Image);
                  Append (Menu, Item);

                  Gtk_New (Separator);
                  Append (Menu, Separator);

                  Gtk_New
                  (  Item,
                     "menu-delete",
                     Browser.Delete_Folder,
                     Browser
                  );
                  Append (Menu, Item);
                  Handlers.Connect
                  (  Item,
                     "activate",
                     Folders.Delete'Access,
                     Browser
                  );
               exception
                  when Error : others =>
                     Log
                     (  Fuzzy_ML_Domain,
                        Log_Level_Critical,
                        (  "Fault: "
                        &  Exception_Information (Error)
                        &  Where ("Key_Press")
                     )  );
               end;
               Show_All (Menu);
               Directory_Popup.Popup
               (  Menu   => Gtk_Menu_Record (Menu.all)'Unchecked_Access,
                  Button => 3,
                  Data   => Gtk_Persistent_Storage_Tree_View_Record
                               (Browser.Tree.all)'Unchecked_Access
               );
            end if;
         when others =>
            null;
      end case;
      return False;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Key_Press")
         )  );
         return False;
   end Key_Press;

   procedure Next (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Next;

   procedure Next
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      Next (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Next")
         )  );
   end Next;

   function Next (Browsing : Files_Browsing_Ptr) return Item_Path is
   begin
      Browsing.Current := Browsing.Current + 1;
      return Get (Browsing.Stack, Browsing.Current).all;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Next")
         )  );
         return "";
   end Next;

   function On_Top (Browsing : Files_Browsing_Ptr) return Boolean is
   begin
      return Browsing.Size <= Browsing.Current;
   end On_Top;

   procedure Previous (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Previous;

   procedure Previous
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      Previous (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Previous")
         )  );
   end Previous;

   function Previous (Browsing : Files_Browsing_Ptr) return Item_Path is
   begin
      Browsing.Current := Browsing.Current - 1;
      return Get (Browsing.Stack, Browsing.Current).all;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Previous")
         )  );
         return "";
   end Previous;

   procedure Progress
             (  Store : not null access Catalogue_Directory_Record;
                Path  : Item_Path;
                State : GDouble
             )  is
      Current : constant Panel := Get_Current (Store.Browser);
      Active  : Boolean;
   begin
      if State >= 1.0 then -- Hide the progress indicator
         if Store.Browser.Classifiers /= null then
            Store.Browser.Classifiers.Set_Progress (False);
         end if;
         if Store.Browser.Features /= null then
            Store.Browser.Features.Set_Progress (False);
         end if;
         if Store.Browser.Lectures /= null then
            Store.Browser.Lectures.Set_Progress (False);
         end if;
      elsif Current /= null then -- Show the progress indicator
         Current.Set_Progress (True);
         if Store.Browser.Classifiers.Progress /= null then
            Store.Browser.Classifiers.Progress.Set_Fraction (State);
            if Path'Length = 0 then
               Store.Browser.Classifiers.Progress.
                  Set_Text ("system volumes");
            else
               Store.Browser.Classifiers.Progress.
                  Set_Text (String (Path));
            end if;
         end if;
         if Store.Browser.Features.Progress /= null then
            Store.Browser.Features.Progress.Set_Fraction (State);
            if Path'Length = 0 then
               Store.Browser.Features.Progress.
                  Set_Text ("system volumes");
            else
               Store.Browser.Features.Progress.
                  Set_Text (String (Path));
            end if;
         end if;
         if Store.Browser.Lectures.Progress /= null then
            Set_Fraction (Store.Browser.Lectures.Progress, State);
            if Path'Length = 0 then
               Store.Browser.Lectures.Progress.
                  Set_Text ("system volumes");
            else
               Store.Browser.Lectures.Progress.
                  Set_Text (String (Path));
            end if;
         end if;
      end if;
      while Gtk.Main.Events_Pending loop
         Active := Gtk.Main.Main_Iteration;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Progress")
         )  );
   end Progress;

   procedure Push (Browsing : Files_Browsing_Ptr; Path : Item_Path) is
   begin
      if (  Browsing.Current > Browsing.Size
         or else
            Get (Browsing.Stack, Browsing.Current).all /= Path
         )
      then
         if Browsing.Current >= Browsing.Size then
            Browsing.Current := Browsing.Size + 1;
         else
            Browsing.Current := Browsing.Current + 1;
         end if;
         Put (Browsing.Stack, Browsing.Current, new Item_Path'(Path));
         Browsing.Size := Browsing.Current;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Push")
         )  );
   end Push;

   function Query
            (  Widget     : not null access Gtk_Fuzzy_Catalogue_Record;
               Title      : UTF8_String;
               Icon_Stock : UTF8_String;
               Icon_Size  : Gtk_Icon_Size_Enum;
               Message    : UTF8_String;
               Cancel     : Boolean := True
            )  return Gtk_Response_Type is
      use Gtk.Image;
      Dialog  : Gtk_Dialog;
      Box     : Gtk_HBox;
      Icon    : Gtk_Image;
      Default : Gtk_Button;
      Text    : Gtk_Text_View;
      Spacing : constant GUInt := Style_Get (Widget, "column-spacing");
      Result  : Gtk_Response_Type;
   begin
      Gtk_New (Dialog, Title, null, Modal);
      Gtk_New_HBox (Box);
      Set_Spacing (Box, GInt (Spacing));
      Gtk_New (Icon, Icon_Stock, Gtk_Icon_Size_Enum'Pos (Icon_Size));
      Box.Pack_Start (Icon, False, False);
      Gtk_New (Text);
      Text.Set_Cursor_Visible (False);
      Set_Editable (Text, False);
      Set_Wrap_Mode (Text, Wrap_Word);
      Box.Pack_Start (Text);
      Modify_Base (Text, State_Normal, Get_BG (Widget));
      Dialog.Get_Content_Area.Pack_Start (Box);
      Set_Text (Get_Buffer (Text), Message);
      Default :=
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Icon     => Stock_OK,
            Label    => "_OK",
            Response => Gtk_Response_OK
         );
      Default.Set_Can_Default (True);
      if Cancel then
         Add_Button_From_Stock
         (  Dialog   => Dialog,
            Icon     => Stock_Cancel,
            Label    => "_Cancel",
            Response => Gtk_Response_Cancel
         );
      end if;
      Set_Default_Response (Dialog, Gtk_Response_OK);
      Set_Size_Request (Box, 200, 50);
      Show_All (Box);
      Result := Run (Dialog);
      GLib.Object.Checked_Destroy (Dialog);
      return Result;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Query")
         )  );
         GLib.Object.Checked_Destroy (Dialog);
         raise;
   end Query;

   procedure Refresh_Clicked
             (  Object : access GObject_Record'Class;
                View   : Panel
             )  is
      Changed : Boolean := True;
   begin
      if View /= null then
         View.List.Refresh;
--       View.List.Refilter;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Refresh_Clicked")
         )  );
   end Refresh_Clicked;

   procedure Remove_Buttons
             (  Item : not null access Gtk_Item_Box_Record
             )  is
   begin
      if Item.Button_Box /= null then
         Item.Remove (Item.Button_Box);
         Item.Remove (Item.Button_Separator);
         Item.Button_Box := null;
         Item.Commit     := null;
         Item.Close      := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove_Buttons")
         )  );
   end Remove_Buttons;

   procedure Remove_Close
             (  Item : not null access Gtk_Item_Box_Record
             )  is
   begin
      if Item.Button_Box /= null and then Item.Close /= null then
         Item.Button_Box.Remove (Item.Close);
         Item.Close := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove_Close")
         )  );
   end Remove_Close;

   procedure Remove_Next
             (  Item : not null access Gtk_Item_Box_Record
             )  is
   begin
      if Item.Button_Box /= null and then Item.Next /= null then
         Item.Button_Box.Remove (Item.Next);
         Item.Next := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove_Next")
         )  );
   end Remove_Next;

   procedure Remove_OK (Item : not null access Gtk_Item_Box_Record) is
   begin
      if Item.Button_Box /= null and then Item.Commit /= null then
         Item.Button_Box.Remove (Item.Commit);
         Item.Commit := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove_OK")
         )  );
   end Remove_OK;

   procedure Remove_Previous
             (  Item : not null access Gtk_Item_Box_Record
             )  is
   begin
      if Item.Button_Box /= null and then Item.Previous /= null then
         Item.Button_Box.Remove (Item.Previous);
         Item.Previous := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove_Previous")
         )  );
   end Remove_Previous;

   function Restore
            (  Widget  : not null access Gtk_Fuzzy_Catalogue_Record;
               Key     : UTF8_String;
               Default : UTF8_String
            )  return UTF8_String is
      List : Gtk_Recent_Info_Array renames
             Get_Items (Get_Manager (Widget.Cache));
      Name : constant String := Get_Application_Name;
   begin
      for Index in List'Range loop
         if (  Has_Application (List (Index), Name)
            and then
               Get_Description (List (Index)) = Key
            )
         then
            return Result : constant UTF8_String :=
                                     Get_URI (List (Index)) do
               for Rest in Index..List'Last loop
                  Unref (List (Rest));
               end loop;
            end return;
         end if;
         Unref (List (Index));
      end loop;
      return Default;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Restore")
         )  );
         return "";
   end Restore;

   procedure Selection_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             )  is
   begin
      Selection_Changed (View);
      State_Changed (View);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selection_Changed")
         )  );
   end Selection_Changed;

   procedure Selection_Changed
             (  View : not null access Panel_Record
             )  is
   begin
      if Get_Current (View.List.Browser) = View.all'Access then
         declare
            Selected : constant Selection := View.List.Get_Selection;
         begin
            if Selected'Length = 1 then
               Set_Text
               (  View.Creation_Time,
                  Ada.Calendar.Formatting.Image
                  (  Get_Creation_Time
                     (  View.List.Get_Directory_Cache,
                        Get_Path (View.List, Selected (Selected'First))
               )  )  );
               return;
            end if;
         end;
      end if;
      View.Creation_Time.Set_Text ("");
   exception
      when Ada.IO_Exceptions.Name_Error =>
         View.Creation_Time.Set_Text ("");
      when Error : others =>
         View.Creation_Time.Set_Text ("");
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selection_Changed")
         )  );
   end Selection_Changed;

   procedure Service (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Service;

   procedure Service (Data : in out Update_Request) is
   begin
      Updated (Data.Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service")
         )  );
   end Service;

   procedure Service_Update
             (  Item : not null access Gtk_Item_Box_Record'Class
             )  is
      Data : Update_Request (Item.all'Unchecked_Access);
   begin
      Gtk.Main.Router.Request (Data);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service_Update")
         )  );
   end Service_Update;

   procedure Set
             (  View   : not null access Panel_Record;
                Store  : Storage_Handle;
                Object : Deposit_Handle
             )  is
      Path    : constant Item_Path :=
                   View.List.Browser.Cache.Get_Path (Store, Object);
      Changed : Boolean;
   begin
      View.List.Browser.Get_Tree_View.Set_Current_Directory
      (  View.List.Browser.Tree.Get_Cache.Get_Directory (Path)
      );
      View.List.Move
      (  Changed  => Changed,
         Modifier => 0,
         To =>
            View.List.Get_Index
            (  Get_Name (View.List.Browser.Tree.Get_Cache, Path)
      )     );
   exception
      when others =>
         null;
   end Set;

   procedure Set
             (  View : not null access Panel_Record;
                Path : Item_Path
             )  is
     Store   : Storage_Handle;
     Object  : Deposit_Handle;
     Changed : Boolean;
   begin
      View.List.Get_Directory_Cache.Browse (Path, Store, Object);
      View.List.Browser.Get_Tree_View.Set_Current_Directory
      (  View.List.Browser.Tree.Get_Cache.Get_Directory (Path)
      );
      View.List.Move
      (  Changed  => Changed,
         Modifier => 0,
         To =>
            View.List.Get_Index
            (  Get_Name (View.List.Browser.Tree.Get_Cache, Path)
      )     );
   exception
      when others =>
         null;
   end Set;

   procedure Set
             (  View : not null access Folder_Picker_Record;
                Path : Item_Path
             )  is
   begin
      View.Browser.Get_Tree_View.Set_Current_Directory (Path);
   exception
      when others =>
         null;
   end Set;

   procedure Set
             (  View   : not null access Folder_Picker_Record;
                Store  : Storage_Handle;
                Object : Deposit_Handle
             )  is
   begin
      Set (View, Get_Path (View.Browser.Cache, Store, Object));
   exception
      when others =>
         null;
   end Set;

   procedure Set_Icon
             (  Item : not null access Gtk_Item_Box_Record;
                Icon : UTF8_String
             )  is
      use Gtk.Icon_Factory;
      Set : constant Gtk_Icon_Set := Lookup_Default (Icon);
   begin
      Gtk_New
      (  Item.Image,
         Icon,
         Gtk_Icon_Size_Enum'Pos
         (  Style_Get (Item.Browser, "tab-icon-size")
      )  );
      if Item.Window = null then
         Erase (Item.Image_Box);
         Item.Image_Box.Pack_Start (Item.Image, False, False);
         Item.Image_Box.Show_All;
      else
         Set_Icon
         (  Item.Window,
            Render_Icon
            (  Set,
               Get_Style (Item.Browser),
               Text_Dir_None,
               State_Normal,
               Icon_Size_Menu,
               Item
         )  );
         Unref (Item.Image);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Icon")
         )  );
   end Set_Icon;

   procedure Set_Item
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Page   : not null access Gtk_Item_Box_Record'Class
             )  is
   begin
      if Page.Window = null then
         Widget.Item_Tabs.Set_Current_Page
         (  Widget.Item_Tabs.Page_Num (Page.Frame)
         );
      elsif Activate_Default (Page.Window) then
         null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Item")
         )  );
   end Set_Item;

   procedure Set_Progress
             (  View  : not null access Panel_Record;
                State : Boolean
             )  is
      Active : Boolean;
   begin
      if State then
         if View.Progress = null then
            Gtk_New (View.Progress_Box, 0.0, 0.5, 1.0, 0.0);
            Gtk_New (View.Progress);
            View.Progress.Set_Show_Text (True);
            View.Progress_Box.Add (View.Progress);
            View.Buttons.Pack_End (View.Progress_Box);
            View.Buttons.Show_All;
         end if;
      else
         if View.Progress /= null then
            View.Progress := null;
            View.Buttons.Remove (View.Progress_Box);
            View.Progress_Box := null;
         end if;
      end if;
      while Gtk.Main.Events_Pending loop
         Active := Gtk.Main.Main_Iteration;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Progress")
         )  );
   end Set_Progress;

   procedure Set_Title
             (  Item  : not null access Gtk_Item_Box_Record;
                Title : UTF8_String
             )  is
   begin
      Set_Text (Item.Text, Title);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Title")
         )  );
   end Set_Title;

   procedure Show_Object
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
   begin
      declare
         Storage : Storage_Handle;
         Parent  : Deposit_Handle;
         Cache   : Gtk_Abstract_Directory_Record'Class renames
                      Gtk_Abstract_Directory_Record'Class
                      (  Item.Browser.Cache.all
                      );
      begin
         Browse
         (  Store   => Item.Browser.Cache,
            Path    => Get_Directory (Cache'Access, Item.Path.all),
            Storage => Storage,
            Object  => Parent
         );
         declare
            Class : constant String :=
                    Get_Class
                    (  Storage,
                       String (Get_Name (Cache'Access, Item.Path.all)),
                       Parent
                    );
         begin
            if Is_Prefix (Directory_Class, Class) then
               if Is_Current (Item.Browser.Classifiers) then
                  Set (Item.Browser.Classifiers, Item.Path.all);
               elsif Is_Current (Item.Browser.Features) then
                  Set (Item.Browser.Features, Item.Path.all);
               elsif Is_Current (Item.Browser.Lectures) then
                  Set (Item.Browser.Lectures, Item.Path.all);
               end if;
            elsif Is_Prefix (Feature_Class, Class) then
               if Item.Browser.Features /= null then
                  Switch_To (Item.Browser.Features, Item.Browser);
                  Set (Item.Browser.Features, Item.Path.all);
               end if;
            elsif Is_Prefix (Lecture_Class, Class) then
               if Item.Browser.Lectures /= null then
                  Switch_To (Item.Browser.Lectures, Item.Browser);
                  Set (Item.Browser.Lectures, Item.Path.all);
               end if;
            elsif Is_Prefix (Classifier_Class, Class) then
               if Item.Browser.Classifiers /= null then
                  Switch_To (Item.Browser.Classifiers, Item.Browser);
                  Set (Item.Browser.Classifiers, Item.Path.all);
               end if;
            end if;
         end;
      end;
   exception
      when others =>
         null;
   end Show_Object;

   procedure Start_Servicing
             (  Item : not null access Gtk_Item_Box_Record'Class
             )  is
   begin
      if Item.Worker /= null then
         raise Use_Error;
      end if;
      Ref (Item);
      Item.Indicator := Create_Indicator (Item);
      if Item.Indicator /= null then
         Item.Pack_Start (Item.Indicator, False, False);
         Show_All (Item.Indicator);
      end if;
      if Item.Commit /= null then
         Item.Commit.Set_Sensitive (False);
      end if;
      Item.Worker := new Worker_Task (Item.all'Unchecked_Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Start_Servicing")
         )  );
   end Start_Servicing;

   procedure Store
             (  Widget : not null access Gtk_Fuzzy_Catalogue_Record;
                Key    : UTF8_String;
                Value  : UTF8_String
             )  is
   begin
      if Add_Full
         (  Manager      => Get_Manager (Widget.Cache),
            URI          => Value,
            Display_Name => "",
            Description  => Key
         )
      then
         null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Store")
         )  );
   end Store;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Browser, "button-spacing");
   begin
      if Browser.Buttons /= null then
         Set_Spacing (Browser.Buttons, GInt (Button_Spacing));
      end if;
      if Browser.Features /= null then
         Style_Updated (Browser.Features);
      end if;
      if Browser.Lectures /= null then
         Style_Updated (Browser.Lectures);
      end if;
      if Browser.Classifiers /= null then
         Style_Updated (Browser.Classifiers);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

   procedure Switch_Items_Page
             (  Widget  : access Gtk_Widget_Record'Class;
                Params  : GValues;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Current_Page : constant GInt := GInt (Get_UInt (Nth (Params, 2)));
      Page : Gtk_Item_Box;
   begin
      for N in 0..Get_N_Pages (Browser.Item_Tabs) - 1 loop
         Page :=
            Gtk_Item_Box_Record'Class
            (  Get_Child
               (  Gtk_Frame_Record'Class
                  (  Get_Nth_Page
                     (  Browser.Item_Tabs,
                        N
                     ) .all
                  ) 'Unchecked_Access
               ) .all
            ) 'Unchecked_Access;
         if Page.Tab_Buttons /= null then
            if N = Current_Page then
               if not Is_In (Page.Tab_Buttons, Page.Cancel) then
                  -- Add the cancel and undock buttons to the tab
                  Pack_End (Page.Tab_Buttons, Page.Cancel);
                  Pack_End (Page.Tab_Buttons, Page.Undock);
               end if;
            else
               if Is_In (Page.Tab_Buttons, Page.Cancel) then
                  -- Remove the cancel and undock buttons from the tab
                  Remove (Page.Tab_Buttons, Page.Cancel);
                  Remove (Page.Tab_Buttons, Page.Undock);
               end if;
            end if;
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Switch_Items_Page")
         )  );
   end Switch_Items_Page;

   procedure Switch_List_Page
             (  Widget  : access Gtk_Widget_Record'Class;
                Params  : GValues;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Page : constant Gtk_Widget :=
                      Get_Nth_Page
                      (  Browser.List_Tabs,
                         GInt (Get_UInt (Nth (Params, 2)))
                      );
      function Is_On (This : Panel) return Boolean is
      begin
         return This /= null and then This.Box.all'Access = Page;
      end Is_On;
   begin
      if Is_On (Browser.Classifiers) then
         Browser.Current := Browser.Classifiers;
      elsif Is_On (Browser.Features) then
         Browser.Current := Browser.Features;
      elsif Is_On (Browser.Lectures) then
         Browser.Current := Browser.Lectures;
      end if;
      if Browser.Classifiers /= null then
         State_Changed (Browser.Classifiers);
      end if;
      if Browser.Features /= null then
         State_Changed (Browser.Features);
      end if;
      if Browser.Lectures /= null then
         State_Changed (Browser.Lectures);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Switch_List_Page")
         )  );
   end Switch_List_Page;

   procedure Switch_To
             (  View   : not null access Panel_Record;
                Widget : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Set_Current_Page
      (  Widget.List_Tabs,
         Widget.List_Tabs.Page_Num (View.Box)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Switch_To")
         )  );
   end Switch_To;

   procedure Switch_To
             (  Widget : not null access
                         Gtk_Fuzzy_Catalogue_Record'Class;
                Mode   : Filter_Mode
             )  is
   begin
      if (  0 /= (Mode and Features_List)
         and then
            Widget.Features /= null
         )  then
         Switch_To (Widget.Features, Widget);
      elsif (  0 /= (Mode and Classifiers_List)
            and then
               Widget.Classifiers /= null
            )  then
         Switch_To (Widget.Features, Widget);
      elsif (  0 /= (Mode and Sets_List)
            and then
               Widget.Lectures /= null
            )  then
         Switch_To (Widget.Lectures, Widget);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Switch_To")
         )  );
   end Switch_To;

   procedure Tree_Selection_Changed
             (  View : not null access Panel_Record
             )  is
   begin
      null;
   end Tree_Selection_Changed;

   procedure Tree_Selection_Changed
             (  Object : access GObject_Record'Class;
                View   : Panel
             )  is
   begin
      Tree_Selection_Changed (View);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Tree_Selection_Changed")
         )  );
   end Tree_Selection_Changed;

   procedure Undock
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Gtk_Item_Box
             )  is
      use Gtk.Image;
      use Gtk.Icon_Factory;
      Size : aliased Gtk_Icon_Size;
      Icon : constant String := Get (Item.Image, Size'Access);
      Name : constant String := Get_Text (Item.Text);
      Set  : constant Gtk_Icon_Set := Lookup_Default (Icon);
   begin
      Ref (Item.Frame);
         -- Remove the cancel and undock buttons from the tab
      if (  Item.Tab_Buttons /= null
         and then
            Is_In (Item.Tab_Buttons, Item.Cancel)
         )
      then
         Remove (Item.Tab_Buttons, Item.Cancel);
         Remove (Item.Tab_Buttons, Item.Undock);
         Item.Tab_Buttons := null;
      end if;
         -- Removing the page
      if (  Item.Browser.Item_Tabs /= null
         and then
            Is_In (Item.Browser.Item_Tabs, Item.Frame)
         )
      then
         Remove (Item.Browser.Item_Tabs, Item.Frame);
         if Get_N_Pages (Item.Browser.Item_Tabs) = 0 then
            Remove (Item.Browser.Dock, Item.Browser.Item_Tabs);
            Item.Browser.Item_Tabs := null;
         end if;
      end if;
      Gtk_New (Item.Window);
      Set_Title (Item.Window, Name);
      Set_Icon
      (  Item.Window,
         Render_Icon
         (  Set,
            Get_Style (Item.Browser),
            Text_Dir_None,
            State_Normal,
            Size,
            Widget
      )  );
      Add (Item.Window, Item.Frame);
      Unref (Item.Frame);
      Add (Item.Browser.Floating, Item.Window.all'Unchecked_Access);
      Handlers.Connect
      (  Item.Window,
         "destroy",
         Destroy_Floating'Access,
         Item.Browser.all'Unchecked_Access
      );
      Box_Return_Handlers.Connect
      (  Item.Window,
         "delete_event",
         Box_Return_Handlers.To_Marshaller (Delete_Floating'Access),
         Item
      );
      Show (Item.Window);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Undock")
         )  );
   end Undock;

   procedure Updated (Item : not null access Gtk_Item_Box_Record) is
   begin
      null;
   end Updated;

   task body Worker_Task is
      use Gtk_Item_Box_Messages;
   begin
      Service (Item);
      Send (Do_Complete'Access, Item.all'Unchecked_Access);
   exception
      when Error : others =>
         if Item.Error = null then
            Item.Error := new Exception_Occurrence;
         end if;
         Save_Occurrence (Item.Error.all, Error);
         Send (Do_Abort'Access, Item.all'Unchecked_Access);
   end Worker_Task;

end Gtk.Fuzzy_Catalogue;
