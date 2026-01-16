--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Feature_Pane                           Winter, 2008       --
--  Implementation                                                    --
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

with Deposit_Handles;                use Deposit_Handles;
with Fuzzy.Feature;                  use Fuzzy.Feature;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Fuzzy_Catalogue.Renamings;  use Gtk.Fuzzy_Catalogue.Renamings;
with Gtk.Separator;                  use Gtk.Separator;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Gtk.Table;                      use Gtk.Table;
with Name_Tables;                    use Name_Tables;
with Persistent;                     use Persistent;
with Persistent.Handle;              use Persistent.Handle;
with Strings_Edit;                   use Strings_Edit;

with Gtk.Fuzzy_Catalogue.Renaming_Tools;
use  Gtk.Fuzzy_Catalogue.Renaming_Tools;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Feature_Pane is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Feature_Pane." & Name;
   end Where;

   procedure Add
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      declare
         View : constant Feature_Panel := Feature_Panel (Browser.Features);
      begin
         if View.Create.Is_Valid then
            Set_Item (Browser, View.Create.Get);
         else
            declare
               Constructor : Feature_Create_Box;
            begin
               Gtk_New (Constructor, Browser);
               View.Create.Set (Constructor);
            end;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            "Fault: " & Exception_Information (Error) & Where ("Add")
         );
   end Add;

   procedure Build_Item_Menu
             (  View  : not null access Feature_Panel_Record;
                Index : Positive
             )  is
      use Menu_References;
      Browser : constant Gtk_Fuzzy_Catalogue := View.List.Browser;
      Item    : Gtk_Image_Menu_Item;
      Menu    : constant Gtk_Menu :=
                         Get (View.Menu).all'Unchecked_Access;
   begin
      if View.Buttons = null then
         return;
      end if;
      if View.Is_Feature ((1 => Index)) then
         Gtk_New (Item, "menu-view", View.View_Button, Browser);
         Menu.Append (Item);
         Handlers.Connect
         (  Item,
            "activate",
            Show'Access,
            View.List.Browser
         );
         Gtk_New (Item, "menu-copy", View.Copy_Button, Browser);
         Menu.Append (Item);
         Handlers.Connect
         (  Item,
            "activate",
            Copy'Access,
            View.List.Browser
         );
      end if;
      Gtk_New (Item, "menu-move", View.Move_Button, Browser);
      Menu.Append (Item);
      Handlers.Connect
      (  Item,
         "activate",
         Move'Access,
         View.List.Browser
      );
      Gtk_New (Item, "menu-rename", View.Rename_Button, Browser);
      Menu.Append (Item);
      Handlers.Connect
      (  Item,
         "activate",
         Rename'Access,
         View.List.Browser
      );
      Gtk_New (Item, "menu-delete", View.Delete_Button, Browser);
      Menu.Append (Item);
      Handlers.Connect
      (  Item,
         "activate",
         Delete'Access,
         View.List.Browser
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Build_Item_Menu")
         )  );
   end Build_Item_Menu;

   procedure Check
             (  View       : not null access Feature_Panel_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      Browse
      (  Get_Directory_Cache (View.List),
         Path,
         Store,
         Object
      );
      if (  Object.Is_Valid
         and then
            Object.Ptr.all in Feature_Object'Class
         )
      then
         if not Check (Constraint, Store) then
            raise Picking_Error with
                  Style_Get (View.List.Browser, "mixed-storage-error");
         end if;
         return;
      end if;
      Store.Invalidate;
      Object.Invalidate;
   exception
      when Picking_Error =>
         Store.Invalidate;
         Object.Invalidate;
         raise;
      when others =>
         Store.Invalidate;
         Object.Invalidate;
   end Check;

   procedure Commit
             (  Item : not null access Feature_Create_Box_Record
             )  is
      Feature : Deposit_Handle;
      Store   : Storage_Handle;
      Folder  : Deposit_Handle;
   begin
      Get_Folder (Item.Folder_Name, Store, Folder);
      if not Is_Valid (Store) then
         return;
      end if;
      declare
         Name : constant String :=
                         Check_New_Name
                         (  Item,
                            Store,
                            Folder,
                            Get_Text (Item.Name_Edit)
                         );
      begin
         if Name'Length = 0 then
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         end if;
         Feature := To_Deposit_Handle (Create (Item.Factory, Name));
         Put
         (  Storage => Store,
            Object  => Feature,
            Name    => Name,
            Parent  => Folder
         );
         declare -- Update the cache
            Class : constant String := Get_Class (Feature);
         begin
            Created
            (   Store     => Get_Cache (Item.Browser),
                Directory => Get_Path  (Item.Folder_Name),
                Item      =>
                (   Name_Length => Name'Length,
                    Kind_Length => Class'Length,
                    Policy      => Cache_Expanded,
                    Directory   => False,
                    Name        => Item_Name (Name),
                    Kind        => Item_Type (Class)
            )   );
         end;
      exception
         when Reason : Ada.IO_Exceptions.Data_Error =>
            Error
            (  Item,
               (  Style_Get (Item.Browser, "creation-error")
               &  Exception_Message (Reason)
            )  );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         when Ada.IO_Exceptions.Name_Error =>
            Error_Duplicated
            (  Item    => Item,
               Name    => Name,
               Storage => Store,
               Parent  => Folder
            );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         when Ada.IO_Exceptions.Use_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "unsupported-error")
            );
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
      end;
      Delete (Item);
   exception
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
   end Commit;

   procedure Copy
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View : constant Feature_Panel := Feature_Panel (Browser.Features);
   begin
      if View.Create.Is_Valid then
         Delete (View.Create.Get);
      end if;
      declare
         Constructor : Feature_Create_Box;
         Store       : Storage_Handle;
         Feature     : Feature_Handle;
      begin
         Store.Invalidate;
         View.Get_Current (Get_Selection (View.List), Store, Feature);
         Store.Invalidate;
         if Feature.Is_Valid then
            Gtk_New (Constructor, Store, Feature, Browser);
            View.Create.Set (Constructor);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Copy")
         )  );
   end Copy;

   procedure Delete
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      Delete
      (  Widget,
         Feature_Panel (Browser.Features).all'Unchecked_Access,
         Browser
      );
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

   procedure Get
             (  View       : not null access Feature_Panel_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      if View.Is_Current then
         declare
            Selected : constant Selection := Get_Selection (View.List);
         begin
            if Selected'Length = 1 then
               Check
               (  View,
                  Get_Path (View.List, Selected (Selected'First)),
                  Constraint,
                  Store,
                  Object
               );
               return;
            end if;
         end;
      end if;
      Store.Invalidate;
      Object.Invalidate;
   exception
      when others =>
         Store.Invalidate;
         Object.Invalidate;
   end Get;

   function Get
            (  View       : not null access Feature_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is
      Selected : constant Selection := Get_Selection (View.List);
   begin
      if View.Is_Feature (Selected) then
         declare
            Store  : Storage_Handle;
            Object : Deposit_Handle;
            Result : constant Item_Path :=
                        Get_Path (View.List, Selected (Selected'First));
         begin
            Check (View, Result, Constraint, Store, Object);
            return Result;
         end;
      end if;
      return "";
   exception
      when Picking_Error =>
         return "";
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get")
         )  );
         return "";
   end Get;

   procedure Get_Current
             (  View     : not null access Feature_Panel_Record;
                Selected : Selection;
                Store    : out Storage_Handle;
                Feature  : out Feature_Handle
             )  is
   begin
      if not View.Is_Current or else Selected'Length /= 1 then
         Store.Invalidate;
         Feature.Invalidate;
      else
         declare
            Object : Deposit_Handle;
         begin
            View.List.Get_Directory_Cache.Browse
            (  Get_Path (View.List, Selected (Selected'First)),
               Store,
               Object
            );
            if (  Object.Is_Valid
               and then
                  Object.Ptr.all in Feature_Object'Class
               )
            then
               Feature.Ref (To_Feature_Object_Ptr (Object.Ptr));
            else
               Store.Invalidate;
               Feature.Invalidate;
            end if;
         exception
            when Reason : others =>
               Fault (View.List.Browser, Exception_Message (Reason));
               Store.Invalidate;
               Feature.Invalidate;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Current")
         )  );
         Store.Invalidate;
         Feature.Invalidate;
   end Get_Current;

   procedure Get_Selected
             (  View     : not null access Feature_Panel_Record;
                Store    : out Storage_Handle;
                Features : out Fuzzy.Feature.Handle.Container.Set
             )  is
   begin
      Store.Invalidate;
      Erase (Features);
      if not View.Is_Current then
         return;
      end if;
      declare
         Selected : constant Selection := Get_Selection (View.List);
         Object   : Deposit_Handle;
      begin
         if Selected'Length = 0 then
            return;
         end if;
         for Index in Selected'Range loop
            Browse
            (  Get_Directory_Cache (View.List),
               Get_Path (View.List, Selected (Index)),
               Store,
               Object
            );
            if (  not Object.Is_Valid
               or else
                  Object.Ptr.all not in Feature_Object'Class
               )
            then
               Store.Invalidate;
               Erase (Features);
               return;
            end if;
            Add (Features, To_Feature_Object_Ptr (Object.Ptr));
         end loop;
      exception
         when Reason : others =>
            Fault (View.List.Browser, Exception_Message (Reason));
            Store.Invalidate;
            Erase (Features);
            return;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Selected")
         )  );
         Store.Invalidate;
         Erase (Features);
   end Get_Selected;

   procedure Gtk_New
             (  Item    : out Feature_Create_Box;
                Store   : Storage_Handle;
                Feature : Feature_Handle;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Feature_Create_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Store, Feature);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Feature_Create_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Item    : out Feature_Create_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
      Store   : Storage_Handle;
      Feature : Feature_Handle;
   begin
      Gtk_New (Item, Store, Feature, Browser);
   end Gtk_New;

   procedure Gtk_New
             (  Item    : out Feature_View_Box;
                Feature : Feature_Handle;
                Title   : UTF8_String;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Feature_View_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Feature, Title);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Feature_View_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  View      : out Feature_Panel;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             )  is
   begin
      View := new Feature_Panel_Record;
      begin
         Initialize
         (  View      => View,
            Widget    => Widget,
            Columns   => Columns,
            Vertical  => Vertical,
            Buttons   => Buttons,
            List_Size => List_Size
         );
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Feature_Panel)")
            )  );
            GLib.Object.Checked_Destroy (View);
            View := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item    : not null access
                          Feature_Create_Box_Record'Class;
                Store   : Storage_Handle;
                Feature : Feature_Handle
             )  is
      Grid  : Gtk_Table;
      Label : Gtk_Label;
      Hint  : Gtk_Box;
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "feature create box", True);
      Set (Get_Constraint (Item), Store);
      Gtk_New (Grid, 2, 3, False);
      Set_Col_Spacings (Grid, Column_Spacing);
      Set_Row_Spacings (Grid, Row_Spacing);

      Gtk_New (Label, Style_Get (Item.Browser, "feature-name-label"));
      Grid.Attach
      (  Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Item.Name_Edit);
      Grid.Attach
      (  Item.Name_Edit,
         1, 2,
         0, 1,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Name_Hint);
      Grid.Attach
      (  Item.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Name_Hint, Item.Browser, None, True);

      Gtk_New (Label, Style_Get (Item.Browser, "folder-path-label"));
      Grid.Attach
      (  Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Item.Folder_Name, "feature folder", Hint, Item);
      Grid.Attach
      (  Item.Folder_Name,
         1, 2,
         1, 2,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Grid.Attach
      (  Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Item.Pack_Start (Grid, False, False);

      Gtk_New
      (  Widget         => Item.Factory,
         Feature_Picker => Item.Browser.Features.all'Unchecked_Access,
         Feature        => Feature,
         Classifier_Picker =>
            Item.Browser.Classifiers.all'Unchecked_Access
      );
      Item.Factory.Set_Spacing
      (  Button => Button_Spacing,
         Column => Column_Spacing,
         Row    => Row_Spacing
      );
      Item.Pack_Start (Item.Factory);

      Combine
      (  Get_Constraint (Item.Folder_Name),
         Get_Constraint (Item.Factory)
      );
      Item.Show_All;

      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-new-feature-label"),
         Style_Get (Item.Browser, "tab-new-feature-icon"),
         Item
      );
      Create_Handlers.Connect
      (  Item.Name_Edit,
         "changed",
         Name_Changed'Access,
         Item.all'Access
      );
   end Initialize;

   procedure Initialize
             (  Item    : not null access
                          Feature_View_Box_Record'Class;
                Feature : Feature_Handle;
                Title   : UTF8_String
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "feature view box", False);
      if Item.Browser.Classifiers = null then
         Gtk_New
         (  Widget   => Item.Factory,
            Feature  => Feature,
            Editable => False,
            Feature_Picker =>
               Item.Browser.Features.all'Unchecked_Access
         );
      else
         Gtk_New
         (  Widget   => Item.Factory,
            Feature  => Feature,
            Editable => False,
            Classifier_Picker =>
               Item.Browser.Classifiers.all'Unchecked_Access,
            Feature_Picker =>
               Item.Browser.Features.all'Unchecked_Access
         );
      end if;
      Item.Factory.Set_Spacing
      (  Button => Button_Spacing,
         Column => Column_Spacing,
         Row    => Row_Spacing
      );
      Item.Pack_Start (Item.Factory);

      Item.Show_All;

      Add_Item
      (  Item.Browser,
         Title,
         Style_Get (Item.Browser, "tab-view-feature-icon"),
         Item
      );
   end Initialize;

   procedure Initialize
             (  View      : not null access Feature_Panel_Record'Class;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Widget, "button-spacing");
      Separator : Gtk_Separator;
   begin
      Initialize
      (  View      => Panel_Record (View.all)'Access,
         Widget    => Widget,
         Columns   => Columns,
         Vertical  => Vertical,
         Buttons   => Buttons,
         Mode      => Features_List,
         List_Size => List_Size
      );

      if Buttons then
         Gtk_New (View.View_Button);
         View.Buttons.Set_Spacing (GInt (Button_Spacing));
         View.View_Button.Set_Sensitive (False);
         View.Buttons.Pack_Start (View.View_Button, False, False);
         Handlers.Connect
         (  View.View_Button,
            "clicked",
            Show'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         View.Buttons.Pack_Start (Separator, False, False);

         Gtk_New (View.Add_Button);
         View.Buttons.Pack_Start (View.Add_Button, False, False);
         Handlers.Connect
         (  View.Add_Button,
            "clicked",
            Add'Access,
            Widget.all'Access
         );

         Gtk_New (View.Copy_Button);
         View.Buttons.Pack_Start (View.Copy_Button, False, False);
         Handlers.Connect
         (  View.Copy_Button,
            "clicked",
            Copy'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         View.Buttons.Pack_Start (Separator, False, False);

         Gtk_New (View.Move_Button);
         View.Buttons.Pack_Start (View.Move_Button, False, False);
         View.Move_Button.Set_Sensitive (False);
         Handlers.Connect
         (  View.Move_Button,
            "clicked",
            Move'Access,
            Widget.all'Access
         );

         Gtk_New (View.Rename_Button);
         View.Buttons.Pack_Start (View.Rename_Button, False, False);
         View.Rename_Button.Set_Sensitive (False);
         Handlers.Connect
         (  View.Rename_Button,
            "clicked",
            Rename'Access,
            Widget.all'Access
         );

         Gtk_New (View.Delete_Button);
         View.Buttons.Pack_Start
         (  View.Delete_Button,
            False,
            False
         );
         View.Delete_Button.Set_Sensitive (False);
         Handlers.Connect
         (  View.Delete_Button,
            "clicked",
            Delete'Access,
            Widget.all'Access
         );
      end if;
      Root_Changed (View);
   end Initialize;

   function Is_Feature
            (  View     : not null access Feature_Panel_Record;
               Selected : Selection
            )  return Boolean is
   begin
      return
      (  View.Is_Current
      and then
         Selected'Length = 1
      and then
         Is_Prefix
         (  Prefix => Fuzzy.Feature.Feature_Class,
            Source => Get_Class
                      (  Get_Directory_Cache (View.List),
                         Get_Path (View.List, Selected (Selected'First))
      )  )            );
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Is_Feature")
         )  );
         return False;
   end Is_Feature;

   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Feature_Create_Box
             )  is
      Name : constant UTF8_String := Get_Text (Item.Name_Edit);
   begin
      Check_Name (Name);
      Set_Hint (Item.Name_Hint, Item.Browser, Checked, True);
   exception
      when others =>
         Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
   end Name_Changed;

   procedure Move
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Tool : Renaming_Tool;
   begin
      Gtk_New
      (  Item    => Tool,
         Mode    => Features_List,
         Browser => Browser
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move")
         )  );
   end Move;

   procedure Rename
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      declare
         Tool : Renaming;
         View : constant Gtk_Persistent_Storage_Items_View :=
                    Get_Current (Browser).List.all'Unchecked_Access;
         Selected : constant Selection := Get_Selection (View);
      begin
         if Selected'Length = 1 then
            Gtk_New
            (  Tool,
               Browser,
               View.Get_Name (Selected (Selected'First))
            );
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rename")
         )  );
   end Rename;

   procedure Root_Changed
             (  View : not null access Feature_Panel_Record
             )  is
   begin
      if View.Buttons /= null then
         View.Add_Button.Set_Sensitive
         (  View.List.Browser.Tree.Get_Selection.Count_Selected_Rows = 1
         );
         View.Copy_Button.Set_Sensitive   (False);
         View.View_Button.Set_Sensitive   (False);
         View.Delete_Button.Set_Sensitive (False);
         View.Move_Button.Set_Sensitive   (False);
         View.Rename_Button.Set_Sensitive (False);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Root_Changed")
         )  );
   end Root_Changed;

   procedure Selection_Changed
             (  View : not null access Feature_Panel_Record
             )  is
   begin
      declare
         Selected : constant Selection := Get_Selection (View.List);
         Feature  : constant Boolean   := View.Is_Feature (Selected);
      begin
         Selection_Changed (Panel_Record (View.all)'Access);
         if View.Buttons /= null then
            View.Add_Button.Set_Sensitive
            (  View.List.Browser.Tree.Get_Selection.Count_Selected_Rows
            =  1
            );
            View.Copy_Button.Set_Sensitive   (Feature);
            View.View_Button.Set_Sensitive   (Feature);
            View.Delete_Button.Set_Sensitive (Selected'Length > 0);
            View.Move_Button.Set_Sensitive   (Selected'Length > 0);
            View.Rename_Button.Set_Sensitive (Selected'Length = 1);
         end if;
         if View.View.Is_Valid then
            -- A viewer panel is active
            if Feature then
               -- Change the feature being viewed
               Show (View, View.List.Browser.all'Unchecked_Access);
            else
               -- Delete the panel
               View.View.Get.Delete;
            end if;
         end if;
         View.State_Changed;
      end;
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

   procedure Set
             (  Item    : not null access Feature_View_Box_Record;
                Feature : Feature_Handle
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      if Item.Factory /= null then
         Remove (Item, Item.Factory);
         Item.Factory := null;
      end if;
      Gtk_New
      (  Widget         => Item.Factory,
         Feature        => Feature,
         Editable       => False,
         Feature_Picker => Item.Browser.Features.all'Unchecked_Access,
         Classifier_Picker =>
            Item.Browser.Classifiers.all'Unchecked_Access
      );
      Item.Factory.Set_Spacing
      (  Button => Button_Spacing,
         Column => Column_Spacing,
         Row    => Row_Spacing
      );
      Item.Pack_Start (Item.Factory);
      Item.Factory.Show_All;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Set")
         )  );
   end Set;

   procedure Show
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      declare
         View     : constant Feature_Panel :=
                             Feature_Panel (Browser.Features);
         Selected : constant Selection := View.List.Get_Selection;
         Store    : Storage_Handle;
         Feature  : Feature_Handle;
         Viewer   : Feature_View_Box;
      begin
         View.Get_Current (Selected, Store, Feature);
         if Feature.Is_Valid then
            declare
               Title : constant UTF8_String :=
                          UTF8_String
                          (  View.List.Get_Cache.Get_Name
                             (  View.List.Get_Path
                                (  Selected (Selected'First)
                          )  )  );
            begin
               if View.View.Is_Valid then
                  View.View.Get.Set (Feature);
                  View.View.Get.Set_Title (Title);
               else
                  Gtk_New (Viewer, Feature, Title, Browser);
                  View.View.Set (Viewer);
               end if;
            end;
         else
            if View.View.Is_Valid then
               View.View.Get.Delete;
            end if;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Show")
         )  );
   end Show;

   procedure Style_Updated
             (  View : not null access Feature_Panel_Record
             )  is
   begin
      View.Tab_Label.Set_Text
      (  Style_Get (View.List.Browser, "tab-feature-label")
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Style set: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

   procedure Tree_Selection_Changed
             (  View : not null access Feature_Panel_Record
             )  is
   begin
      if View.Buttons /= null then
         View.Add_Button.Set_Sensitive
         (  View.List.Browser.Tree.Get_Selection.Count_Selected_Rows = 1
         );
      end if;
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

end Gtk.Fuzzy_Catalogue.Feature_Pane;
