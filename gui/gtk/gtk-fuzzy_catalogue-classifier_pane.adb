--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Classifier_Pane                        Winter, 2008       --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Deposit_Handles;                use Deposit_Handles;
with Fuzzy.Classifier;               use Fuzzy.Classifier;
with Fuzzy.Classifier.Handle;        use Fuzzy.Classifier.Handle;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Fuzzy_Catalogue.Renamings;  use Gtk.Fuzzy_Catalogue.Renamings;
with Gtk.Separator;                  use Gtk.Separator;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Strings_Edit;                   use Strings_Edit;

with Gtk.Fuzzy_Catalogue.Renaming_Tools;
use  Gtk.Fuzzy_Catalogue.Renaming_Tools;

with Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes;
use  Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes;

with Gtk.Fuzzy_Catalogue.Learn_Boxes;
use  Gtk.Fuzzy_Catalogue.Learn_Boxes;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Classifier_Pane is
   use Classifications;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Classifier_Pane." & Name;
   end Where;

   procedure Add
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      declare
         Box  : Learn_Box;
         View : Classifier_Panel :=
                   Classifier_Panel (Browser.Classifiers);
      begin
         Gtk_New (Box, Browser, False);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add")
         )  );
   end Add;

   procedure Build_Item_Menu
             (  View  : not null access Classifier_Panel_Record;
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
      if Is_Classifier (View, (1 => Index)) then
         Gtk_New (Item, "menu-view", View.View_Button, Browser);
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Show'Access,
            View.List.Browser
         );
         Gtk_New
         (  Item,
            "menu-classify-input",
            View.Classify_Input_Button,
            Browser
         );
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Classify_Input'Access,
            View.List.Browser
         );
         Gtk_New (Item, "menu-classify", View.Classify_Button, Browser);
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Classify'Access,
            View.List.Browser
         );
         Gtk_New (Item, "menu-verify", View.Verify_Button, Browser);
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Verify'Access,
            View.List.Browser
         );
      end if;
      Gtk_New (Item, "menu-move", View.Move_Button, Browser);
      Append (Menu, Item);
      Handlers.Connect
      (  Item,
         "activate",
         Move'Access,
         View.List.Browser
      );
      Gtk_New (Item, "menu-rename", View.Rename_Button, Browser);
      Append (Menu, Item);
      Handlers.Connect
      (  Item,
         "activate",
         Rename'Access,
         View.List.Browser
      );
      Gtk_New (Item, "menu-delete", View.Delete_Button, Browser);
      Append (Menu, Item);
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
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Build_Item_Menu")
         )  );
   end Build_Item_Menu;

   procedure Check
             (  View       : not null access Classifier_Panel_Record;
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
      if (  Is_Valid (Object)
         and then
            Ptr (Object).all in Classifier_Object'Class
         )
      then
         if not Check (Constraint, Store) then
            raise Picking_Error;
         end if;
         return;
      end if;
      Invalidate (Store);
      Invalidate (Object);
   exception
      when Picking_Error =>
         Invalidate (Store);
         Invalidate (Object);
         raise;
      when others =>
         Invalidate (Store);
         Invalidate (Object);
   end Check;

   procedure Classify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View : constant Classifier_Panel :=
                      Classifier_Panel (Browser.Classifiers);
   begin
      if Is_Valid (View.Classify) then
         Browser.Set_Item (View.Classify.Get);
      else
         declare
            Box : Classification_Box;
         begin
            Gtk_New
            (  Item           => Box,
               Browser        => Browser,
               Verify         => False,
               Set_Classifier => True,
               Set_Lecture    => False
            );
            Set (View.Classify, Box);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Classify")
         )  );
   end Classify;

   procedure Classify_Input
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View : Classifier_Panel := Classifier_Panel (Browser.Classifiers);
      Box  : Interactive_Classification_Box;
   begin
      Gtk_New
      (  Item           => Box,
         Browser        => Browser,
         Set_Classifier => True
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Classify_Input")
         )  );
   end Classify_Input;

   procedure Delete
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      Delete
      (  Widget,
         Classifier_Panel (Browser.Classifiers).all'Unchecked_Access,
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
             (  View       : not null access Classifier_Panel_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      if Is_Current (View.all'Access) then
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
      Invalidate (Store);
      Invalidate (Object);
   exception
      when others =>
         Invalidate (Store);
         Invalidate (Object);
   end Get;

   function Get
            (  View       : not null access Classifier_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is
      Selected : constant Selection := Get_Selection (View.List);
   begin
      if Is_Classifier (View, Selected) then
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
      when others =>
         return "";
   end Get;

   function Get_Current
            (  View     : not null access Classifier_Panel_Record;
               Selected : Selection
            )  return Classifier_Handle is
      Result : Classifier_Handle;
   begin
      if Is_Current (View.all'Access) and then Selected'Length = 1 then
         declare
            Storage : Storage_Handle;
            Object  : Deposit_Handle;
         begin
            Browse
            (  Get_Directory_Cache (View.List),
               Get_Path (View.List, Selected (Selected'First)),
               Storage,
               Object
            );
            if Ptr (Object).all in Classifier_Object'Class then
               Result := Ref (To_Classifier_Object_Ptr (Ptr (Object)));
            end if;
         exception
            when Reason : others =>
               Fault (View.List.Browser, Exception_Message (Reason));
         end;
      end if;
      return Result;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Current")
         )  );
         return Result;
   end Get_Current;

   procedure Gtk_New
             (  Item       : out Classifier_View_Box;
                Classifier : Classifier_Handle;
                Title      : UTF8_String;
                Browser    : not null access
                             Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Classifier_View_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Classifier, Title);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Classifier_View_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  View      : out Classifier_Panel;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             )  is
   begin
      View := new Classifier_Panel_Record;
      begin
         Gtk.Fuzzy_Catalogue.Classifier_Pane.Initialize
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
               &  Where ("Gtk_New (Classifier_Panel)")
            )  );
            GLib.Object.Checked_Destroy (View);
            View := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item       : not null access
                             Classifier_View_Box_Record'Class;
                Classifier : Classifier_Handle;
                Title      : UTF8_String
             )  is
   begin
      Initialize (Item, "classifier view", False);
      Gtk_New (Item.Classifier);
      Put (Item.Classifier, Classifier);
      Pack_Start (Item, Item.Classifier);

      Show_All (Item);

      Add_Item
      (  Item.Browser,
         Title,
         Style_Get (Item.Browser, "tab-view-classifier-icon"),
         Item
      );
   end Initialize;

   procedure Initialize
             (  View      : not null access
                            Classifier_Panel_Record'Class;
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
         Mode      => Classifiers_List,
         List_Size => List_Size
      );

      if Buttons then
         View.Buttons.Set_Spacing (GInt (Button_Spacing));
         Gtk_New (View.View_Button);
         Set_Sensitive (View.View_Button, False);
         Pack_Start
         (  View.Buttons,
            View.View_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.View_Button,
            "clicked",
            Show'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         Pack_Start
         (  View.Buttons,
            Separator,
            False,
            False
         );

         Gtk_New (View.Classify_Input_Button);
         Set_Sensitive (View.Classify_Input_Button, False);
         Pack_Start
         (  View.Buttons,
            View.Classify_Input_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Classify_Input_Button,
            "clicked",
            Classify_Input'Access,
            Widget.all'Access
         );

         Gtk_New (View.Classify_Button);
         Set_Sensitive (View.Classify_Button, False);
         Pack_Start
         (  View.Buttons,
            View.Classify_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Classify_Button,
            "clicked",
            Classify'Access,
            Widget.all'Access
         );

         Gtk_New (View.Verify_Button);
         Set_Sensitive (View.Verify_Button, False);
         Pack_Start
         (  View.Buttons,
            View.Verify_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Verify_Button,
            "clicked",
            Verify'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         Pack_Start
         (  View.Buttons,
            Separator,
            False,
            False
         );

         Gtk_New (View.Add_Button);
         Pack_Start
         (  View.Buttons,
            View.Add_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Add_Button,
            "clicked",
            Add'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         Pack_Start
         (  View.Buttons,
            Separator,
            False,
            False
         );

         Gtk_New (View.Move_Button);
         Pack_Start
         (  View.Buttons,
            View.Move_Button,
            False,
            False
         );
         Set_Sensitive (View.Move_Button, False);
         Handlers.Connect
         (  View.Move_Button,
            "clicked",
            Move'Access,
            Widget.all'Access
         );

         Gtk_New (View.Rename_Button);
         Pack_Start
         (  View.Buttons,
            View.Rename_Button,
            False,
            False
         );
         Set_Sensitive (View.Rename_Button, False);
         Handlers.Connect
         (  View.Rename_Button,
            "clicked",
            Rename'Access,
            Widget.all'Access
         );

         Gtk_New (View.Delete_Button);
         Pack_Start
         (  View.Buttons,
            View.Delete_Button,
            False,
            False
         );
         Set_Sensitive (View.Delete_Button, False);
         Handlers.Connect
         (  View.Delete_Button,
            "clicked",
            Delete'Access,
            Widget.all'Access
         );
      end if;
      Root_Changed (View);
   end Initialize;

   function Is_Classifier
            (  View     : not null access Classifier_Panel_Record;
               Selected : Selection
            )  return Boolean is
   begin
      return
      (  Is_Current (View.all'Access)
      and then
         Selected'Length = 1
      and then
         Is_Prefix
         (  Prefix => Fuzzy.Classifier.Classifier_Class,
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
            Log_Level_Warning,
            (  "Selection test: " & Exception_Information (Error)
            &  Where ("Is_Classifier")
         )  );
         return False;
   end Is_Classifier;

   procedure Move
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Tool : Renaming_Tool;
   begin
      Gtk_New
      (  Item    => Tool,
         Mode    => Classifiers_List,
         Browser => Browser
      );
   end Move;

   procedure Rename
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Tool     : Renaming;
      View     : constant Gtk_Persistent_Storage_Items_View :=
                    Get_Current (Browser).List.all'Unchecked_Access;
      Selected : constant Selection := Get_Selection (View);
   begin
      if Selected'Length = 1 then
         Gtk_New
         (  Tool,
            Browser,
            Get_Name (View, Selected (Selected'First))
         );
      end if;
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
             (  View : not null access Classifier_Panel_Record
             )  is
   begin
      Set_Sensitive
      (  View.Add_Button,
         Get_Directory (View.List)'Length /= 0
      );
      if View.Buttons /= null then
         Set_Sensitive (View.Classify_Input_Button, False);
         Set_Sensitive (View.Classify_Button,       False);
         Set_Sensitive (View.View_Button,           False);
         Set_Sensitive (View.Delete_Button,         False);
         Set_Sensitive (View.Move_Button,           False);
         Set_Sensitive (View.Rename_Button,         False);
         Set_Sensitive (View.Verify_Button,         False);
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
             (  View : not null access Classifier_Panel_Record
             )  is
      Selected   : constant Selection := Get_Selection (View.List);
      Classifier : constant Boolean := Is_Classifier (View, Selected);
   begin
      Selection_Changed (Panel_Record (View.all)'Access);
      if View.Buttons /= null then
         Set_Sensitive (View.Classify_Input_Button, Classifier);
         Set_Sensitive (View.Classify_Button,       Classifier);
         Set_Sensitive (View.View_Button,           Classifier);
         Set_Sensitive (View.Verify_Button,         Classifier);
         Set_Sensitive (View.Delete_Button, Selected'Length > 0);
         Set_Sensitive (View.Move_Button,   Selected'Length > 0);
         Set_Sensitive (View.Rename_Button, Selected'Length = 1);
      end if;
      if View.View.Is_Valid then
         -- A viewer panel is active
         if Classifier then
            -- Change the classifier being viewed
            Show (View, View.List.Browser.all'Unchecked_Access);
         else
            -- Delete the panel
            Delete (View.View.Get);
         end if;
      end if;
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

   procedure Show
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View       : constant Classifier_Panel :=
                            Classifier_Panel (Browser.Classifiers);
      Selected   : constant Selection := View.List.Get_Selection;
      Classifier : constant Classifier_Handle :=
                            View.Get_Current (Selected);
   begin
      if View.View.Is_Valid then
         Delete (View.View.Get);
      end if;
      if Classifier.Is_Valid then
         declare
            Viewer : Classifier_View_Box;
         begin
            Gtk_New
            (  Viewer,
               Classifier,
               UTF8_String
               (  Get_Name
                  (  Get_Cache (View.List),
                     Get_Path (View.List, Selected (Selected'First))
               )  ),
               Browser
            );
            Set (View.View, Viewer);
         end;
      end if;
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
             (  View : not null access Classifier_Panel_Record
             )  is
   begin
      Set_Text
      (  View.Tab_Label,
         Style_Get (View.List.Browser, "tab-classifier-label")
      );
   end Style_Updated;

   procedure Verify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View : Classifier_Panel := Classifier_Panel (Browser.Classifiers);
      Box  : Classification_Box;
   begin
      Gtk_New
      (  Item           => Box,
         Browser        => Browser,
         Verify         => True,
         Set_Classifier => True,
         Set_Lecture    => False
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Verify")
         )  );
   end Verify;

end Gtk.Fuzzy_Catalogue.Classifier_Pane;
