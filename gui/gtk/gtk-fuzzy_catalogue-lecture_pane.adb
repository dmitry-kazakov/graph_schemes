--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Pane           Luebeck            --
--  Implementation                                 Winter, 2008       --
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

with Deposit_Handles;                use Deposit_Handles;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Fuzzy_Catalogue.Renamings;  use Gtk.Fuzzy_Catalogue.Renamings;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Fuzzy.Lecture;                  use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Strings_Edit;                   use Strings_Edit;

with Gtk.Fuzzy_Catalogue.Lecture_Edit;
use  Gtk.Fuzzy_Catalogue.Lecture_Edit;

with Gtk.Fuzzy_Catalogue.Renaming_Tools;
use  Gtk.Fuzzy_Catalogue.Renaming_Tools;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;
with Gtk.Fuzzy_Catalogue.Learn_Boxes;
with Gtk.Fuzzy_Catalogue.Lecture_Copy;

package body Gtk.Fuzzy_Catalogue.Lecture_Pane is
   use Classifications;
   use Learn_Boxes;
   use Lecture_Copy;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Lecture_Pane." & Name;
   end Where;

   procedure Add
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Editor : Lecture_Edit_Box;
   begin
      Gtk_New (Editor, "", Browser);
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

   function Are_Two_Lectures
            (  View     : access Lecture_Panel_Record;
               Selected : Selection
            )  return Boolean is
   begin
      return
      (  Is_Current (View.all'Access)
      and then
         Selected'Length = 2
      and then
         (  Is_Prefix
            (  Prefix => Fuzzy.Lecture.Lecture_Class,
               Source =>
                  Get_Class
                  (  Get_Directory_Cache (View.List),
                     Get_Path (View.List, Selected (Selected'First))
            )     )
         and then
            Is_Prefix
            (  Prefix => Fuzzy.Lecture.Lecture_Class,
               Source =>
                  Get_Class
                  (  Get_Directory_Cache (View.List),
                     Get_Path (View.List, Selected (Selected'Last))
      )  )  )     );
   exception
      when Ada.IO_Exceptions.Name_Error =>
         return False;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Are_Two_Lectures")
         )  );
         return False;
   end Are_Two_Lectures;

   procedure Build_Item_Menu
             (  View  : not null access Lecture_Panel_Record;
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
      if Is_Lecture (View, (1 => Index)) then
         Gtk_New (Item, "menu-view", View.View_Button, Browser);
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Show'Access,
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
         Gtk_New (Item, "menu-learn", View.Learn_Button, Browser);
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Learn'Access,
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
         Gtk_New
         (  Item,
            "menu-save-as-file",
             View.Save_As_Text_Button,
            Browser
         );
         Append (Menu, Item);
         Handlers.Connect
         (  Item,
            "activate",
            Save_As_Text'Access,
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
             (  View       : not null access Lecture_Panel_Record;
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
            Ptr (Object).all in Lecture_Object'Class
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
      View : constant Lecture_Panel := Lecture_Panel (Browser.Lectures);
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
               Set_Classifier => False,
               Set_Lecture    => True
            );
            View.Classify.Set (Box);
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

   procedure Compare
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View     : constant Lecture_Panel :=
                          Lecture_Panel (Browser.Lectures);
      Selected : constant Selection := Get_Selection (View.List);
   begin
      if View.Compare.Is_Valid then
         Delete (View.Compare.Get);
      end if;
      if Selected'Length = 2 then
         declare
            Viewer  : Lecture_Compare_Box;
            Storage : Storage_Handle;
            Object  : Deposit_Handle;
            First   : Lecture_Handle;
            Second  : Lecture_Handle;
         begin
            Get_Directory_Cache (View.List).Browse
            (  Get_Path (View.List, Selected (Selected'First)),
               Storage,
               Object
            );
            if Ptr (Object).all in Lecture_Object'Class then
               First := Ref (To_Lecture_Object_Ptr (Ptr (Object)));
            end if;
            Get_Directory_Cache (View.List).Browse
            (  Get_Path (View.List, Selected (Selected'Last)),
               Storage,
               Object
            );
            if Object.Ptr.all in Lecture_Object'Class then
               Second := Ref (To_Lecture_Object_Ptr (Ptr (Object)));
            end if;
            Gtk_New (Viewer, First, Second, Browser);
            View.Compare.Set (Viewer);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Compare")
         )  );
   end Compare;

   procedure Copy
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View        : constant Lecture_Panel :=
                             Lecture_Panel (Browser.Lectures);
      Selected    : constant Selection := Get_Selection (View.List);
      Store       : Storage_Handle;
      Lesson      : Lecture_Handle;
      Constructor : Lecture_Copy_Box;
   begin
      Get_Current (View, Selected, Store, Lesson);
      Gtk_New (Constructor, Store, Lesson, Browser);
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
         Lecture_Panel (Browser.Lectures).all'Unchecked_Access,
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

   procedure Edit
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View     : constant Lecture_Panel :=
                          Lecture_Panel (Browser.Lectures);
      Selected : constant Selection := Get_Selection (View.List);
      Store    : Storage_Handle;
      Lesson   : Lecture_Handle;
      Editor   : Lecture_Edit_Box;
   begin
      Get_Current (View, Selected, Store, Lesson);
      if Is_Valid (Lesson) then
         Gtk_New
         (  Editor,
            String (Get (View, Unconstrained)),
            Store,
            Lesson,
            Browser
         );
      else
         Gtk_New
         (  Editor,
            String (Get (View, Unconstrained)),
            Browser
         );
      end if;
   exception
      when Ada.IO_Exceptions.End_Error => -- Action aborted by user
         Delete (Editor);
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit")
         )  );
   end Edit;

   procedure Get
             (  View       : not null access Lecture_Panel_Record;
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
            (  View       : not null access Lecture_Panel_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is
      Selected : constant Selection := Get_Selection (View.List);
   begin
      if Is_Lecture (View, Selected) then
         declare
            Store  : Storage_Handle;
            Object : Deposit_Handle;
            Result : constant Item_Path :=
                        Get_Path (View.List, Selected (Selected'First));
         begin
            Check (View, Result, Constraint, Store, Object);
            return Result;
         end;
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
            &  Where ("Get")
         )  );
         return "";
   end Get;

   procedure Get_Current
             (  View     : not null access Lecture_Panel_Record;
                Selected : Selection;
                Store    : out Storage_Handle;
                Lesson   : out Lecture_Handle
             )  is
   begin
      if Is_Current (View.all'Access) and then Selected'Length = 1 then
         declare
            Object : Deposit_Handle;
         begin
            Browse
            (  Get_Directory_Cache (View.List),
               Get_Path (View.List, Selected (Selected'First)),
               Store,
               Object
            );
            if Ptr (Object).all in Lecture_Object'Class then
               Lesson := Ref (To_Lecture_Object_Ptr (Ptr (Object)));
            else
               Invalidate (Store);
               Invalidate (Lesson);
            end if;
         exception
            when Reason : others =>
               Fault (View.List.Browser, Exception_Message (Reason));
               Invalidate (Store);
               Invalidate (Lesson);
         end;
      else
         Invalidate (Store);
         Invalidate (Lesson);
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
         Invalidate (Store);
         Invalidate (Lesson);
   end Get_Current;

   procedure Get_From_Text
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      use Lecture_Create_From_Text_Boxes;
      View : constant Lecture_Panel := Lecture_Panel (Browser.Lectures);
   begin
      if Is_Valid (View.Create_From_Text) then
         Browser.Set_Item (View.Create_From_Text.Get);
      else
         declare
            Constructor : Lecture_Create_From_Text_Box;
         begin
            Gtk_New (Constructor, Browser);
            View.Create_From_Text.Set (Constructor);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_From_Text")
         )  );
   end Get_From_Text;

   procedure Gtk_New
             (   Item    : out Lecture_Compare_Box;
                 First   : Lecture_Handle;
                 Second  : Lecture_Handle;
                 Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_Compare_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, First, Second);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Lecture_Compare_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Item    : out Lecture_View_Box;
                Lesson  : Lecture_Handle;
                Title   : UTF8_String;
                Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_View_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Lesson, Title);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Lecture_View_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  View      : out Lecture_Panel;
                Widget    : not null access
                            Gtk_Fuzzy_Catalogue_Record'Class;
                Columns   : Positive;
                Vertical  : Boolean;
                Buttons   : Boolean;
                List_Size : Gtk_Requisition
             )  is
   begin
      View := new Lecture_Panel_Record;
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
               &  Where ("Gtk_New (Lecture_Panel)")
            )  );
            GLib.Object.Checked_Destroy (View);
            View := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item   : access Lecture_Compare_Box_Record'Class;
                First  : Lecture_Handle;
                Second : Lecture_Handle
             )  is
      Wait : Wait_Cursor (Item.Browser);
      Size : GUInt := Style_Get (Item.Browser, "lecture-cache");
   begin
      Initialize (Item, "sets compare box", False);
      Gtk_New (Item.Lesson);
      Put
      (  Widget    => Item.Lesson,
         Reference => First,
         Result    => Second,
         Symmetric => False
      );
      Pack_Start (Item, Item.Lesson);

      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-difference-lecture-label"),
         Style_Get (Item.Browser, "tab-difference-lecture-icon"),
         Item
      );
   end Initialize;

   procedure Initialize
             (  Item   : access Lecture_View_Box_Record'Class;
                Lesson : Lecture_Handle;
                Title  : UTF8_String
             )  is
      Wait : Wait_Cursor (Item.Browser);
      Size : constant GUInt :=
                      Style_Get (Item.Browser, "lecture-cache");
   begin
      Initialize (Item, "set view box", False);
      Gtk_New (Item.Lesson);
      Put
      (  Widget => Item.Lesson,
         Lesson => Lesson,
         Size   => Positive (Size)
      );
      Pack_Start (Item, Item.Lesson);

      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Title,
         Style_Get (Item.Browser, "tab-view-lecture-icon"),
         Item
      );
   end Initialize;

   procedure Initialize
             (  View      : not null access Lecture_Panel_Record'Class;
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
         Mode      => Sets_List,
         List_Size => List_Size
      );

      if Buttons then
         Gtk_New (View.View_Button);
         View.Buttons.Set_Spacing (GInt (Button_Spacing));
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

         Gtk_New (View.Diff_Button);
         Pack_Start
         (  View.Buttons,
            View.Diff_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Diff_Button,
            "clicked",
            Compare'Access,
            Widget.all'Access
         );

         Gtk_New_Vseparator (Separator);
         Pack_Start
         (  View.Buttons,
            Separator,
            False,
            False
         );

         Gtk_New (View.Classify_Button);
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

         Gtk_New (View.Learn_Button);
         Pack_Start
         (  View.Buttons,
            View.Learn_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Learn_Button,
            "clicked",
            Learn'Access,
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

         Gtk_New (View.Edit_Button);
         Set_Sensitive (View.Edit_Button, False);
         Pack_Start
         (  View.Buttons,
            View.Edit_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Edit_Button,
            "clicked",
            Edit'Access,
            Widget.all'Access
         );

         Gtk_New (View.Copy_Button);
         Pack_Start
         (  View.Buttons,
            View.Copy_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Copy_Button,
            "clicked",
            Copy'Access,
            Widget.all'Access
         );

         Gtk_New (View.Get_From_Text_Button);
         Pack_Start
         (  View.Buttons,
            View.Get_From_Text_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Get_From_Text_Button,
            "clicked",
            Get_From_Text'Access,
            Widget.all'Access
         );

         Gtk_New (View.Save_As_Text_Button);
         Pack_Start
         (  View.Buttons,
            View.Save_As_Text_Button,
            False,
            False
         );
         Handlers.Connect
         (  View.Save_As_Text_Button,
            "clicked",
            Save_As_Text'Access,
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
         Handlers.Connect
         (  View.Delete_Button,
            "clicked",
            Delete'Access,
            Widget.all'Access
         );
      end if;
      Root_Changed (View);
   end Initialize;

   function Is_Lecture
            (  View     : not null access Lecture_Panel_Record;
               Selected : Selection
            )  return Boolean is
   begin
      return
      (  Is_Current (View.all'Access)
      and then
         Selected'Length = 1
      and then
         Is_Prefix
         (  Prefix => Fuzzy.Lecture.Lecture_Class,
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
            (  "Selection test: "
            &  Exception_Information (Error)
            &  Where ("Is_Lecture")
         )  );
         return False;
   end Is_Lecture;

   procedure Learn
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
   begin
      declare
         Box : Learn_Box;
         View : Lecture_Panel := Lecture_Panel (Browser.Lectures);
      begin
         Gtk_New (Box, Browser, True);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Learn")
         )  );
   end Learn;

   procedure Move
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      Tool : Renaming_Tool;
   begin
      Gtk_New
      (  Item    => Tool,
         Mode    => Sets_List,
         Browser => Browser
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move")
         )  );
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
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Rename")
         )  );
   end Rename;

   procedure Root_Changed
             (  View : not null access Lecture_Panel_Record
             )  is
      Can_Create : constant Boolean :=
                            View.List.Get_Directory'Length /= 0;
   begin
      if View.Buttons /= null then
         Set_Sensitive (View.Add_Button,           Can_Create);
         Set_Sensitive (View.Get_From_Text_Button, Can_Create);
         Set_Sensitive (View.Classify_Button,      False);
         Set_Sensitive (View.Copy_Button,          False);
         Set_Sensitive (View.Delete_Button,        False);
         Set_Sensitive (View.Diff_Button,          False);
         Set_Sensitive (View.Edit_Button,          False);
         Set_Sensitive (View.Learn_Button,         False);
         Set_Sensitive (View.Move_Button,          False);
         Set_Sensitive (View.Rename_Button,        False);
         Set_Sensitive (View.Save_As_Text_Button,  False);
         Set_Sensitive (View.Verify_Button,        False);
         Set_Sensitive (View.View_Button,          False);
      end if;
      if View.View.Is_Valid then
         -- Delete the panel
         Delete (View.View.Get);
      end if;
      if View.Compare.Is_Valid then
         -- Delete the panel
         Delete (View.Compare.Get);
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

   procedure Save_As_Text
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      use Lecture_Save_As_Text_Boxes;
      View : constant Lecture_Panel :=
                      Lecture_Panel (Browser.Lectures);
   begin
      if Is_Valid (View.Save_As_Text) then
         Browser.Set_Item (View.Save_As_Text.Get);
      else
         declare
            Constructor : Lecture_Save_As_Text_Box;
         begin
            Gtk_New (Constructor, Browser);
            View.Save_As_Text.Set (Constructor);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save_As_Text")
         )  );
   end Save_As_Text;

   procedure Selection_Changed
             (  View : not null access Lecture_Panel_Record
             )  is
      Selected : constant Selection := Get_Selection (View.List);
      Lesson   : constant Boolean   := Is_Lecture (View, Selected);
   begin
      Selection_Changed (Panel_Record (View.all)'Access);
      if View.Buttons /= null then
         Set_Sensitive (View.Classify_Button,     Lesson);
         Set_Sensitive (View.Copy_Button,         Lesson);
         Set_Sensitive (View.Learn_Button,        Lesson);
         Set_Sensitive (View.Save_As_Text_Button, Lesson);
         Set_Sensitive (View.Verify_Button,       Lesson);
         Set_Sensitive (View.View_Button,         Lesson);
         Set_Sensitive (View.Delete_Button, Selected'Length > 0);
         Set_Sensitive (View.Move_Button,   Selected'Length > 0);
         Set_Sensitive (View.Rename_Button, Selected'Length = 1);
         Set_Sensitive (View.Edit_Button,   Selected'Length = 1);
         Set_Sensitive
         (  View.Diff_Button,
            Are_Two_Lectures (View, Selected)
         );
      end if;
      if View.View.Is_Valid then
         -- A viewer panel is active
         if Lesson then
            -- Change the feature being viewer
            Show (View, View.List.Browser.all'Unchecked_Access);
         else
            -- Delete the panel
            Delete (View.View.Get);
         end if;
      end if;
      if View.Compare.Is_Valid then -- Delete the panel
         Delete (View.Compare.Get);
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
      View     : constant Lecture_Panel :=
                          Lecture_Panel (Browser.Lectures);
      Selected : constant Selection := Get_Selection (View.List);
      Store    : Storage_Handle;
      Lesson   : Lecture_Handle;
   begin
      Get_Current (View, Selected, Store, Lesson);
      if View.View.Is_Valid then
         Delete (View.View.Get);
      end if;
      if Is_Valid (Lesson) then
         declare
            Viewer : Lecture_View_Box;
         begin
            Gtk_New
            (  Viewer,
               Lesson,
               UTF8_String
               (  Get_Name
                  (  Get_Cache (View.List),
                     Get_Path (View.List, Selected (Selected'First))
               )  ),
               Browser
            );
            View.View.Set (Viewer);
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
             (  View : not null access Lecture_Panel_Record
             )  is
   begin
      View.Tab_Label.Set_Text
      (  Style_Get (View.List.Browser, "tab-lecture-label")
      );
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

   procedure Verify
             (  Widget  : access Gtk_Widget_Record'Class;
                Browser : Gtk_Fuzzy_Catalogue
             )  is
      View : constant Lecture_Panel := Lecture_Panel (Browser.Lectures);
   begin
      if View.Verify.Is_Valid then
         Browser.Set_Item (View.Verify.Get);
      else
         declare
            Box : Classification_Box;
         begin
            Gtk_New
            (  Item           => Box,
               Browser        => Browser,
               Verify         => True,
               Set_Classifier => False,
               Set_Lecture    => True
            );
            View.Verify.Set (Box);
         end;
      end if;
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

end Gtk.Fuzzy_Catalogue.Lecture_Pane;
