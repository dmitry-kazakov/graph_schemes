--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Copy           Luebeck            --
--  Implementation                                 Summer, 2009       --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Glib.Messages;                 use Glib.Messages;
with Gtk.Widget.Styles;             use Gtk.Widget.Styles;
with Fuzzy.Lecture;                 use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Factory;  use Fuzzy.Lecture.Handle.Factory;
with Fuzzy.Persistence;             use Fuzzy.Persistence;
with Name_Tables;                   use Name_Tables;
with Strings_Edit.Integers;         use Strings_Edit.Integers;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Lecture_Copy is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Lecture_Copy." & Name;
   end Where;

   procedure Cancel
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             )  is
   begin
      Invalidate (Item.Result);
      Set_Editable (Item.Name_Edit,   True);
      Set_Editable (Item.Source_Name, True);
      Set_Editable (Item.Folder_Name, True);
      Set_Editable (Item.From_Edit,   True);
      Set_Editable (Item.To_Edit,     True);
   end Cancel;

   procedure Commit (Item : not null access Lecture_Copy_Box_Record) is
   begin
      Item.Source :=
         Get_Lecture
         (  Get_Path (Item.Source_Name),
            Item,
            Item.Source_Name_Hint
         );
      if not Is_Valid (Item.Source) then
         return;
      end if;
      begin
         Item.From := Value (Get_Text (Item.From_Edit));
         if Item.From in 1..Get_Examples_Number (Item.Source) then
            Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
         else
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-range-error")
            );
            return;
         end if;
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-range-error")
            );
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            return;
         when others =>
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-error")
            );
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            return;
      end;
      begin
         Item.From := Value (Get_Text (Item.From_Edit));
         if Item.From in 1..Get_Examples_Number (Item.Source) then
            Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
         else
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-range-error")
            );
            return;
         end if;
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-range-error")
            );
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            return;
         when others =>
            Error
            (  Item,
               Style_Get (Item.Browser, "from-example-error")
            );
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
            return;
      end;
      begin
         Item.To := Value (Get_Text (Item.To_Edit));
         if Item.To in 1..Get_Examples_Number (Item.Source) then
            if Item.To >= Item.From then
               Set_Hint (Item.To_Hint, Item.Browser, Checked, True);
            else
               Error
               (  Item,
                  Style_Get (Item.Browser, "examples-range-error")
               );
               Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
               Set_Hint (Item.To_Hint,   Item.Browser, Erroneous, True);
               return;
            end if;
         else
            Set_Hint (Item.To_Hint, Item.Browser, Erroneous, True);
            Error
            (  Item,
               Style_Get (Item.Browser, "to-example-range-error")
            );
            return;
         end if;
      exception
         when Constraint_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "to-example-range-error")
            );
            Set_Hint (Item.To_Hint, Item.Browser, Erroneous, True);
            return;
         when others =>
            Error
            (  Item,
               Style_Get (Item.Browser, "to-example-error")
            );
            Set_Hint (Item.To_Hint, Item.Browser, Erroneous, True);
            return;
      end;
      Get_Folder (Item.Folder_Name, Item.Store, Item.Folder);
      if not Is_Valid (Item.Store) then
         return;
      end if;
      if (  ""
         =  Check_New_Name
            (  Item,
               Item.Store,
               Item.Folder,
               Get_Text (Item.Name_Edit)
         )  )
      then
         Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
         return;
      end if;
      Item.Result := Create_Persistent (Item.Store);
      Set_Editable (Item.Source_Name, False);
      Set_Editable (Item.Name_Edit,   False);
      Set_Editable (Item.Folder_Name, False);
      Set_Editable (Item.From_Edit,   False);
      Set_Editable (Item.To_Edit,     False);
      Start_Servicing (Item);
   exception
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
   end Commit;

   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             )  is
      Lesson : constant Lecture_Handle := Get_Lecture (Item.Browser);
      From   : Integer;
   begin
      if Is_Valid (Lesson) then
         From := Value (Get_Text (Item.From_Edit));
         if From in 1..Get_Examples_Number (Lesson) then
            Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
         else
            Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
         end if;
      else
         Set_Hint (Item.From_Hint, Item.Browser, Erroneous, False);
      end if;
   exception
      when others =>
         Set_Hint (Item.From_Hint, Item.Browser, Erroneous, True);
   end From_Changed;

   procedure Gtk_New
             (  Item    : out Lecture_Copy_Box;
                Store   : Storage_Handle;
                Lesson  : Lecture_Handle;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_Copy_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Store, Lesson);
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
             (  Item   : not null access Lecture_Copy_Box_Record'Class;
                Store  : Storage_Handle;
                Lesson : Lecture_Handle
             )  is
      Label : Gtk_Label;
      Hint  : Gtk_Box;
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "copy box", True);
      Set (Item.Constraint, Store);
      Gtk_New (Item.Grid, 5, 10, False);
      Set_Col_Spacings (Item.Grid, Column_Spacing);
      Set_Row_Spacings (Item.Grid, Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "lecture-name-label"));
      Attach
      (  Item.Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);

      Gtk_New (Item.Name_Edit);
      Attach
      (  Item.Grid,
         Item.Name_Edit,
         1, 9,
         0, 1,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );

      Gtk_New_HBox (Item.Name_Hint);
      Attach
      (  Item.Grid,
         Item.Name_Hint,
         9, 10,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.Name_Hint, Item.Browser, None, True);
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "folder-path-label"));
      Attach
      (  Item.Grid,
         Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);

      Gtk_New (Item.Folder_Name, "set folder", Hint, Item);
      Combine (Item.Constraint, Get_Constraint (Item.Folder_Name));
      Attach
      (  Item.Grid,
         Item.Folder_Name,
         1, 9,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Attach
      (  Item.Grid,
         Hint,
         9, 10,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 3
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "copy-lecture-name-label")
      );
      Attach
      (  Item.Grid,
         Label,
         0, 1,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);

      Gtk_New
      (  Item.Source_Name,
         "source name",
         Item.Source_Name_Hint,
         Item.Browser.Lectures,
         True
      );
      Combine (Item.Constraint, Get_Constraint (Item.Source_Name));
      Attach
      (  Item.Grid,
         Item.Source_Name,
         1, 9,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Attach
      (  Item.Grid,
         Item.Source_Name_Hint,
         9, 10,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 4
      Gtk_New (Label, Style_Get (Item.Browser, "from-example-label"));
      Attach
      (  Item.Grid,
         Label,
         0, 1,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);

      Gtk_New (Item.From_Edit);
      Set_Text (Item.From_Edit, "1");
      Attach
      (  Item.Grid,
         Item.From_Edit,
         1, 2,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );

      Gtk_New_HBox (Item.From_Hint);
      Attach
      (  Item.Grid,
         Item.From_Hint,
         2, 3,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.From_Hint, Item.Browser, Checked, True);

      Gtk_New (Label, Style_Get (Item.Browser, "to-example-label"));
      Attach
      (  Item.Grid,
         Label,
         3, 4,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);

      Gtk_New (Item.To_Edit);
      Attach
      (  Item.Grid,
         Item.To_Edit,
         4, 5,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );

      Gtk_New_HBox (Item.To_Hint);
      Attach
      (  Item.Grid,
         Item.To_Hint,
         5, 6,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.To_Hint, Item.Browser, None, True);

      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "out-of-examples-label")
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Grid,
         Label,
         6, 7,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );

      Gtk_New (Item.Total, "?");
      Item.Total.Set_Halign (Align_Start);
      Item.Total.Set_Valign (Align_Center);
--    Set_Alignment (Item.Total, 0.0, 0.5);
      Attach
      (  Item.Grid,
         Item.Total,
         7, 8,
         3, 4,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );

      Pack_Start (Item, Item.Grid, False, False);

      Show_All (Item);
      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-new-lecture-label"),
         Style_Get (Item.Browser, "tab-new-lecture-icon"),
         Item
      );
      Copy_Handles.Connect
      (  Item.Name_Edit,
         "changed",
         Name_Changed'Access,
         Item.all'Access
      );
      Copy_Handles.Connect
      (  Get_Entry (Item.Source_Name),
         "changed",
         Source_Name_Changed'Access,
         Item.all'Access
      );
      Copy_Handles.Connect
      (  Item.From_Edit,
         "changed",
         From_Changed'Access,
         Item.all'Access
      );
      Copy_Handles.Connect
      (  Item.To_Edit,
         "changed",
         To_Changed'Access,
         Item.all'Access
      );
      Source_Name_Changed (Item, Item.all'Access);
   end Initialize;

   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             )  is
      Name : constant UTF8_String := Get_Text (Item.Name_Edit);
   begin
      Check_Name (Name);
      Set_Hint
      (  Item.Name_Hint,
         Item.Browser,
         Checked,
         True
      );
   exception
      when others =>
         Set_Hint
         (  Item.Name_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Name_Changed;

   procedure Completed
             (  Item : not null access Lecture_Copy_Box_Record
             )  is
   begin
      declare
         Name : constant UTF8_String :=
                   Check_New_Name
                   (  Item,
                      Item.Store,
                      Item.Folder,
                      Get_Text (Item.Name_Edit)
                   );
      begin
         if Name'Length /= 0 then
            Rename
            (  Storage    => Item.Store,
               Object     => Item.Result,
               New_Name   => Name,
               New_Parent => Item.Folder
            );
            declare -- Update the cache
               Class : constant String := Get_Class (Item.Result);
            begin
               Created
               (   Store     => Get_Cache (Item.Browser),
                   Directory => Get_Path (Item.Folder_Name),
                   Item      =>
                   (   Name_Length => Name'Length,
                       Kind_Length => Class'Length,
                       Policy      => Cache_Expanded,
                       Directory   => False,
                       Name        => Item_Name (Name),
                       Kind        => Item_Type (Class)
               )   );
               Delete (Item);
               return;
            end;
         end if;
      exception
         when Reason : Ada.IO_Exceptions.Data_Error =>
            Error
            (  Item,
               (  Style_Get
                  (  Item.Browser,
                     "inconsistent-storage-error"
                  )
               &  Exception_Message (Reason)
            )  );
         when Ada.IO_Exceptions.Name_Error =>
            Error_Duplicated
            (  Item    => Item,
               Name    => Name,
               Storage => Item.Store,
               Parent  => Item.Folder
            );
         when Ada.IO_Exceptions.Use_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "unsupported-error")
            );
         when Reason : others =>
            Error (Item, Exception_Information (Reason));
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Completion fault:"
               &  Exception_Information (Reason)
               &  Where ("Completed")
            )  );
      end;
      Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
      Set_Editable (Item.Name_Edit,   True);
      Set_Editable (Item.Source_Name, True);
      Set_Editable (Item.Folder_Name, True);
      Set_Editable (Item.From_Edit,   True);
      Set_Editable (Item.To_Edit,     True);
   end Completed;

   procedure Source_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             )  is
      Lesson : constant Lecture_Handle := Get_Lecture (Item.Browser);
   begin
      if Is_Valid (Lesson) then
         declare
            Total : constant String :=
                       Strings_Edit.Integers.Image
                       (  Get_Examples_Number (Lesson)
                       );
         begin
            Set_Text (Item.From_Edit, "1");
            Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
            Set_Text (Item.To_Edit, Total);
            Set_Hint (Item.To_Hint, Item.Browser, Checked, True);
            Set_Text (Item.Total, Total);
         end;
      else
         Set_Text (Item.From_Edit, "1");
         Set_Hint (Item.From_Hint, Item.Browser, None, True);
         Set_Text (Item.To_Edit, "");
         Set_Hint (Item.To_Hint, Item.Browser, None, True);
         Set_Text (Item.Total, "?");
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Source_Name_Changed")
         )  );
   end Source_Name_Changed;

   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Copy_Box
             )  is
      Lesson : constant Lecture_Handle := Get_Lecture (Item.Browser);
      To     : Integer;
   begin
      if Is_Valid (Lesson) then
         To := Value (Get_Text (Item.To_Edit));
         if To in 1..Get_Examples_Number (Lesson) then
            Set_Hint (Item.To_Hint, Item.Browser, Checked, True);
         else
            Set_Hint (Item.To_Hint, Item.Browser, Erroneous, True);
         end if;
      else
         Set_Hint (Item.To_Hint, Item.Browser, Erroneous, False);
      end if;
   exception
      when others =>
         Set_Hint (Item.To_Hint, Item.Browser, Erroneous, True);
   end To_Changed;

   procedure Service (Item : not null access Lecture_Copy_Box_Record) is
   begin
      Copy
      (  Target => Item.Result,
         Source => Item.Source,
         From   => Item.From,
         To     => Item.To,
         Viewer => Get_Indicator (Item.Indicator)
      );
   end Service;

end Gtk.Fuzzy_Catalogue.Lecture_Copy;
