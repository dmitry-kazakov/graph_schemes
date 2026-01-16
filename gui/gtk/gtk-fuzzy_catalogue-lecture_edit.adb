--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.Lecture_Edit           Luebeck            --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Deposit_Handles;                use Deposit_Handles;
with Fuzzy.Lecture.Handle.Factory;   use Fuzzy.Lecture.Handle.Factory;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Alignment;                  use Gtk.Alignment;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Separator;                  use Gtk.Separator;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;    use Gtk.Widget.Styles.Icon_Size;
with Fuzzy.Lecture;                  use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Name_Tables;                    use Name_Tables;
with Parsers;                        use Parsers;

with Gtk.Fuzzy_Catalogue.Feature_Pane;
use  Gtk.Fuzzy_Catalogue.Feature_Pane;

with GLib.Object.Checked_Destroy;
with Strings_Edit.UTF8;

package body Gtk.Fuzzy_Catalogue.Lecture_Edit is
   use Gtk.Indicators.Progress;
   use Save_As_Buttons;
   use Save_Buttons;

   Modified : constant UTF8_String :=
              Strings_Edit.UTF8.Image (16#25CF#);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Lecture_Edit." & Name;
   end Where;

   procedure Aborted
             (  Item  : not null access Lecture_Edit_Box_Record;
                Fault : Exception_Occurrence
             )  is
   begin
      if Exception_Identity (Fault) = Data_Error'Identity then
         Error (Item, Exception_Message (Fault));
      elsif Exception_Identity (Fault) = Syntax_Error'Identity then
         Error (Item, Exception_Message (Fault));
      elsif Exception_Identity (Fault) = End_Error'Identity then
         if Item.Folder_Name /= null then
            Remove (Item.Save_Grid, Item.Folder_Name);
            Remove (Item.Save_Grid, Item.Folder_Hint);
            Item.Folder_Name := null;
         end if;
         Item.Remove (Item.Save_Grid);
         Item.Pack_Start (Item.Editor, True, True);
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

   procedure Add_Feature
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
      View    : constant Feature_Panel :=
                         Feature_Panel (Item.Browser.Features);
      Store   : Storage_Handle;
      Feature : Feature_Handle;
      This    : Picker_Constraint;
   begin
      Get_Current (View, Get_Selection (View.List), Store, Feature);
      if (  Feature.Is_Valid
         and then
            not Item.Editor.Has_Feature (Feature)
         and then
            Check (Item.Constraint, Store)
         )
      then
         Add_Feature (Item.Editor, Feature);
         This := Create (Get_Name (Feature));
         Combine (This, Item.Constraint);
         Set (This, Store);
         Add (Item.Features_List, Feature, This);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add_Feature")
         )  );
   end Add_Feature;

   function Cancel (Item : not null access Lecture_Edit_Box_Record)
      return Boolean is
   begin
      return
      (  not Get_Modified (Item.Editor)
      or else
         (  Gtk_Response_OK
         =  Query
            (  Item.Browser,
               Style_Get (Item.Browser, "modified-query-title"),
               Style_Get (Item.Browser, "query-icon"),
               Style_Get (Item.Browser, "query-icon-size"),
               Style_Get (Item.Browser, "modified-query")
      )  )  );
   end Cancel;

   procedure Close (Item : not null access Lecture_Edit_Box_Record) is
      View      : constant Gtk_Tree_View := Get_Tree_View (Item.Editor);
      Column    : Gtk_Tree_View_Column;
      Alignment : Gtk_Alignment;
      Title     : Gtk_Widget;
      Parent    : Gtk_Widget;
   begin
      if Item.Folder_Name /= null then
         Remove (Item.Save_Grid, Item.Folder_Name);
         Remove (Item.Save_Grid, Item.Folder_Hint);
         Item.Folder_Name := null;
      end if;
      Item.Remove (Item.Save_Grid);
      Remove_Buttons (Item);
      Clean (Item);
      Item.Pack_Start (Item.Editor, True, True);
         -- This is a hack to expand the titles of the columns
      for Index in 2..GInt'Last loop
         Column := Get_Column (View, Index);
         exit when Column = null;
         Title  := Get_Widget (Column);
         Parent := Get_Parent (Title);
         if Parent.all in Gtk_Alignment_Record'Class then
            Alignment :=
               Gtk_Alignment_Record'Class (Parent.all)'Unchecked_Access;
            Ref (Title);
            Remove (Alignment, Title);
            Add (Alignment, Title);
            Set (Alignment, 0.0, 0.0, 1.0, 1.0);
            Show_All (Title);
            Unref (Title);
         end if;
      end loop;
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

   procedure Columns_Selection_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
   begin
      Set_Sensitive
      (  Item.Delete_Feature,
         Is_Valid (Get_Selected_Feature (Item.Editor))
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Selection test: "
            &  Exception_Information (Error)
            &  Where ("Columns_Selection_Changed")
         )  );
   end Columns_Selection_Changed;

   procedure Completed
             (  Item : not null access Lecture_Edit_Box_Record
             )  is
      Name    : constant UTF8_String := To_String (Item.Name);
      Object  : Deposit_Handle := To_Deposit_Handle (Item.Result);
      Browser : constant Gtk_Fuzzy_Catalogue :=
                   Item.Browser.all'Unchecked_Access;
      Path    : constant Item_Path :=
                   Get_Folder
                   (  Item.Browser,
                      Item.Store,
                      Item.Folder
                   );
   begin
      begin
         if (  not Item.Overwrite
            and then
               Item.Store.Is_In (Name, Item.Folder)
            and then
               (  Gtk_Response_OK
               /= Query
                  (  Browser,
                     Style_Get (Browser, "replace-lecture-title"),
                     Style_Get (Browser, "query-icon"),
                     Style_Get (Browser, "query-icon-size"),
                     (  Style_Get (Browser, "replace-name-query")
                     &  Name
                     &  Style_Get (Browser, "folder-delete-from-query")
                     &  UTF8_String (Path)
            )  )  )  )
         then
            raise Ada.IO_Exceptions.Name_Error;
         end if;
         Item.Store.Unname (Name, Item.Folder);
         Item.Store.Rename (Object, Name, Item.Folder);
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
               Storage => Item.Store,
               Parent  => Item.Folder
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
      declare
         Class : constant String := Object.Get_Class;
      begin
         Created
         (   Store     => Item.Browser.Get_Cache,
             Directory => Path,
             Item      =>
             (   Name_Length => Name'Length,
                 Kind_Length => Class'Length,
                 Policy      => Cache_Expanded,
                 Directory   => False,
                 Name        => Item_Name (Name),
                 Kind        => Item_Type (Class)
         )  );
      end;
      Item.Result.Invalidate;
      Item.Editor.Set_Stored;
      Close (Item);
      if not Item.Exists then
         Item.Exists := True;
         Item.Editor.Get_Buttons_Box.Pack_Start
         (  Item.Save,
            False,
            False
         );
         Item.Save.Set_Sensitive (False);
      end if;
      Item.Set_Title (Name);
   end Completed;

   procedure Commit (Item : not null access Lecture_Edit_Box_Record) is
   begin
      Item.Folder_Name.Get_Folder (Item.Store, Item.Folder);
      if not Item.Store.Is_Valid then
         return;
      end if;
      declare
         Name : constant String :=
                         Check_New_Name
                         (  Item,
                            Item.Store,
                            Item.Folder,
                            Get_Text (Item.Name_Edit),
                            False
                         );
         Browser : constant Gtk_Fuzzy_Catalogue :=
                            Item.Browser.all'Unchecked_Access;
      begin
         if Name'Length = 0 then
            return;
         elsif not Item.Overwrite then
            if Item.Store.Is_In (Name, Item.Folder) then
               if (  Gtk_Response_OK
                  /= Query
                     (  Browser,
                        Style_Get (Browser, "replace-lecture-title"),
                        Style_Get (Browser, "query-icon"),
                        Style_Get (Browser, "query-icon-size"),
                        (  Style_Get (Browser, "replace-name-query")
                        &  Name
                        &  Style_Get
                           (  Browser,
                              "folder-delete-from-query"
                           )
                        &  UTF8_String
                           (  Get_Folder
                              (  Item.Browser,
                                 Item.Store,
                                 Item.Folder
                  )  )  )  )  )
               then
                  Error_Duplicated
                  (  Item    => Item,
                     Name    => Name,
                     Storage => Item.Store,
                     Parent  => Item.Folder
                  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               end if;
               Item.Overwrite := True;
            end if;
         end if;
         Item.Name := To_Unbounded_String (Name);
         Item.Result.Invalidate;
         Item.Result := Create_Persistent (Item.Store);
      exception
         when Reason : Ada.IO_Exceptions.Data_Error =>
            Error
            (  Item,
               (  Style_Get (Item.Browser, "creation-error")
               &  Exception_Message (Reason)
            )  );
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
      Start_Servicing (Item);
   exception
      when Reason : others =>
         Error (Item, Exception_Message (Reason));
   end Commit;

   procedure Delete_Feature
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
      Feature : Feature_Handle;
      This    : Picker_Constraint;
   begin
      Feature := Get_Selected_Feature (Item.Editor);
      begin
         This := Item.Features_List.Get (Feature);
      exception
         when Constraint_Error =>
            null;
      end;
      Free (This);
      Delete_Feature (Item.Editor, Feature);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Delete_Feature")
         )  );
   end Delete_Feature;

   procedure Features_Selection_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
   begin
      declare
         View     : constant Feature_Panel :=
                             Feature_Panel (Item.Browser.Features);
         Selected : constant Selection := Get_Selection (View.List);
         Store    : Storage_Handle;
         Feature  : Feature_Handle;
      begin
         Get_Current (View, Selected, Store, Feature);
         Item.Add_Feature.Set_Sensitive
         (  Feature.Is_Valid
         and then
            not Item.Editor.Has_Feature (Feature)
         and then
            Check (Item.Constraint, Store)
         );
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Selection test: "
            &  Exception_Information (Error)
            &  Where ("Features_Selection_Changed")
         )  );
   end Features_Selection_Changed;

   procedure Finalize
             (  Item : not null access Lecture_Edit_Box_Record
             )  is
      This : Picker_Constraint;
   begin
      for Index in 1..Get_Size (Item.Features_List) loop
         This := Item.Features_List.Get (Index);
         Free (This);
      end loop;
      Erase (Item.Features_List);
      Unref (Item.Progress);
      Unref (Item.Save);
      Unref (Item.Save_As);
      Unref (Item.Editor);
      Unref (Item.Save_Grid);
      Finalize (Gtk_Item_Box_Record (Item.all)'Unchecked_Access);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Lecture_Edit_Box_Record)")
         )  );
   end Finalize;

   procedure Gtk_New
             (  Item    : out Lecture_Edit_Box;
                Name    : String;
                Store   : Storage_Handle;
                Lesson  : Lecture_Handle;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_Edit_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Name, Store, Lesson);
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

   procedure Gtk_New
             (  Item    : out Lecture_Edit_Box;
                Name    : String;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
      Store  : Storage_Handle;
      Lesson : Lecture_Handle;
   begin
      Item :=
         new Lecture_Edit_Box_Record (Browser.all'Unchecked_Access);
      begin
         Initialize (Item, Name, Store, Lesson);
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
             (  Item   : not null access Lecture_Edit_Box_Record'Class;
                Name   : String;
                Store  : Storage_Handle;
                Lesson : Lecture_Handle
             )  is
      function Title return UTF8_String is
      begin
         return UTF8_String (Get_Name (Item_Path (Name)));
      exception
         when others =>
            return "";
      end;
      Wait      : Wait_Cursor (Item.Browser);
      Separator : Gtk_Separator;
      Label     : Gtk_Label;
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "set edit box", False);
      Set (Item.Constraint, Store);
         -- Editor
      Gtk_New (Item.Editor);
      Item.Pack_Start (Item.Editor);
      Ref (Item.Editor);

      if Item.Browser.Features /= null then
         Gtk_New_VSeparator (Separator);
         Item.Editor.Get_Buttons_Box.Pack_Start
         (  Separator,
            False,
            False
         );

         Gtk_New (Item.Add_Feature);
         Item.Add_Feature.Set_Sensitive (False);
         Item.Editor.Get_Buttons_Box.Pack_Start
         (  Item.Add_Feature,
            False,
            False
         );
         Gtk_New (Item.Delete_Feature);
         Item.Delete_Feature.Set_Sensitive (False);
         Item.Editor.Get_Buttons_Box.Pack_Start
         (  Item.Delete_Feature,
            False,
            False
         );
         Set
         (  Item.Selector_1,
            Edit_Handlers.Connect
            (  Item.Browser.Features.List,
               "selection-changed",
               Edit_Handlers.To_Marshaller
               (  Features_Selection_Changed'Access
               ),
               Item.all'Access
         )  );
         Edit_Handlers.Connect
         (  Item.Add_Feature,
            "clicked",
            Add_Feature'Access,
            Item.all'Unchecked_Access
         );
         Edit_Handlers.Connect
         (  Item.Delete_Feature,
            "clicked",
            Delete_Feature'Access,
            Item.all'Unchecked_Access
         );
      end if;

      Gtk_New_VSeparator (Separator);
      Item.Editor.Get_Buttons_Box.Pack_Start
      (  Separator,
         False,
         False
      );

      Gtk_New (Item.Save);
      Item.Save.Set_Sensitive (False);

      if Lesson.Is_Valid then
         Ref (Item.Save);
         Item.Exists := True;
         Item.Editor.Get_Buttons_Box.Pack_Start
         (  Item.Save,
            False,
            False
         );
      else
         Item.Save.Ref_Sink;
      end if;

      Gtk_New (Item.Save_As);
      Item.Editor.Get_Buttons_Box.Pack_Start
      (  Item.Save_As,
         False,
         False
      );
      Ref (Item.Save_As);

      Gtk_New (Item.Stop);
      Gtk_New
      (  Widget          => Item.Progress,
         Button          => Item.Stop,
         Button_Position => (1, 2, 0, 1),
         Size            => (2, 1),
         Spacing =>
            (  Width  => Style_Get (Item.Browser, "row-spacing"),
               Height => Style_Get (Item.Browser, "column-spacing")
      )     );
      Item.Editor.Get_Buttons_Box.Pack_Start (Item.Progress);
      Ref (Item.Progress);

      Edit_Handlers.Connect
      (  Item.Editor,
         "modified-changed",
         Modified_Changed'Access,
         Item.all'Unchecked_Access
      );
      Edit_Handlers.Connect
      (  Item.Editor,
         "features-selection-changed",
         Columns_Selection_Changed'Access,
         Item.all'Unchecked_Access
      );
      Edit_Handlers.Connect
      (  Item.Save,
         "clicked",
         Save'Access,
         Item.all'Unchecked_Access
      );
      Edit_Handlers.Connect
      (  Item.Save_As,
         "clicked",
         Save_As'Access,
         Item.all'Unchecked_Access
      );

      Item.Show_All;
      Item.Browser.Add_Item
      (  Title,
         Style_Get (Item.Browser, "tab-edit-lecture-icon"),
         Item
      );
      begin
         Put
         (  Widget => Item.Editor,
            Lesson => Lesson,
            Viewer => Get_Indicator (Item.Progress)
         );
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
      end;
      Item.Editor.Get_Buttons_Box.Remove (Item.Progress);

         -- Save grid
      Gtk_New (Item.Save_Grid, 2, 3, False);
      Item.Save_Grid.Ref_Sink;
      Item.Save_Grid.Set_Col_Spacings (Column_Spacing);
      Item.Save_Grid.Set_Row_Spacings (Row_Spacing);

      Gtk_New (Label, Style_Get (Item.Browser, "lecture-name-label"));
      Item.Save_Grid.Attach
      (  Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Item.Name_Edit);
      Item.Name_Edit.Set_Text (Title);
      Item.Save_Grid.Attach
      (  Item.Name_Edit,
         1, 2,
         0, 1,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Name_Hint);
      Item.Save_Grid.Attach
      (  Item.Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Name_Hint, Item.Browser, None, True);

      Gtk_New (Label, Style_Get (Item.Browser, "folder-path-label"));
      Item.Save_Grid.Attach
      (  Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End);
      Label.Set_Valign (Align_Center);
--    Label.Set_Alignment (1.0, 0.5);
      Edit_Handlers.Connect
      (  Item.Name_Edit,
         "changed",
         Name_Changed'Access,
         Item.all'Access
      );
      Set
      (  Item.Selector_2,
         Edit_Handlers.Connect
         (  Item.Browser.List_Tabs,
            "switch_page",
            Edit_Handlers.To_Marshaller
            (  Features_Selection_Changed'Access
            ),
            Item.all'Access
      )  );
   end Initialize;

   procedure Modified_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
   begin
      if Get_Modified (Item.Editor) then
         Item.Save.Set_Sensitive (True);
         Item.Set_Icon
         (  Style_Get (Item.Browser, "tab-edited-lecture-icon")
         );
      else
         Item.Save.Set_Sensitive (False);
         Item.Set_Icon
         (  Style_Get (Item.Browser, "tab-edit-lecture-icon")
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Modified_Changed")
         )  );
   end Modified_Changed;

   procedure Name_Changed
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
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

   procedure Save
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
   begin
      Item.Overwrite := True;
      Item.Remove (Item.Editor);
      Add_Buttons (Item);
      Gtk_New (Item.Folder_Name, "set folder", Item.Folder_Hint, Item);
      Combine (Item.Constraint, Get_Constraint (Item.Folder_Name));
      Item.Save_Grid.Attach
      (  Item.Folder_Name,
         1, 2,
         1, 2,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Item.Save_Grid.Attach
      (  Item.Folder_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Item.Pack_Start (Item.Save_Grid, False, False);
      Item.Save_Grid.Show_All;
      Item.Add_Close;
      Commit (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save")
         )  );
   end Save;

   procedure Save_As
             (  Object : access GObject_Record'Class;
                Item   : Lecture_Edit_Box
             )  is
   begin
      Item.Overwrite := False;
      Item.Remove (Item.Editor);
      Add_Buttons (Item);
      Gtk_New (Item.Folder_Name, "set folder", Item.Folder_Hint, Item);
      Combine (Item.Constraint, Get_Constraint (Item.Folder_Name));
      Item.Save_Grid.Attach
      (  Item.Folder_Name,
         1, 2,
         1, 2,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Item.Save_Grid.Attach
      (  Item.Folder_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Item.Pack_Start (Item.Save_Grid, False, False);
      Item.Save_Grid.Show_All;
      Item.Add_Close;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Save_As")
         )  );
   end Save_As;

   procedure Service (Item : not null access Lecture_Edit_Box_Record) is
   begin
      Item.Editor.Get_Examples
      (  Item.Result,
         Item.Indicator.Get_Indicator
      );
   end Service;

end Gtk.Fuzzy_Catalogue.Lecture_Edit;
