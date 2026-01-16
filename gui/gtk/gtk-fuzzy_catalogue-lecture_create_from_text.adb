--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Lecture_Create_From_Text                 Winter, 2009       --
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

with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Ada.Strings.Maps;               use Ada.Strings.Maps;
with Glib.Messages;                  use Glib.Messages;
with Gtk.Alignment;                  use Gtk.Alignment;
with Gtk.Frame;                      use Gtk.Frame;
with Gtk.Fuzzy_Catalogue.File_Views; use Gtk.Fuzzy_Catalogue.File_Views;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Paned;                      use Gtk.Paned;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Fuzzy.Lecture;                  use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Bulk;      use Fuzzy.Lecture.Handle.Bulk;
with Fuzzy.Lecture.Handle.Factory;   use Fuzzy.Lecture.Handle.Factory;
with Fuzzy.Lecture.Handle.Text_IO;   use Fuzzy.Lecture.Handle.Text_IO;
with Indicator.Handle;               use Indicator.Handle;
with Name_Tables;                    use Name_Tables;
with Strings_Edit;                   use Strings_Edit;
with Strings_Edit.Integers;          use Strings_Edit.Integers;
with Strings_Edit.UTF8.Handling;     use Strings_Edit.UTF8.Handling;

with Ada.Directories;
with Fuzzy.Feature.Handle.Bounded_Arrays;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text is
   use Ada.Exceptions;
   use File_Tokens;
   use Units;

   Read_From_Text_Key : constant String :=
      "read training set from text";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text." & Name;
   end Where;

   procedure Aborted
             (  Item  : not null access
                        Lecture_Create_From_Text_Box_Record;
                Fault : Exception_Occurrence
             )  is
      Button : Show_Location_Buttons.Gtk_Style_Button;
      Box    : Gtk_VBox;
   begin
      Invalidate (Item.Lesson);
      Gtk.Fuzzy_Catalogue.Aborted
      (  Gtk_Item_Box_Record (Item.all)'Unchecked_Access,
         Fault
      );
      if Exception_Identity (Fault) =  Data_Error'Identity then
         declare
            Text       : constant String  := Exception_Message (Fault);
            Has_Brace  : Boolean := False;
            Has_Column : Boolean := False;
            Has_Line   : Boolean := False;
            Pointer    : Integer;
         begin
            for Index in reverse Text'First..Text'Last loop
               if Text (Index) = ']' then
                  Has_Brace := True;
               elsif Has_Brace and then Text (Index) = ':' then
                  Pointer := Index + 1;
                  if Has_Column then
                     Get (Text, Pointer);
                     Get (Text, Pointer, Item.Line_No);
                     Has_Line := True;
                     exit;
                  else
                     Get (Text, Pointer);
                     Get (Text, Pointer, Item.Position_No);
                     Has_Column := True;
                  end if;
               end if;
            end loop;
            if not Has_Line then
               return;
            end if;
         exception
            when others =>
               return;
         end;
         Gtk_New_VBox (Box);
         Pack_Start (Item.Error_Box, Box, False, False);
         Gtk_New (Button);
         Pack_Start (Box, Button, False, False);
         Show_All (Box);
         Create_Handles.Connect
         (  Button,
            "clicked",
            Show_Location'Access,
            Item.all'Unchecked_Access
         );
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

   procedure Commit
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             )  is
      procedure Set_Hint
                (  Token : Keyword;
                   Hint  : Hint_Type;
                   Show  : Boolean
                )  is
         Box : Gtk_Box;
      begin
         case Token is
            when Comment =>
               Box := Item.Comment_Hint;
            when Positive_Example =>
               Box := Item.Positive_Hint;
            when Negative_Example =>
               Box := Item.Negative_Hint;
            when Full_Example =>
               Box := Item.Full_Hint;
            when Undefined_Example =>
               Box := Item.Undefined_Hint;
         end case;
         Set_Hint (Box, Item.Browser, Hint, Show);
      end Set_Hint;

      function Add (Text : String; Token : Keyword) return Boolean is
      begin
         if Text'Length > 0 then
            Add (Item.Keywords, Text, Token);
         end if;
         Set_Hint (Token, Checked, True);
         return False;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Set_Hint (Token, Erroneous, True);
            Set_Hint (Find (Item.Keywords, Text), Erroneous, True);
            Error
            (  Item,
               Style_Get
               (  Item.Browser,
                  "duplicated-example-keyword-error"
            )  );
            return True;
      end Add;

      Directory : Unbounded_String;
   begin
      if Is_Valid (Item.Lesson) then
         -- Storing the set
         Clean (Item);
         declare
            Store  : Storage_Handle;
            Folder : Deposit_Handle;
         begin
            Get_Folder (Item.Folder_Name, Store, Folder);
            if not Is_Valid (Store) then
               return;
            end if;
            declare
               Class   : constant String := Get_Class (Item.Lesson);
               Lecture : Deposit_Handle :=
                            To_Deposit_Handle (Item.Lesson);
               Name    : constant String :=
                                  Check_New_Name
                                  (  Item,
                                     Store,
                                     Folder,
                                     Get_Text (Item.Name_Edit)
                                  );
            begin
               if Name'Length = 0 then
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               end if;
               Rename
               (  Storage    => Store,
                  Object     => Lecture,
                  New_Name   => Name,
                  New_Parent => Folder
               );
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
            exception
               when Reason : Data_Error =>
                  Error
                  (  Item,
                     (  Style_Get (Item.Browser, "creation-error")
                     &  Exception_Message (Reason)
                  )  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True)
                  ;
                  return;
               when Name_Error =>
                  Error_Duplicated
                  (  Item    => Item,
                     Name    => Name,
                     Storage => Store,
                     Parent  => Folder
                  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               when Use_Error =>
                  Error
                  (  Item,
                     Style_Get (Item.Browser, "unsupported-error")
                  );
                  Set_Hint
                  (  Item.Name_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
            end;
         end;
      else  -- Reading the set
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
            -- Features list
         declare
            use Fuzzy.Feature.Handle.Bounded_Arrays;
            Features : Bounded_Array renames
                          Get_Selected (Item.Sequence);
         begin
            if Features.Last - Features.First < 1 then
               Error
               (  Item,
                  Style_Get (Item.Browser, "no-features-error")
               );
               return;
            end if;
            Erase (Item.Features);
            for Index in Features.First..Features.Last loop
               Put (Item.Features, Index, Get (Features, Index));
            end loop;
         end;
            -- Keyword tables
         Erase (Item.Keywords);
         if (  Add (Get_Text (Item.Comment_Edit), Comment)
            or else
               Add (Get_Text (Item.Positive_Edit), Positive_Example)
            or else
               Add (Get_Text (Item.Negative_Edit), Negative_Example)
            or else
               Add (Get_Text (Item.Full_Edit), Full_Example)
            or else
               Add (Get_Text (Item.Undefined_Edit), Undefined_Example)
            )
         then
            return;
         end if;
            -- Delimiter
         begin
            declare
               List : constant UTF8_String :=
                               Get (Item.Delimiter_Frame);
            begin
              Item.Delimiter := To_Unbounded_String (To_String (List));
            exception
               when Constraint_Error | Data_Error =>
                  Error
                  (  Item,
                     Style_Get
                     (  Item.Browser,
                        "invalid-delimiters-error"
                  )  );
                  return;
            end;
         exception
            when Data_Error =>
               Error
               (  Item,
                  Style_Get (Item.Browser, "no-delimiter-error")
               );
               return;
         end;
            -- Encoding and other stuff
         Item.Encoding    := Get (Item.Encoding_Box);
         Item.Allow_Empty := Get_Active (Item.Allow_Empty_Button);
         Item.Quote_Units := Get_Active (Item.Quote_Units_Button);
         Item.Get_Units   := Get_Active (Item.Get_Units_Button);
         if Get_Active (Item.Necessity_Button) then
            Item.Default := Certain_True;
         else
            Item.Default := Uncertain;
         end if;
         if Get_Active (Item.Positive_By_Default) then
            Item.Default_Example := Positive_Example;
         elsif Get_Active (Item.Negative_By_Default) then
            Item.Default_Example := Negative_Example;
         else
            Item.Default_Example := Full_Example;
         end if;
            -- Training set creation
         declare
            Store  : Storage_Handle;
            Folder : Deposit_Handle;
         begin
            Browse
            (  Store   => Get_Cache (Item.Browser),
               Path    => Get_Path (Item.Folder_Name),
               Storage => Store,
               Object  => Folder,
               Partial => False
            );
            if not Is_Valid (Store) then
               Error
               (  Item,
                  Style_Get (Item.Browser, "storage-browse-error")
               );
               Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
               return;
            end if;
            Item.Lesson := Create_Persistent (Store);
         exception
            when Reason : Data_Error =>
               Error
               (  Item,
                  (  Style_Get (Item.Browser, "creation-error")
                  &  Exception_Message (Reason)
               )  );
               Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
               return;
            when Use_Error =>
               Error
               (  Item,
                  Style_Get (Item.Browser, "unsupported-error")
               );
               Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
               return;
         end;

         Clean (Item);
         begin
            Store
            (  Widget => Item.Browser,
               Key    => Read_From_Text_Key,
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
         Error (Item, Exception_Message (Reason));
   end Commit;

   procedure Completed
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             )  is
   begin
      if Item.Pane /= null then
         Remove (Item, Item.Pane);
         Item.Pane := null;
      end if;
      Remove (Item.Grid, Item.File_Name_Label);
      Remove (Item.Grid, Item.File_Name_Edit);
      Remove (Item.Grid, Item.File_Name_Hint);
      Commit (Item);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Completed")
         )  );
   end Completed;

   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
             )  is
      File : constant UTF8_String := Get_Selection (Item.Directory);
   begin
      if File'Length > 0 then
         Set_Text (Item.File_Name_Edit, File);
         Set_Hint (Item.File_Name_Hint, Item.Browser, Checked, True);
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

   procedure Gtk_New
             (  Item    : out Lecture_Create_From_Text_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_Create_From_Text_Box_Record
             (  Browser.all'Unchecked_Access
             );
      begin
         Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text.Initialize (Item);
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
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record'Class
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
      Parameters_Box : Gtk_Box;
      Data_Grid      : Gtk_Table;

      Align : Gtk_Alignment;
      Frame : Gtk_Frame;
      Label : Gtk_Label;
      Hint  : Gtk_Box;
      Box   : Gtk_Box;
   begin
      Initialize (Item, "read box", True);
      Gtk_New (Item.Grid, 3, 3, False);
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
         1, 2,
         0, 1,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
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
         1, 2,
         1, 2,
         Xoptions => Expand or Fill,
         Yoptions => Shrink
      );
      Attach
      (  Item.Grid,
         Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
         -- Row 3
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
      Item.File_Name_Label.Set_Halign (Align_End);
      Item.File_Name_Label.Set_Valign (Align_Center);
--    Set_Alignment (Item.File_Name_Label, 1.0, 0.5);
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

      Pack_Start (Item, Item.Grid, False, False);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New_HBox (Parameters_Box);
      Set_Border_Width (Parameters_Box, Row_Spacing);
      Set_Spacing (Parameters_Box, GInt (Column_Spacing));

         Gtk_New (Data_Grid, 6, 4, False);
         Set_Col_Spacings (Data_Grid, Column_Spacing);
         Set_Row_Spacings (Data_Grid, Row_Spacing);

         Gtk_New (Label, Style_Get (Item.Browser, "by-default-label"));
         Set_Angle (Label, 90.0);
         Gtk_New (Align, 0.5, 1.0, 0.2, 0.2);
         Add (Align, Label);
         Attach
         (  Data_Grid,
            Align,
            3, 4,
            0, 3,
            Xoptions => Shrink,
            Yoptions => Fill or Expand
         );
            -- Row 1
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "encoding-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            0, 1,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Encoding_Box, UTF8_Set);
         Attach
         (  Data_Grid,
            Item.Encoding_Box,
            1, 2,
            0, 1,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
            -- Row 2
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "comment-examples-prefix-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            1, 2,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Comment_Edit);
         Set_Text (Item.Comment_Edit, "--");
         Attach
         (  Data_Grid,
            Item.Comment_Edit,
            1, 2,
            1, 2,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Comment_Hint);
         Attach
         (  Data_Grid,
            Item.Comment_Hint,
            2, 3,
            1, 2,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
         Set_Hint (Item.Comment_Hint, Item.Browser, None, True);
            -- Row 3
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "undefined-example-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            2, 3,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Undefined_Edit);
         Set_Text (Item.Undefined_Edit, "?");
         Attach
         (  Data_Grid,
            Item.Undefined_Edit,
            1, 2,
            2, 3,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Undefined_Hint);
         Attach
         (  Data_Grid,
            Item.Undefined_Hint,
            2, 3,
            2, 3,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
         Set_Hint (Item.Undefined_Hint, Item.Browser, None, True);
            -- Row 4
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "positive-examples-prefix-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            3, 4,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Positive_Edit);
         Set_Text (Item.Positive_Edit, "<+>");
         Attach
         (  Data_Grid,
            Item.Positive_Edit,
            1, 2,
            3, 4,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Positive_Hint);
         Attach
         (  Data_Grid,
            Item.Positive_Hint,
            2, 3,
            3, 4,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
         Set_Hint (Item.Positive_Hint, Item.Browser, None, True);

         Gtk_New (Item.Positive_By_Default, Item.Positive_By_Default);
         Attach
         (  Data_Grid,
            Item.Positive_By_Default,
            3, 4,
            3, 4,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 5
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "negative-examples-prefix-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            4, 5,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Negative_Edit);
         Set_Text (Item.Negative_Edit, "<->");
         Attach
         (  Data_Grid,
            Item.Negative_Edit,
            1, 2,
            4, 5,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Negative_Hint);
         Attach
         (  Data_Grid,
            Item.Negative_Hint,
            2, 3,
            4, 5,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
         Set_Hint (Item.Negative_Hint, Item.Browser, None, True);

         Gtk_New (Item.Negative_By_Default, Item.Positive_By_Default);
         Attach
         (  Data_Grid,
            Item.Negative_By_Default,
            3, 4,
            4, 5,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 6
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "full-examples-prefix-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            5, 6,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Full_Edit);
         Set_Text (Item.Full_Edit, "<*>");
         Attach
         (  Data_Grid,
            Item.Full_Edit,
            1, 2,
            5, 6,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Full_Hint);
         Attach
         (  Data_Grid,
            Item.Full_Hint,
            2, 3,
            5, 6,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
         Set_Hint (Item.Full_Hint, Item.Browser, None, True);

         Gtk_New (Item.Full_By_Default, Item.Positive_By_Default);
         Set_Active (Item.Positive_By_Default, True);
         Attach
         (  Data_Grid,
            Item.Full_By_Default,
            3, 4,
            5, 6,
            Xoptions => Shrink,
            Yoptions => Shrink
         );

         Gtk_New_VBox (Box);
         Pack_Start (Box, Data_Grid, False, False);
         Pack_Start (Parameters_Box, Box, False, False);
            -- Delimiters
         Gtk_New
         (  Item.Delimiter_Frame,
            False,
            "delimiters-list-frame-label",
            Item.Browser
         );
         Gtk_New_VBox (Box);
         Pack_Start (Parameters_Box, Box, False, False);
         Pack_Start (Box, Item.Delimiter_Frame, False, False);
            -- Check boxes
         Gtk_New (Data_Grid, 4, 2, False);
         Set_Col_Spacings (Data_Grid, Column_Spacing);
         Set_Row_Spacings (Data_Grid, Row_Spacing);
         Pack_Start (Box, Data_Grid, False, False);
            -- Row 1
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "allow-empty-fields-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            0, 1,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Allow_Empty_Button);
         Attach
         (  Data_Grid,
            Item.Allow_Empty_Button,
            1, 2,
            0, 1,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 2
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "require-units-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            1, 2,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Get_Units_Button);
         Attach
         (  Data_Grid,
            Item.Get_Units_Button,
            1, 2,
            1, 2,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 3
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "quote-units-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            2, 3,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Quote_Units_Button);
         Attach
         (  Data_Grid,
            Item.Quote_Units_Button,
            1, 2,
            2, 3,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 3
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "default-necessity-label")
         );
         Attach
         (  Data_Grid,
            Label,
            0, 1,
            3, 4,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Necessity_Button);
         Set_Active (Item.Necessity_Button, True);
         Attach
         (  Data_Grid,
            Item.Necessity_Button,
            1, 2,
            3, 4,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Features sequence
         Gtk_New (Item.Sequence, Item.Browser);
         Combine (Item.Constraint, Get_Constraint (Item.Sequence));
         Set_Spacing (Item.Sequence, GInt (Column_Spacing));
         Pack_Start (Parameters_Box, Item.Sequence, True, True);
         Add (Frame, Parameters_Box);

      Gtk_New_Vpaned (Item.Pane);
      Add1 (Item.Pane, Frame);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New
      (  Widget  => Item.Directory,
         Browser => Item.Browser,
         Key     => Read_From_Text_Key
      );
      Add (Frame, Item.Directory);
      Add2 (Item.Pane, Frame);
      Pack_Start (Item, Item.Pane);

      Show_All (Item);

      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-new-lecture-label"),
         Style_Get (Item.Browser, "tab-new-lecture-icon"),
         Item
      );
      Create_Handles.Connect
      (  Item.Name_Edit,
         "changed",
         Name_Changed'Access,
         Item.all'Access
      );
      Reader_Handlers.Connect
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

   procedure Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
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

   procedure Show_Location
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Create_From_Text_Box
             )  is
      View : File_View;
   begin
      Gtk_New
      (  Item        => View,
         File_Name   => To_String (Item.File_Name),
         New_File    => False,
         Language    => null,
         Mode        => Item.Encoding,
         Line_No     => Item.Line_No,
         Position_No => Item.Position_No,
         Browser     => Item.Browser
      );
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Show_Location")
         )  );
   end Show_Location;

   procedure Service
             (  Item : not null access
                       Lecture_Create_From_Text_Box_Record
             )  is
      Features : Feature_Array renames To_Feature_Array (Item.Features);
      Delimiters : constant Character_Set :=
                            To_Set (To_String (Item.Delimiter));
      Parameters : constant Read_Parameters :=
      (  Blanks             => Read_Defaults.Blanks,
         Delimiters         => Delimiters,
         Default_Example    => Item.Default_Example,
         Allow_Empty_Fields => Item.Allow_Empty,
         Keywords           => Item.Keywords,
         Base               => 10,
         Default            => Item.Default,
         Mode               => Item.Encoding,
         Get_Units          => Item.Get_Units,
         Quote_Units        => Item.Quote_Units
      );
      Transaction : Lecture_Update (Ptr (Item.Lesson));
   begin
      Read
      (  File_Name  => To_String (Item.File_Name),
         Lesson     => Item.Lesson,
         Features   => Features,
         Parameters => Parameters,
         Viewer     => Ptr (Get_Indicator (Item.Indicator))
      );
   end Service;

end Gtk.Fuzzy_Catalogue.Lecture_Create_From_Text;
