--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Lecture_Save_As_Text                     Winter, 2008       --
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

with Ada.Directories;               use Ada.Directories;
with Ada.IO_Exceptions;             use Ada.IO_Exceptions;
with GLib.Messages;                 use GLib.Messages;
with GLib.Properties;               use GLib.Properties;
with Gtk.Frame;                     use Gtk.Frame;
with Gtk.Missed;                    use Gtk.Missed;
with Gtk.Paned;                     use Gtk.Paned;
with Gtk.Table;                     use Gtk.Table;
with Gtk.Tree_Model;                use Gtk.Tree_Model;
with Gtk.Tree_View;                 use Gtk.Tree_View;
with Gtk.Tree_View_Column;          use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;             use Gtk.Widget.Styles;
with Gtk.Widget.Styles.Icon_Size;   use Gtk.Widget.Styles.Icon_Size;
with Fuzzy.Lecture;                 use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Text_IO;  use Fuzzy.Lecture.Handle.Text_IO;
with Fuzzy.Lecture.Text_IO;         use Fuzzy.Lecture.Text_IO;
with Indicator.Handle;              use Indicator.Handle;
with Strings_Edit;                  use Strings_Edit;
with Strings_Edit.Integers;         use Strings_Edit.Integers;

with Fuzzy.Feature.Handle.Bounded_Arrays;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text is
   use Ada.Exceptions;

   Save_As_Text_Key : constant String := "save training set as text";

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text." & Name;
   end Where;

   procedure Commit
             (  Item : not null access Lecture_Save_As_Text_Box_Record
             )  is
      Directory : Unbounded_String;
   begin
         -- Training set
      Item.Lesson :=
         Get_Lecture
         (  Get_Path (Item.Lecture_Name),
            Item,
            Item.Lecture_Name_Hint
         );
      if not Is_Valid (Item.Lesson) then
         return;
      end if;
         -- File name
      Item.File_Name :=
         To_Unbounded_String (Trim (Get_Text (Item.File_Name_Edit)));
      if Length (Item.File_Name) = 0 then
         Set_Hint (Item.File_Name_Hint, Item.Browser, Erroneous, True);
         Error
         (  Item,
            Style_Get (Item.Browser, "empty-file-name-error")
         );
         return;
      end if;
      if Is_Absolute (To_String (Item.File_Name)) then
         Append (Directory, Get_Dirname (To_String (Item.File_Name)));
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
         if Exists (To_String (Item.File_Name)) then
            null;
         end if;
         if not Exists (To_String (Directory)) then
            Set_Hint
            (  Item.File_Name_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            Error
            (  Item,
               Style_Get (Item.Browser, "no-directory-error")
            );
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
         -- From
      begin
         Item.From := Value (Get_Text (Item.From_Edit));
         if Item.From in 1..Get_Examples_Number (Item.Lesson) then
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
         -- To
      begin
         Item.To := Value (Get_Text (Item.To_Edit));
         if Item.To in 1..Get_Examples_Number (Item.Lesson) then
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
         -- Features list
      declare
         use Fuzzy.Feature.Handle.Bounded_Arrays;
         Features : Bounded_Array renames Get_Selected (Item.List);
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
            Put
            (  Item.Features,
               Index,
               Get (Features, Index)
            );
         end loop;
      end;
         -- Examples to write
      Item.Write_Positive := Get_Active (Item.Positive_Use);
      Item.Write_Negative := Get_Active (Item.Negative_Use);
      if not (Item.Write_Positive or Item.Write_Negative) then
         Error (Item, Style_Get (Item.Browser, "no-examples-error"));
         return;
      end if;
      Item.Positive_Example :=
         To_Unbounded_String (Get_Text (Item.Positive_Edit));
      Item.Negative_Example :=
         To_Unbounded_String (Get_Text (Item.Negative_Edit));
      if (  Item.Write_Positive
         and then
            Item.Write_Negative
         and then
            Item.Positive_Example = Item.Negative_Example
         )
      then
         Error
         (  Item,
            Style_Get (Item.Browser, "same-examples-prefix-error")
         );
         return;
      end if;
         -- Delimiter
      begin
         Item.Delimiter :=
            To_Unbounded_String (Get (Item.Delimiter_Frame));
      exception
         when Data_Error =>
            Error
            (  Item,
               Style_Get (Item.Browser, "no-delimiter-error")
            );
            return;
      end;

      Clean (Item);
      begin
         Store
         (  Widget => Item.Browser,
            Key    => Save_As_Text_Key,
            Value  => Get_Directory (Item.Directory)
         );
      exception
         when Ada.IO_Exceptions.Name_Error =>
            null;
      end;

      if Exists (To_String (Item.File_Name)) then
         declare
            Title : constant String :=
               Style_Get (Item.Browser, "overwrite-query-title");
            Icon : constant String :=
               Style_Get (Item.Browser, "query-icon");
            Size : constant Gtk_Icon_Size_Enum :=
               Style_Get (Item.Browser, "query-icon-size");
            Message : Unbounded_String;
            Result  : Gtk_Response_Type;
         begin
            Append
            (  Message,
               Style_Get (Item.Browser, "overwrite-query")
            );
            Append (Message, Base_Name (To_String (Item.File_Name)));
            if Length (Directory) = 0 then
               Result :=
                  Query
                  (  Item.Browser,
                     Title,
                     Icon,
                     Size,
                     To_String (Message)
                  );
            else
               Result :=
                  Query
                  (  Item.Browser,
                     Title,
                     Icon,
                     Size,
                     (  To_String (Message)
                     &  Style_Get (Item.Browser, "overwrite-from-query")
                     &  To_String (Directory)
                  )  );
            end if;
            if Result /= Gtk_Response_OK then
               return;
            end if;
         end;
      end if;
      Start_Servicing (Item);
   exception
      when Error : others =>
         Log
         (  GtkAda_Contributions_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   procedure File_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
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
            (  "Fault: " & Exception_Information (Error)
            &  Where ("File_Changed")
         )  );
   end File_Changed;

   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
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
             (  Item    : out Lecture_Save_As_Text_Box;
                Browser : access Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Item :=
         new Lecture_Save_As_Text_Box_Record
             (  Browser.all'Unchecked_Access
             );
      begin
         Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text.Initialize (Item);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item : access Lecture_Save_As_Text_Box_Record'Class
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
      Parameters_Box : Gtk_Box;
      Range_Grid     : Gtk_Table;
      Names_Grid     : Gtk_Table;
      Label          : Gtk_Label;
      Frame          : Gtk_Frame;
      Pane           : Gtk_Paned;
   begin
      Initialize (Item, "save set box", True);

      Gtk_New (Names_Grid, 2, 3, False);
      Set_Col_Spacings (Names_Grid, Column_Spacing);
      Set_Row_Spacings (Names_Grid, Row_Spacing);
      Pack_Start (Item, Names_Grid, False, False);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "lecture-name-label"));
      Attach
      (  Names_Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New
      (  Item.Lecture_Name,
         "set name",
         Item.Lecture_Name_Hint,
         Item.Browser.Lectures,
         True
      );
      Attach
      (  Names_Grid,
         Item.Lecture_Name,
         1, 2,
         0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Attach
      (  Names_Grid,
         Item.Lecture_Name_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "output-file-label"));
      Attach
      (  Names_Grid,
         Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.File_Name_Edit);
      Attach
      (  Names_Grid,
         Item.File_Name_Edit,
         1, 2,
         1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.File_Name_Hint);
      Attach
      (  Names_Grid,
         Item.File_Name_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.File_Name_Hint, Item.Browser, None, True);

      Gtk_New (Range_Grid, 5, 6, False);
      Set_Col_Spacings (Range_Grid, Column_Spacing);
      Set_Row_Spacings (Range_Grid, Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "from-example-label"));
      Attach
      (  Range_Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.From_Edit);
      Item.From_Edit.Set_Width_Chars (6);
      if Find_Property (Item.From_Edit, "max-width-chars") /= null then
         Set_Property
         (  Item.From_Edit,
            Build ("max-width-chars"),
            GInt'(6)
         );
      end if;
      Set_Text (Item.From_Edit, "1");
      Attach
      (  Range_Grid,
         Item.From_Edit,
         1, 2,
         0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.From_Hint);
      Attach
      (  Range_Grid,
         Item.From_Hint,
         2, 3,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "to-example-label"));
      Attach
      (  Range_Grid,
         Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.To_Edit);
      Item.To_Edit.Set_Width_Chars (6);
      if Find_Property (Item.To_Edit, "max-width-chars") /= null then
         Set_Property
         (  Item.To_Edit,
            Build ("max-width-chars"),
            GInt'(6)
         );
      end if;
      Attach
      (  Range_Grid,
         Item.To_Edit,
         1, 2,
         1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.To_Hint);
      Attach
      (  Range_Grid,
         Item.To_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.To_Hint, Item.Browser, None, True);

      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "out-of-examples-label")
      );
      Attach
      (  Range_Grid,
         Label,
         3, 4,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.Total, "?");
      Attach
      (  Range_Grid,
         Item.Total,
         4, 6,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Item.Total.Set_Halign (Align_Start);
      Item.Total.Set_Valign (Align_Center);
--    Set_Alignment (Item.Total, 0.0, 0.5);
         -- Row 3
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "positive-examples-prefix-label")
      );
      Attach
      (  Range_Grid,
         Label,
         0, 1,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.Positive_Edit);
      Attach
      (  Range_Grid,
         Item.Positive_Edit,
         1, 5,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Set_Text (Item.Positive_Edit, "<+>");
      Gtk_New
      (  Item.Positive_Use,
         Style_Get (Item.Browser, "examples-write-check-button-label")
      );
      Set_Active (Item.Positive_Use, True);
      Attach
      (  Range_Grid,
         Item.Positive_Use,
         5, 6,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 4
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "negative-examples-prefix-label")
      );
      Attach
      (  Range_Grid,
         Label,
         0, 1,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.Negative_Edit);
      Attach
      (  Range_Grid,
         Item.Negative_Edit,
         1, 5,
         3, 4,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Set_Text (Item.Negative_Edit, "<->");
      Gtk_New
      (  Item.Negative_Use,
         Style_Get (Item.Browser, "examples-write-check-button-label")
      );
      Set_Active (Item.Negative_Use, True);
      Attach
      (  Range_Grid,
         Item.Negative_Use,
         5, 6,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 5
      Gtk_New
      (  Item.Delimiter_Frame,
         True,
         "delimiter-frame-label",
         Item.Browser
      );
      Attach
      (  Range_Grid,
         Item.Delimiter_Frame,
         0, 6,
         4, 5,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
         -- Features selection
      Gtk_New (Item.List);
      Set_Spacing (Item.List, GInt (Column_Spacing));
      Set_Spacing (Get_Button_Box (Item.List), GInt (Button_Spacing));
      Set_Title
      (  Get_Column (Get_Selected_Tree_View (Item.List), 0),
         Style_Get (Item.Browser, "selected-features-to-save-title")
      );
      Set_Title
      (  Get_Column (Get_Deselected_Tree_View (Item.List), 0),
         Style_Get (Item.Browser, "deselected-features-to-save-title")
      );

      Gtk_New_HBox (Parameters_Box);
      Set_Border_Width (Parameters_Box, Row_Spacing);
      Set_Spacing (Parameters_Box, GInt (Column_Spacing));
      Pack_Start (Parameters_Box, Range_Grid, False, False);
      Pack_Start (Parameters_Box, Item.List);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Parameters_Box);

      Gtk_New_Vpaned (Pane);
      Add1 (Pane, Frame);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New
      (  Widget  => Item.Directory,
         Browser => Item.Browser,
         Key     => Save_As_Text_Key
      );
      Add (Frame, Item.Directory);
      Add2 (Pane, Frame);
      Pack_Start (Item, Pane);

      Show_All (Item);

      Writer_Handlers.Connect
      (  Get_Entry (Item.Lecture_Name),
         "changed",
         Lecture_Name_Changed'Access,
         Item.all'Access
      );
      Writer_Handlers.Connect
      (  Item.From_Edit,
         "changed",
         From_Changed'Access,
         Item.all'Access
      );
      Writer_Handlers.Connect
      (  Item.To_Edit,
         "changed",
         To_Changed'Access,
         Item.all'Access
      );
      Writer_Handlers.Connect
      (  Item.Negative_Use,
         "toggled",
         Toggled'Access,
         Item.all'Access
      );
      Writer_Handlers.Connect
      (  Item.Positive_Use,
         "toggled",
         Toggled'Access,
         Item.all'Access
      );
      Writer_Handlers.Connect
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
      Lecture_Name_Changed (Item, Item.all'Access);

      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-save-lecture-label"),
         Style_Get (Item.Browser, "tab-save-lecture-icon"),
         Item
      );
   end Initialize;

   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
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
            Put (Item.List, Get_Features (Lesson));
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
            &  Where ("Lecture_Name_Changed")
         )  );
   end Lecture_Name_Changed;

   procedure Service
             (  Item : not null access Lecture_Save_As_Text_Box_Record
             )  is
      Features : Feature_Array renames To_Feature_Array (Item.Features);
      Parameters : constant Write_Parameters :=
      (  Delimiter_Length        => Length (Item.Delimiter),
         Positive_Example_Length => Length (Item.Positive_Example),
         Negative_Example_Length => Length (Item.Negative_Example),
         Abs_Small               => Write_Defaults.Abs_Small,
         Base                    => 10,
         Default                 => Write_Defaults.Default,
         Delimiter               => To_String (Item.Delimiter),
         Mode                    => Write_Defaults.Mode,
         Negative_Example        => To_String (Item.Negative_Example),
         Output_Negative         => Item.Write_Negative,
         Output_Positive         => Item.Write_Positive,
         Positive_Example        => To_String (Item.Positive_Example),
         Put_Plus                => False,
         Put_Units               => False,
         Quote_Units             => False,
         Rel_Small               => Write_Defaults.Rel_Small,
         Skip_Undefined          => True,
         Use_Derived             => True,
         Use_SI                  => False
      );
   begin
      Write
      (  File_Name  => To_String (Item.File_Name),
         Lesson     => Item.Lesson,
         Features   => Features,
         From       => Item.From,
         To         => Item.To,
         Parameters => Parameters,
         Viewer     => Ptr (Get_Indicator (Item.Indicator))
      );
   end Service;

   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
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

   procedure Toggled
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Lecture_Save_As_Text_Box
             )  is
   begin
      Set_Sensitive
      (  Item.Positive_Edit,
         Get_Active (Item.Positive_Use)
      );
      Set_Sensitive
      (  Item.Negative_Edit,
         Get_Active (Item.Negative_Use)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Toggled")
         )  );
   end Toggled;

end Gtk.Fuzzy_Catalogue.Lecture_Save_As_Text;
