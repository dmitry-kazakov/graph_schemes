--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Learn_Boxes                            Autumn, 2008       --
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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Deposit_Handles;          use Deposit_Handles;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Fuzzy.Graph.Scheme;       use Fuzzy.Graph.Scheme;
with Fuzzy.Persistence;        use Fuzzy.Persistence;
with GLib.Messages;            use GLib.Messages;
with GLib.Properties;          use GLib.Properties;
with Gtk.Alignment;            use Gtk.Alignment;
with Gtk.Indicators;           use Gtk.Indicators;
with Gtk.Missed;               use Gtk.Missed;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;        use Gtk.Widget.Styles;
with Indicator.Handle;         use Indicator.Handle;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Fuzzy.Feature.Handle.Bounded_Arrays;
with Gtk.Fuzzy_Catalogue.Feature_Pane;
with GLib.Object.Checked_Destroy;

with Gtk.Indicators.Graph_Training_Progress;
use  Gtk.Indicators.Graph_Training_Progress;

package body Gtk.Fuzzy_Catalogue.Learn_Boxes is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Learn_Boxes." & Name;
   end Where;

   procedure Aborted
             (  Item  : not null access Learn_Box_Record;
                Fault : Exception_Occurrence
             )  is
   begin
      Off (Item.Animation);
      Remove (Item, Item.Learn_Grid);
      Item.Pack_Start (Item.Config_Grid);
      if Exception_Identity (Fault) = Data_Error'Identity then
         Error (Item, Exception_Message (Fault));
      elsif Exception_Identity (Fault) = End_Error'Identity then
         Error (Item, Style_Get (Item.Browser, "canceled-error"));
      else
         Error (Item, "Fault: " & Exception_Information (Fault));
      end if;
   end Aborted;

   procedure Commit_Config (Item : access Learn_Box_Record'Class) is
      Path : constant Item_Path := Get_Path (Item.Lecture_Name);
   begin
      Item.Name := To_Unbounded_String (String (Path));
      Item.Lesson := Get_Lecture (Path, Item, Item.Lecture_Name_Hint);
      if not Is_Valid (Item.Lesson) then
         return;
      end if;
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
      begin
         Item.Threshold := Value (Get_Text (Item.Threshold_Entry));
         Set_Hint (Item.Threshold_Hint, Item.Browser, Checked, True);
      exception
         when others =>
            Error (Item, Style_Get (Item.Browser, "threshold-error"));
            Set_Hint
            (  Item.Threshold_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
      end;
      begin
         Item.Equivalence := Value (Get_Text (Item.Equivalence_Entry));
         Set_Hint (Item.Equivalence_Hint, Item.Browser, Checked, True);
      exception
         when others =>
            Error (Item, Style_Get (Item.Browser, "equivalence-error"));
            Set_Hint
            (  Item.Equivalence_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
      end;
      declare
         use Fuzzy.Feature.Handle.Bounded_Arrays;
         Features : Bounded_Array renames Get_Selected (Item.List);
      begin
         if Features.Last - Features.First < 1 then
            Error (Item, Style_Get (Item.Browser, "no-features-error"));
            return;
         end if;
         Erase (Item.Features);
         for Index in Features.First..Features.Last loop
            Put (Item.Features, Index, Get (Features, Index));
         end loop;
      end;
      --
      -- Start training
      --
      Clean (Item);
      Remove (Item, Item.Config_Grid);
      if Item.Learn_Grid = null then
         declare
            Label : Gtk_Label;
            Hint  : Gtk_Box;
         begin
            Gtk_New (Item.Learn_Grid, 3, 4, False);

            Set_Col_Spacings
            (  Item.Learn_Grid,
               Style_Get (Item.Browser, "column-spacing")
            );
            Set_Row_Spacings
            (  Item.Learn_Grid,
               Style_Get (Item.Browser, "row-spacing")
            );
               -- Row 1
            Gtk_New
            (  Label,
               Style_Get (Item.Browser, "classifier-name-label")
            );
            Attach
            (  Item.Learn_Grid,
               Label,
               0, 1,
               0, 1,
               Xoptions => Fill,
               Yoptions => Shrink
            );
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Gtk_New (Item.Name_Edit);
            Attach
            (  Item.Learn_Grid,
               Item.Name_Edit,
               1, 2,
               0, 1,
               Xoptions => Expand or Fill,
               Yoptions => Shrink
            );
            Gtk_New_HBox (Item.Name_Hint);
            Attach
            (  Item.Learn_Grid,
               Item.Name_Hint,
               2, 3,
               0, 1,
               Xoptions => Shrink,
               Yoptions => Shrink
            );
            Set_Hint (Item.Name_Hint, Item.Browser, None, True);
            Gtk_New (Item.Animation);
            Attach
            (  Item.Learn_Grid,
               Item.Animation,
               3, 4,
               0, 2,
               Xoptions => Shrink,
               Yoptions => Shrink
            );
               -- Row 2
            Gtk_New
            (  Label,
               Style_Get (Item.Browser, "folder-path-label")
            );
            Attach
            (  Item.Learn_Grid,
               Label,
               0, 1,
               1, 2,
               Xoptions => Fill,
               Yoptions => Shrink
            );
            Label.Set_Halign (Align_End);
            Label.Set_Valign (Align_Center);
            Gtk_New
            (  Item.Folder_Name,
               "classifier folder",
               Hint,
               Item
            );
            Combine
            (  Item.Constraint,
               Get_Constraint (Item.Folder_Name)
            );
            Attach
            (  Item.Learn_Grid,
               Item.Folder_Name,
               1, 2,
               1, 2,
               Xoptions => Expand or Fill,
               Yoptions => Shrink
            );
            Attach
            (  Item.Learn_Grid,
               Hint,
               2, 3,
               1, 2,
               Xoptions => Shrink,
               Yoptions => Shrink
            );

            Ref (Item.Learn_Grid);
         end;
      end if;
      On (Item.Animation);
      Item.Learn_Grid.Show_All;
      Item.Pack_Start (Item.Learn_Grid);
      Item.Trained := False;
      Start_Servicing (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Config")
         )  );
   end Commit_Config;

   procedure Commit_Trained (Item : access Learn_Box_Record'Class) is
      Store  : Storage_Handle;
      Folder : Deposit_Handle;
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
         if Name'Length = 0 then
            Set_Hint (Item.Name_Hint, Item.Browser, Erroneous, True);
            return;
         end if;
         begin
            Item.Animation.On;
            Put (Store, Item.Classifier, Name, Folder);
            Item.Animation.Off;
         exception
            when others =>
               Item.Animation.Off;
               raise;
         end;
         --
         -- Update the cache
         --
         declare
            Changed : Boolean;
            Class   : constant String :=
                        Get_Class (To_Deposit_Handle (Item.Classifier));
         begin
            Created
            (   Store     => Get_Cache (Item.Browser),
                Directory => Get_Path (Item.Folder_Name),
                Item      => (  Name_Length => Name'Length,
                                Kind_Length => Class'Length,
                                Policy      => Cache_Expanded,
                                Directory   => False,
                                Name        => Item_Name (Name),
                                Kind        => Item_Type (Class)
            )                );
            if Item.Browser.Classifiers /= null then
               -- Switch to the classifier panel and select the result
               Switch_To (Item.Browser.Classifiers, Item.Browser);
               Item.Browser.Tree.Set_Current_Directory
               (  Get_Path (Item.Folder_Name)
               );
               Item.Browser.Classifiers.List.Move
               (  Changed,
                  0,
                  Item.Browser.Classifiers.List.Get_Index
                  (  Item_Name (Name)
               )  );
            end if;
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
   end Commit_Trained;

   procedure Commit (Item : not null access Learn_Box_Record) is
   begin
      if Item.Trained then
         Commit_Trained (Item);
      else
         Commit_Config (Item);
      end if;
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

   procedure Completed (Item : not null access Learn_Box_Record) is
   begin
      Off (Item.Animation);
      Item.Trained := True;
      Add_Previous (Item);
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

   function Create_Indicator (Item : not null access Learn_Box_Record)
      return Gtk_Indicator is
      Progress : Gtk_Graph_Training_Progress;
      State    : Gtk_Training_State;
      Button   : Abort_Buttons.Gtk_Style_Button;
   begin
      Gtk_New (Button);
      Gtk_New (State);
      Attach
      (  Item.Learn_Grid,
         State,
         0, 4,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Fill or Expand
      );
      Show_All (State);
      Gtk_New
      (  Widget  => Progress,
         Button  => Button,
         State   => State,
         Spacing =>
            (  Width  => Style_Get (Item.Browser, "row-spacing"),
               Height => Style_Get (Item.Browser, "column-spacing")
      )     );
      return Progress.all'Unchecked_Access;
   end Create_Indicator;

   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             )  is
   begin
      Select_Features (Item.List, Select_Features (Item));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Down")
         )  );
   end Down;

   procedure Equivalence_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             )  is
      Equivalence : Confidence;
   begin
      Equivalence := Value (Get_Text (Item.Equivalence_Entry));
      Set_Hint (Item.Equivalence_Hint, Item.Browser, Checked, True);
   exception
      when others =>
         Set_Hint
         (  Item.Equivalence_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Equivalence_Changed;

   procedure Finalize (Item : not null access Learn_Box_Record) is
   begin
      Unref (Item.Config_Grid);
      if Item.Learn_Grid /= null then
         Unref (Item.Learn_Grid);
      end if;
      Finalize (Gtk_Item_Box_Record (Item.all)'Access);
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

   procedure From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
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

   --  function Get_Lecture
   --           (  Item : access Learn_Box_Record
   --           )  return Lecture_Handle is
   --     Store  : Storage_Handle;
   --     Object : Deposit_Handle;
   --     Lesson : Lecture_Handle;
   --  begin
   --     Get (Item.Browser.Lectures, Unconstrained, Store, Object);
   --     if Is_Valid (Object) then
   --        begin
   --           Lesson := To_Lecture_Handle (Object);
   --        exception
   --           when Constraint_Error =>
   --              null;
   --        end;
   --     end if;
   --     return Lesson;
   --  exception
   --     when Error : others =>
   --        Log
   --        (  Fuzzy_ML_Domain,
   --           Log_Level_Critical,
   --           (  "Fault: "
   --           &  Exception_Information (Error)
   --           &  Where ("Get_Lecture")
   --        )  );
   --        return Lesson;
   --  end Get_Lecture;
   --
   procedure Gtk_New
             (  Item        : out Learn_Box;
                Browser     : not null access
                              Gtk_Fuzzy_Catalogue_Record'Class;
                Set_Lecture : Boolean
             )  is
   begin
      Item := new Learn_Box_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Learn_Boxes.Initialize (Item, Set_Lecture);
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
             (  Item        : not null access Learn_Box_Record'Class;
                Set_Lecture : Boolean
             )  is
      Label     : Gtk_Label;
      Alignment : Gtk_Alignment;
      Button_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Gtk.Fuzzy_Catalogue.Initialize (Item, "training box", True);
      Gtk_New (Item.Config_Grid, 4, 9, False);
      Set_Col_Spacings (Item.Config_Grid, Column_Spacing);
      Set_Row_Spacings (Item.Config_Grid, Row_Spacing);
         -- Row 1
      Gtk_New (Label, Style_Get (Item.Browser, "lecture-name-label"));
      Attach
      (  Item.Config_Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New
      (  Widget     => Item.Lecture_Name,
         Name       => "set name",
         Hint       => Item.Lecture_Name_Hint,
         Picker     => Item.Browser.Lectures,
         Initialize => Set_Lecture
      );
      Combine (Item.Constraint, Get_Constraint (Item.Lecture_Name));
      Attach
      (  Item.Config_Grid,
         Item.Lecture_Name,
         1, 8,
         0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Attach
      (  Item.Config_Grid,
         Item.Lecture_Name_Hint,
         8, 9,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
         -- Row 2
      Gtk_New (Label, Style_Get (Item.Browser, "from-example-label"));
      Attach
      (  Item.Config_Grid,
         Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.From_Edit);
      Item.From_Edit.Set_Width_Chars (10);
      if Find_Property (Item.From_Edit, "max-width-chars") /= null then
         Set_Property
         (  Item.From_Edit,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Set_Text (Item.From_Edit, "1");
      Attach
      (  Item.Config_Grid,
         Item.From_Edit,
         1, 2,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.From_Hint);
      Attach
      (  Item.Config_Grid,
         Item.From_Hint,
         2, 3,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
      Gtk_New (Label, Style_Get (Item.Browser, "to-example-label"));
      Attach
      (  Item.Config_Grid,
         Label,
         3, 4,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New (Item.To_Edit);
      Item.To_Edit.Set_Width_Chars (10);
      if Find_Property (Item.To_Edit, "max-width-chars") /= null then
         Set_Property
         (  Item.To_Edit,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Attach
      (  Item.Config_Grid,
         Item.To_Edit,
         4, 5,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.To_Hint);
      Attach
      (  Item.Config_Grid,
         Item.To_Hint,
         5, 6,
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
      (  Item.Config_Grid,
         Label,
         6, 7,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Gtk_New (Item.Total, "?");
      Item.Total.Set_Halign (Align_Start);
      Item.Total.Set_Valign (Align_Center);
--    Set_Alignment (Item.Total, 0.0, 0.5);
      Attach
      (  Item.Config_Grid,
         Item.Total,
         7, 8,
         1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
         -- Row 3
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "learn-threshold-label")
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Config_Grid,
         Label,
         0, 1,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New (Item.Threshold_Entry);
      Item.Threshold_Entry.Set_Width_Chars (10);
      if Find_Property (Item.Threshold_Entry, "max-width-chars") /= null
      then
         Set_Property
         (  Item.Threshold_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Set_Text (Item.Threshold_Entry, "0");
      Attach
      (  Item.Config_Grid,
         Item.Threshold_Entry,
         1, 2,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Threshold_Hint);
      Attach
      (  Item.Config_Grid,
         Item.Threshold_Hint,
         2, 3,
         2, 3,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Threshold_Hint, Item.Browser, Checked, True);
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "learn-equivalence-label")
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Config_Grid,
         Label,
         3, 4,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New (Item.Equivalence_Entry);
      Item.Equivalence_Entry.Set_Width_Chars (10);
      if (  Find_Property (Item.Equivalence_Entry, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Item.Equivalence_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      Set_Text (Item.Equivalence_Entry, "1");
      Attach
      (  Item.Config_Grid,
         Item.Equivalence_Entry,
         4, 5,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Equivalence_Hint);
      Attach
      (  Item.Config_Grid,
         Item.Equivalence_Hint,
         5, 6,
         2, 3,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
      Set_Hint (Item.Equivalence_Hint, Item.Browser, Checked, True);
      Gtk_New (Alignment, 0.8, 0.5, 0.5, 0.5);
      Gtk_New_HBox (Item.Feature_Buttons);
      Add (Alignment, Item.Feature_Buttons);
      Set_Spacing (Item.Feature_Buttons, GInt (Button_Spacing));
      Attach
      (  Item.Config_Grid,
         Alignment,
         7, 9,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
         -- Row 4
      Gtk_New (Item.List);
      Set_Spacing (Item.List, GInt (Column_Spacing));
      Set_Spacing (Get_Button_Box (Item.List), GInt (Button_Spacing));
      Set_Title
      (  Get_Column (Get_Deselected_Tree_View (Item.List), 0),
         Style_Get (Item.Browser, "learn-unused-title")
      );
      Set_Title
      (  Get_Column (Get_Selected_Tree_View (Item.List), 0),
         Style_Get (Item.Browser, "learn-used-title")
      );
      Attach
      (  Item.Config_Grid,
         Item.List,
         0, 9,
         3, 4,
         Xoptions => Fill or Expand,
         Yoptions => Fill or Expand
      );
      Get_External_Buttons
      (  Item.List,
         Item.Up_Button,
         Item.Down_Button
      );
      Pack_Start (Item.Feature_Buttons, Item.Up_Button,   False, False);
      Pack_Start (Item.Feature_Buttons, Item.Down_Button, False, False);

      if Item.Browser.Features /= null then
         Set
         (  Item.Directory_Changed,
            Trainer_Handlers.Connect
            (  Item.Browser.Features.List,
               "directory-changed",
               Trainer_Handlers.To_Marshaller (State_Changed'Access),
               Item.all'Unchecked_Access
         )  );
         Set
         (  Item.Selection_Changed,
            Trainer_Handlers.Connect
            (  Item.Browser.Features.List,
               "selection-changed",
               Trainer_Handlers.To_Marshaller (State_Changed'Access),
               Item.all'Unchecked_Access
         )  );
         Set
         (  Item.Panel_Switched,
            Trainer_Handlers.Connect
            (  Item.Browser.List_Tabs,
               "switch_page",
               Trainer_Handlers.To_Marshaller (State_Changed'Access),
               Item.all'Unchecked_Access
         )  );
      end if;

      Item.Pack_Start (Item.Config_Grid);
      Ref (Item.Config_Grid);

      Show_All (Item);

      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-learn-label"),
         Style_Get (Item.Browser, "tab-classifier-icon"),
         Item
      );
      Trainer_Handlers.Connect
      (  Get_Entry (Item.Lecture_Name),
         "changed",
         Lecture_Name_Changed'Access,
         Item.all'Access
      );
      Trainer_Handlers.Connect
      (  Item.From_Edit,
         "changed",
         From_Changed'Access,
         Item.all'Access
      );
      Trainer_Handlers.Connect
      (  Item.Equivalence_Entry,
         "changed",
         Equivalence_Changed'Access,
         Item.all'Access
      );
      Trainer_Handlers.Connect
      (  Item.Threshold_Entry,
         "changed",
         Threshold_Changed'Access,
         Item.all'Access
      );
      Trainer_Handlers.Connect
      (  Item.To_Edit,
         "changed",
         To_Changed'Access,
         Item.all'Access
      );
      Trainer_Handlers.Connect
      (  Item.Down_Button,
         "clicked",
         Down'Access,
         Item.all'Access
      );
      if Set_Lecture then
         Lecture_Name_Changed (Item, Item.all'Access);
      end if;
   end Initialize;

   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             )  is
      Path   : constant Item_Path := Get_Path (Item.Lecture_Name);
      Lesson : constant Lecture_Handle :=
                  Get_Lecture (Path, Item, Item.Lecture_Name_Hint);
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

   procedure Previous (Item : not null access Learn_Box_Record) is
   begin
      Remove_Previous (Item);
      Clean (Item);
      Remove (Item, Item.Learn_Grid);
      Item.Pack_Start (Item.Config_Grid);
      Item.Trained := False;
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

   function Select_Features
            (  Widget : not null access Learn_Box_Record
            )  return Fuzzy.Feature.Handle.Container.Set is
      use Gtk.Fuzzy_Catalogue.Feature_Pane;
      Store    : Storage_Handle;
      Features : Fuzzy.Feature.Handle.Container.Set;
   begin
      if Widget.Browser.Features /= null then
         Get_Selected
         (  Feature_Panel_Record'Class
            (  Widget.Browser.Features.all
            ) 'Unchecked_Access,
            Store,
            Features
         );
      end if;
      return Features;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Select_Features")
         )  );
         return Features;
   end Select_Features;

   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             )  is
   begin
      Check_Features (Item.List, Select_Features (Item));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("State_Changed")
         )  );
   end State_Changed;

   procedure Threshold_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
             )  is
      Threshold : Confidence;
   begin
      Threshold := Value (Get_Text (Item.Threshold_Entry));
      Set_Hint (Item.Threshold_Hint, Item.Browser, Checked, True);
   exception
      when others =>
         Set_Hint (Item.Threshold_Hint, Item.Browser, Erroneous, True);
   end Threshold_Changed;

   procedure To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Learn_Box
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

   procedure Service (Item : not null access Learn_Box_Record) is
      Features : Feature_Array renames
                    To_Feature_Array (Item.Features);
   begin
      Item.Classifier :=
         Fuzzy.Graph.Scheme.Create
         (  Lesson      => Item.Lesson,
            Name        => To_String (Item.Name),
            Features    => Features (1..Features'Last - 1),
            Classes     => Features (Features'Last),
            From        => Item.From,
            To          => Item.To,
            Threshold   => Item.Threshold,
            Equivalence => Item.Equivalence,
            Viewer      => Ptr (Get_Indicator (Item.Indicator))
         );
   end Service;

end Gtk.Fuzzy_Catalogue.Learn_Boxes;
