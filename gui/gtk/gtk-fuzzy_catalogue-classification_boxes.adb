--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Classification_Boxes                   Spring, 2008       --
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

with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Fuzzy.Lecture;            use Fuzzy.Lecture;
with Fuzzy.Lecture.Composite;  use Fuzzy.Lecture.Composite;
with Fuzzy.Lecture.Subrange;   use Fuzzy.Lecture.Subrange;
with Glib.Messages;            use Glib.Messages;
with GLib.Properties;          use GLib.Properties;
with Gtk.Fuzzy_Lecture;        use Gtk.Fuzzy_Lecture;
with Gtk.Indicators;           use Gtk.Indicators;
with Gtk.Missed;               use Gtk.Missed;
with Gtk.Widget.Styles;        use Gtk.Widget.Styles;
with Indicator.Handle;         use Indicator.Handle;
with Strings_Edit.Integers;    use Strings_Edit.Integers;

with Fuzzy.Feature.Handle.Bounded_Arrays;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Classification_Boxes is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Classification_Boxes." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Fuzzy.Intuitionistic.Set,
             Set_Ptr
          );

   procedure Set_Lesson_Range
             (  Item : access Classification_Box_Record'Class
             )  is
      Total : constant String :=
                 Strings_Edit.Integers.Image
                 (  Get_Examples_Number (Item.Lesson)
                 );
   begin
      if Item.Source_From > 1 then
         Set_Text (Item.From_Edit, "1");
         Set_Text
         (  Item.To_Edit,
            Strings_Edit.Integers.Image (Item.Source_From - 1)
         );
      elsif Item.Source_To < Get_Examples_Number (Item.Lesson) then
         Set_Text
         (  Item.From_Edit,
            Strings_Edit.Integers.Image (Item.Source_To + 1)
         );
         Set_Text (Item.To_Edit, Total);
      else
         Set_Text (Item.From_Edit, "1");
         Set_Text (Item.To_Edit, Total);
      end if;
      Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
      Set_Hint (Item.To_Hint,   Item.Browser, Checked, True);
      Set_Text (Item.Total, Total);
   end Set_Lesson_Range;

   procedure Classifier_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             )  is
   begin
      Item.Classifier :=
         Get_Classifier
         (  Get_Path (Item.Classifier_Name),
            Item,
            Item.Classifier_Name_Hint
         );
      if Item.Verify and then Is_Valid (Item.Classifier) then
         declare
            Name : constant Item_Path :=
                   Item_Path (Get_Training_Set_Name (Item.Classifier));
         begin
            if Name'Length > 0 then
               Item.Source := Get_Lecture (Name, Item);
               if Is_Valid (Item.Source) then
                  Set_Path (Item.Source_Name, Name);
                  Item.Source_From :=
                     Get_Training_Set_From (Item.Classifier);
                  Item.Source_To :=
                     (  Item.Source_From
                     +  Get_Training_Set_Length (Item.Classifier)
                     -  1
                     );
                  declare
                     Total : constant Natural :=
                                Get_Examples_Number (Item.Source);
                  begin
                     Item.Source_To :=
                        Integer'Min (Total, Item.Source_To);
                     Item.Source_From :=
                        Integer'Min (Item.Source_From, Item.Source_To);
                     if Item.Source = Item.Lesson then
                        Set_Lesson_Range (Item);
                     end if;
                     if Item.Source_From > 0 then
                        Set_Text
                        (  Item.Source_From_Edit,
                           Strings_Edit.Integers.Image
                           (  Item.Source_From
                        )  );
                        Set_Hint
                        (  Item.Source_From_Hint,
                           Item.Browser,
                           Checked,
                           True
                        );
                     else
                        Set_Text (Item.Source_From_Edit, "");
                        Set_Hint
                        (  Item.Source_From_Hint,
                           Item.Browser,
                           None,
                           True
                        );
                     end if;
                     if Item.Source_To > 0 then
                        Set_Text
                        (  Item.Source_To_Edit,
                           Strings_Edit.Integers.Image (Item.Source_To)
                        );
                        Set_Hint
                        (  Item.Source_To_Hint,
                           Item.Browser,
                           Checked,
                           True
                        );
                     else
                        Set_Text (Item.Source_To_Edit, "");
                        Set_Hint
                        (  Item.Source_To_Hint,
                           Item.Browser,
                           None,
                           True
                        );
                     end if;
                  end;
               end if;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Classifier_Name_Changed")
         )  );
   end Classifier_Name_Changed;

   procedure Commit
             (  Item : not null access Classification_Box_Record
             )  is
   begin
      Item.Classifier :=
         Get_Classifier
         (  Get_Path (Item.Classifier_Name),
            Item,
            Item.Classifier_Name_Hint
         );
      if not Is_Valid (Item.Classifier) then
         return;
      end if;
      Item.Lesson :=
         Get_Lecture
         (  Get_Path (Item.Lecture_Name),
            Item,
            Item.Lecture_Name_Hint
         );
      if not Is_Valid (Item.Lesson) then
         return;
      end if;
      if Item.Verify then
         declare
            Path : constant Item_Path := Get_Path (Item.Source_Name);
         begin
            if Path'Length > 0 then
               Item.Source :=
                  Get_Lecture (Path, Item, Item.Source_Name_Hint);
            else
               Invalidate (Item.Source);
            end if;
         end;
         if Is_Valid (Item.Source) then
            begin
               Item.Source_From :=
                  Value (Get_Text (Item.Source_From_Edit));
               if Item.Source_From in 1
                                   .. Get_Examples_Number (Item.Source)
               then
                  Set_Hint
                  (  Item.Source_From_Hint,
                     Item.Browser,
                     Checked,
                     True
                  );
               else
                  Set_Hint
                  (  Item.Source_From_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  Error
                  (  Item,
                     Style_Get
                     (  Item.Browser,
                        "from-example-range-error"
                  )  );
                  return;
               end if;
            exception
               when Constraint_Error =>
                  Error
                  (  Item,
                     Style_Get
                     (  Item.Browser,
                        "from-example-range-error"
                  )  );
                  Set_Hint
                  (  Item.Source_From_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               when others =>
                  Error
                  (  Item,
                     Style_Get (Item.Browser, "from-example-error")
                  );
                  Set_Hint
                  (  Item.Source_From_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
            end;
            begin
               Item.Source_To := Value (Get_Text (Item.Source_To_Edit));
               if Item.Source_To in 1
                                 .. Get_Examples_Number (Item.Source)
               then
                  if Item.Source_To >= Item.Source_From then
                     Set_Hint
                     (  Item.Source_To_Hint,
                        Item.Browser,
                        Checked,
                        True
                     );
                  else
                     Error
                     (  Item,
                        Style_Get (Item.Browser, "examples-range-error")
                     );
                     Set_Hint
                     (  Item.Source_From_Hint,
                        Item.Browser,
                        Erroneous,
                        True
                     );
                     Set_Hint
                     (  Item.Source_To_Hint,
                        Item.Browser,
                        Erroneous,
                        True
                     );
                     return;
                  end if;
               else
                  Set_Hint
                  (  Item.Source_To_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
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
                  Set_Hint
                  (  Item.Source_To_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
               when others =>
                  Error
                  (  Item,
                     Style_Get (Item.Browser, "to-example-error")
                  );
                  Set_Hint
                  (  Item.Source_To_Hint,
                     Item.Browser,
                     Erroneous,
                     True
                  );
                  return;
            end;
         end if;
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
            Error
            (  Item,
               Style_Get (Item.Browser, "threshold-error")
            );
            Set_Hint
            (  Item.Threshold_Hint,
               Item.Browser,
               Erroneous,
               True
            );
            return;
      end;
      Item.Generalize := Get (Item.Generalize_Combo);
      Start_Servicing (Item);
   end Commit;

   procedure Completed
             (  Item : not null access Classification_Box_Record
             )  is
   begin
      Remove (Item, Item.Classification_Grid);
      Remove_Buttons (Item);
      if Item.Verify then
         declare
            Report : Gtk_Fuzzy_Classifier_Report_Box;
         begin
            Gtk_New (Report, Item);
            Pack_Start (Item, Report);
            if Is_Valid (Item.Source) then
               Item.Source_Slice :=
                  Create
                  (  Item.Source,
                     Item.Source_From,
                     Item.Source_To
                  );
               Put
               (  Widget     => Report,
                  Classifier => Item.Classifier,
                  Control    => Item.Lesson_Slice,
                  Result     => Item.Result,
                  Source     => Item.Source_Slice,
                  Report     => Item.Data.all
               );
            else
               Put
               (  Widget     => Report,
                  Classifier => Item.Classifier,
                  Control    => Item.Lesson_Slice,
                  Result     => Item.Result,
                  Report     => Item.Data.all
               );
            end if;
            Show_All (Report);
         end;
      else
         declare
            use Fuzzy.Feature.Handle.Bounded_Arrays;
            Frame    : Gtk_Frame;
            Report   : Gtk_Fuzzy_Lecture;
            Feature  : Feature_Handle;
            Classes  : constant Feature_Handle :=
                          Get_Classes (Item.Classifier);
            Features : constant Bounded_Array :=
                          Get_Features (Item.Lesson_Slice);
            Mapping  : Feature_To_Lecture_Map
                       (  1
                       .. Features.Last - Features.First + 2
                       );
            Position : Positive := 1;
         begin
            Mapping (Position) := (Classes, Item.Result);
            Position := Position + 1;
            for Index in Features.First..Features.Last loop
               Feature := Ref (Features, Index);
               if Feature /= Classes then
                  Mapping (Position) := (Feature, Item.Lesson_Slice);
                  Position := Position + 1;
               end if;
            end loop;
            Gtk_New (Report);
            Put (Report, Create (Mapping (1..Position - 1)));
            Gtk_New (Frame);
            Set_Shadow_Type (Frame, Shadow_In);
            Add (Frame, Report);
            Pack_Start (Item, Frame);
            Show_All (Frame);
         end;
      end if;
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

   procedure Error
             (  Widget  : not null access
                          Gtk_Fuzzy_Classifier_Report_Box_Record;
                Message : UTF8_String
             )  is
   begin
      Error (Widget.Item, Message);
   end Error;

   procedure Error_Reset
             (  Widget : not null access
                         Gtk_Fuzzy_Classifier_Report_Box_Record
             )  is
   begin
      Clean (Widget.Item);
   end Error_Reset;

   procedure Finalize
             (  Item : not null access Classification_Box_Record
             )  is
   begin
      Free (Item.Data);
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
                Item   : Classification_Box
             )  is
   begin
      if Is_Valid (Item.Lesson) then
         Item.From := Value (Get_Text (Item.From_Edit));
         if Item.From in 1..Get_Examples_Number (Item.Lesson) then
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
             (  Widget : out Gtk_Fuzzy_Classifier_Report_Box;
                Item   : not null access Classification_Box_Record'Class
             )  is
   begin
      Widget :=
         new Gtk_Fuzzy_Classifier_Report_Box_Record
             (  Item.all'Unchecked_Access
             );
      begin
         Gtk.Fuzzy_Classifier_Report.Initialize (Widget);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Gtk_Fuzzy_Classifier_Report_Box)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Item           : out Classification_Box;
                Browser        : not null access
                                 Gtk_Fuzzy_Catalogue_Record'Class;
                Verify         : Boolean;
                Set_Classifier : Boolean;
                Set_Lecture    : Boolean
             )  is
   begin
      Item :=
         new Classification_Box_Record (Browser.all'Unchecked_Access);
      begin
         Gtk.Fuzzy_Catalogue.Classification_Boxes.Initialize
         (  Item           => Item,
            Verify         => Verify,
            Set_Classifier => Set_Classifier,
            Set_Lecture    => Set_Lecture
         );
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New (Classification_Box)")
            )  );
            GLib.Object.Checked_Destroy (Item);
            Item := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Item           : not null access
                                 Classification_Box_Record'Class;
                Verify         : Boolean;
                Set_Classifier : Boolean;
                Set_Lecture    : Boolean
             )  is
      Label : Gtk_Label;
   -- Button_Spacing : constant GUInt :=
   --                     Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Gtk.Fuzzy_Catalogue.Initialize (Item, "classification box", True);
      Item.Verify := Verify;
      if Verify then
         Gtk_New (Item.Classification_Grid, 7, 9, False);
      else
         Gtk_New (Item.Classification_Grid, 4, 9, False);
      end if;
      Set_Col_Spacings (Item.Classification_Grid, Column_Spacing);
      Set_Row_Spacings (Item.Classification_Grid, Row_Spacing);
         -- Row 1
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "classifier-name-label")
      );
      Attach
      (  Item.Classification_Grid,
         Label,
         0, 1,
         0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Gtk_New
      (  Item.Classifier_Name,
         "classifier name",
         Item.Classifier_Name_Hint,
         Item.Browser.Classifiers,
         True
      );
      Combine (Item.Constraint, Get_Constraint (Item.Classifier_Name));
      Attach
      (  Item.Classification_Grid,
         Item.Classifier_Name,
         1, 8,
         0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Attach
      (  Item.Classification_Grid,
         Item.Classifier_Name_Hint,
         8, 9,
         0, 1,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
         -- Row 2
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "classify-lecture-name-label")
      );
      Attach
      (  Item.Classification_Grid,
         Label,
         0, 1,
         1, 2,
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
      Combine (Item.Constraint, Get_Constraint (Item.Lecture_Name));
      Attach
      (  Item.Classification_Grid,
         Item.Lecture_Name,
         1, 8,
         1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Attach
      (  Item.Classification_Grid,
         Item.Lecture_Name_Hint,
         8, 9,
         1, 2,
         Xoptions => Shrink,
         Yoptions => Shrink
      );
         -- Row 3
      Gtk_New (Label, Style_Get (Item.Browser, "from-example-label"));
      Attach
      (  Item.Classification_Grid,
         Label,
         0, 1,
         2, 3,
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
      (  Item.Classification_Grid,
         Item.From_Edit,
         1, 2,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.From_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.From_Hint,
         2, 3,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
      Gtk_New
      (  Label,
         Style_Get (Item.Browser, "classify-threshold-label")
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Classification_Grid,
         Label,
         3, 4,
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
      (  Item.Classification_Grid,
         Item.Threshold_Entry,
         4, 5,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Threshold_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.Threshold_Hint,
         5, 6,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.Threshold_Hint, Item.Browser, Checked, True);
      Gtk_New (Label, Style_Get (Item.Browser, "generalization-label"));
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Classification_Grid,
         Label,
         6, 7,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New (Item.Generalize_Combo, Nearest);
      Attach
      (  Item.Classification_Grid,
         Item.Generalize_Combo,
         7, 8,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Generalize_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.Generalize_Hint,
         8, 9,
         2, 3,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.Generalize_Hint, Item.Browser, Checked, True);
         -- Row 4
      Gtk_New (Label, Style_Get (Item.Browser, "to-example-label"));
      Attach
      (  Item.Classification_Grid,
         Label,
         0, 1,
         3, 4,
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
      (  Item.Classification_Grid,
         Item.To_Edit,
         1, 2,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.To_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.To_Hint,
         2, 3,
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
      (  Item.Classification_Grid,
         Label,
         3, 4,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New (Item.Total, "?");
      Item.Total.Set_Halign (Align_Start);
      Item.Total.Set_Valign (Align_Center);
--    Set_Alignment (Item.Total, 0.0, 0.5);
      Attach
      (  Item.Classification_Grid,
         Item.Total,
         4, 9,
         3, 4,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      if Verify then
            -- Row 5
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "classify-source-name-label")
         );
         Attach
         (  Item.Classification_Grid,
            Label,
            0, 1,
            4, 5,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New
         (  Item.Source_Name,
            "source name",
            Item.Source_Name_Hint,
            Item.Browser.Lectures,
            True,
            False
         );
         Combine (Item.Constraint, Get_Constraint (Item.Source_Name));
         Attach
         (  Item.Classification_Grid,
            Item.Source_Name,
            1, 8,
            4, 5,
            Xoptions => Fill or Expand,
            Yoptions => Shrink
         );
         Attach
         (  Item.Classification_Grid,
            Item.Source_Name_Hint,
            8, 9,
            4, 5,
            Xoptions => Shrink,
            Yoptions => Shrink
         );
            -- Row 6
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "from-example-label")
         );
         Attach
         (  Item.Classification_Grid,
            Label,
            0, 1,
            5, 6,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Source_From_Edit);
         Set_Sensitive (Item.Source_From_Edit, False);
         Item.Source_From_Edit.Set_Width_Chars (10);
         if (  Find_Property (Item.Source_From_Edit, "max-width-chars")
            /= null
            )
         then
            Set_Property
            (  Item.Source_From_Edit,
               Build ("max-width-chars"),
               GInt'(10)
            );
         end if;
         Set_Text (Item.Source_From_Edit, "1");
         Attach
         (  Item.Classification_Grid,
            Item.Source_From_Edit,
            1, 2,
            5, 6,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Source_From_Hint);
         Attach
         (  Item.Classification_Grid,
            Item.Source_From_Hint,
            2, 3,
            5, 6,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Set_Hint (Item.Source_From_Hint, Item.Browser, Checked, True);
            -- Row 7
         Gtk_New (Label, Style_Get (Item.Browser, "to-example-label"));
         Attach
         (  Item.Classification_Grid,
            Label,
            0, 1,
            6, 7,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Gtk_New (Item.Source_To_Edit);
         Set_Sensitive (Item.Source_To_Edit, False);
         Item.Source_To_Edit.Set_Width_Chars (10);
         if (  Find_Property (Item.Source_To_Edit, "max-width-chars")
            /= null
            )
         then
            Set_Property
            (  Item.Source_To_Edit,
               Build ("max-width-chars"),
               GInt'(10)
            );
         end if;
         Attach
         (  Item.Classification_Grid,
            Item.Source_To_Edit,
            1, 2,
            6, 7,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Gtk_New_HBox (Item.Source_To_Hint);
         Attach
         (  Item.Classification_Grid,
            Item.Source_To_Hint,
            2, 3,
            6, 7,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Set_Hint (Item.Source_To_Hint, Item.Browser, None, True);
         Gtk_New
         (  Label,
            Style_Get (Item.Browser, "out-of-examples-label")
         );
         Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
         Attach
         (  Item.Classification_Grid,
            Label,
            3, 4,
            6, 7,
            Xoptions => Fill,
            Yoptions => Shrink
         );
         Gtk_New (Item.Source_Total, "?");
         Item.Source_Total.Set_Halign (Align_Start);
         Item.Source_Total.Set_Valign (Align_Center);
--       Set_Alignment (Item.Source_Total, 0.0, 0.5);
         Attach
         (  Item.Classification_Grid,
            Item.Source_Total,
            4, 9,
            6, 7,
            Xoptions => Fill,
            Yoptions => Shrink
         );
      end if;
      Pack_Start (Item, Item.Classification_Grid, False, False);
      Show_All (Item);

      if Verify then
         Add_Item
         (  Item.Browser,
            Style_Get (Item.Browser, "tab-verify-label"),
            Style_Get (Item.Browser, "tab-verify-icon"),
            Item
         );
      else
         Add_Item
         (  Item.Browser,
            Style_Get (Item.Browser, "tab-classify-label"),
            Style_Get (Item.Browser, "tab-classify-icon"),
            Item
         );
      end if;

      Classification_Handlers.Connect
      (  Item.From_Edit,
         "changed",
         From_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Get_Entry (Item.Classifier_Name),
         "changed",
         Classifier_Name_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Get_Entry (Item.Lecture_Name),
         "changed",
         Lecture_Name_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Item.Threshold_Entry,
         "changed",
         Threshold_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Item.To_Edit,
         "changed",
         To_Changed'Access,
         Item.all'Access
      );
      if Verify then
         Classification_Handlers.Connect
         (  Item.Source_From_Edit,
            "changed",
            Source_From_Changed'Access,
            Item.all'Access
         );
         Classification_Handlers.Connect
         (  Get_Entry (Item.Source_Name),
            "changed",
            Source_Name_Changed'Access,
            Item.all'Access
         );
         Classification_Handlers.Connect
         (  Item.Source_To_Edit,
            "changed",
            Source_To_Changed'Access,
            Item.all'Access
         );
      end if;
      if Set_Classifier then
         Classifier_Name_Changed (Item, Item.all'Access);
      end if;
      if Set_Lecture then
         Lecture_Name_Changed (Item, Item.all'Access);
      end if;
   end Initialize;

   procedure Lecture_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             )  is
   begin
      Item.Lesson :=
         Get_Lecture
         (  Get_Path (Item.Lecture_Name),
            Item,
            Item.Lecture_Name_Hint
         );
      if Is_Valid (Item.Lesson) then
         if Item.Lesson = Item.Source then  -- Same test and source
            Set_Lesson_Range (Item);
         else
            declare
               Total : constant String :=
                          Strings_Edit.Integers.Image
                          (  Get_Examples_Number (Item.Lesson)
                          );
            begin
               Set_Text (Item.From_Edit, "1");
               Set_Hint (Item.From_Hint, Item.Browser, Checked, True);
               Set_Text (Item.To_Edit, Total);
               Set_Hint (Item.To_Hint, Item.Browser, Checked, True);
               Set_Text (Item.Total, Total);
            end;
         end if;
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
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Lecture_Name_Changed")
         )  );
   end Lecture_Name_Changed;

   procedure Service
             (  Item : not null access Classification_Box_Record
             )  is
   begin
      Free (Item.Data);
      Item.Data :=
         new Fuzzy.Intuitionistic.Set
             (  Get_Cardinality (Get_Classes (Item.Classifier))
             );
      Invalidate (Item.Result);
      Item.Lesson_Slice := Create (Item.Lesson, Item.From, Item.To);
      Verify
      (  Classifier  => Item.Classifier,
         Lesson      => Item.Lesson_Slice,
         Result      => Item.Result,
         Report      => Item.Data.all,
         Generalize  => Item.Generalize,
         Threshold   => Item.Threshold,
         Viewer      => Get_Indicator (Item.Indicator)
      );
   end Service;

   procedure Source_From_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             )  is
   begin
      if Is_Valid (Item.Source) then
         Item.Source_From := Value (Get_Text (Item.Source_From_Edit));
         if Item.Source_From in 1..Get_Examples_Number (Item.Source)
         then
            Set_Hint
            (  Item.Source_From_Hint,
               Item.Browser,
               Checked,
               True
            );
         else
            Set_Hint
            (  Item.Source_From_Hint,
               Item.Browser,
               Erroneous,
               True
            );
         end if;
      else
         Set_Hint
         (  Item.Source_From_Hint,
            Item.Browser,
            Erroneous,
            False
         );
      end if;
   exception
      when others =>
         Set_Hint
         (  Item.Source_From_Hint,
            Item.Browser,
            Erroneous,
            True
         );
   end Source_From_Changed;

   procedure Source_Name_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             )  is
      Name : constant Item_Path := Get_Path (Item.Source_Name);
   begin
      if Name'Length > 0 then
         Item.Source := Get_Lecture (Name, Item, Item.Source_Name_Hint);
         if Is_Valid (Item.Source) then
            Set_Sensitive (Item.Source_To_Edit,   True);
            Set_Sensitive (Item.Source_From_Edit, True);
            if Item.Lesson = Item.Source then  -- Same test and source
               Set_Lesson_Range (Item);
            else
               declare
                  Total : constant String :=
                             Strings_Edit.Integers.Image
                             (  Get_Examples_Number (Item.Source)
                             );
               begin
                  Set_Text (Item.Source_From_Edit, "1");
                  Set_Hint
                  (  Item.Source_From_Hint,
                     Item.Browser,
                     Checked,
                     True
                  );
                  Set_Text (Item.Source_To_Edit, Total);
                  Set_Hint
                  (  Item.Source_To_Hint,
                     Item.Browser,
                     Checked,
                     True
                  );
                  Set_Text (Item.Source_Total, Total);
               end;
            end if;
         else
            Set_Sensitive (Item.Source_To_Edit,   False);
            Set_Sensitive (Item.Source_From_Edit, False);
            Set_Text (Item.Source_From_Edit, "1");
            Set_Hint (Item.Source_From_Hint, Item.Browser, None, True);
            Set_Text (Item.Source_To_Edit, "");
            Set_Hint (Item.Source_To_Hint, Item.Browser, None, True);
            Set_Text (Item.Source_Total, "?");
         end if;
      else
         Invalidate (Item.Source);
         Set_Sensitive (Item.Source_To_Edit,   False);
         Set_Sensitive (Item.Source_From_Edit, False);
         Set_Text (Item.Source_From_Edit, "");
         Set_Hint (Item.Source_From_Hint, Item.Browser, None, False);
         Set_Text (Item.Source_To_Edit, "");
         Set_Hint (Item.Source_To_Hint, Item.Browser, None, False);
         Set_Text (Item.Source_Total, "");
         Set_Hint (Item.Source_Name_Hint, Item.Browser, None, False);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Source_Name_Changed")
         )  );
   end Source_Name_Changed;

   procedure Source_To_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
             )  is
   begin
      if Is_Valid (Item.Source) then
         Item.Source_To := Value (Get_Text (Item.Source_To_Edit));
         if Item.Source_To in 1..Get_Examples_Number (Item.Source) then
            Set_Hint (Item.Source_To_Hint, Item.Browser, Checked, True);
         else
            Set_Hint
            (  Item.Source_To_Hint,
               Item.Browser,
               Erroneous,
               True
            );
         end if;
      else
         Set_Hint (Item.Source_To_Hint, Item.Browser, Erroneous, False);
      end if;
   exception
      when others =>
         Set_Hint (Item.Source_To_Hint, Item.Browser, Erroneous, True);
   end Source_To_Changed;

   procedure Threshold_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Item   : Classification_Box
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
                Item   : Classification_Box
             )  is
   begin
      if Is_Valid (Item.Lesson) then
         Item.To := Value (Get_Text (Item.To_Edit));
         if Item.To in 1..Get_Examples_Number (Item.Lesson) then
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

end Gtk.Fuzzy_Catalogue.Classification_Boxes;
