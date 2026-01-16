--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Catalogue.                       Luebeck            --
--          Interactive_Classification_Boxes       Summer, 2010       --
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

with Confidence_Factors.Edit;     use Confidence_Factors.Edit;
with Fuzzy.Lecture;               use Fuzzy.Lecture;
with Fuzzy.Lecture.General;       use Fuzzy.Lecture.General;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties;             use GLib.Properties;
with GLib.Values;                 use GLib.Values;
with Gtk.Cell_Renderer_Fuzzy;     use Gtk.Cell_Renderer_Fuzzy;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Fuzzy_Feature;           use Gtk.Fuzzy_Feature;
with Gtk.Fuzzy_Set;               use Gtk.Fuzzy_Set;
with Gtk.Fuzzy_Set_Entry;         use Gtk.Fuzzy_Set_Entry;
with Gtk.Image;                   use Gtk.Image;
with Gtk.List_Store;              use Gtk.List_Store;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;

with Fuzzy.Abstract_Edit.Handle;
with Fuzzy.Feature.Classificatory;
with Fuzzy.Feature.Binary;
with Fuzzy.Feature.Handle.Container;
with GLib.Object.Checked_Destroy;
with GLib.Values.Fuzzy_Feature;
with GLib.Values.Fuzzy.Intuitionistic;

package body Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes is
   use Entry_Edit_Handles;
   use Fuzzy;
   use Fuzzy.Feature.Handle.Container;
   use Fuzzy.Feature.Classificatory;
   use Fuzzy.Feature.Binary;
   use Fuzzy.Intuitionistic;
   use Gtk.Missed;

   function Where (Name : String) return String is
   begin
      return
      (  " in Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes."
      &  Name
      );
   end Where;

   procedure Add_Icon
             (  Item : access
                       Interactive_Classification_Box_Record'Class
             )  is
      Icon      : Gtk_Image;
      Alignment : Gtk_Alignment;
   begin
      Erase (Item.Right);
      Gtk_New (Icon, Classify_Icon, Icon_Size_Panel);
      Gtk_New (Alignment, 0.5, 0.2, 0.5, 0.5);
      Add (Alignment, Icon);
      Pack_Start (Item.Right, Alignment, True, True);
--      Pack_Start (Item.Right, Icon, False, False);
      Show_All (Item.Right);
   end Add_Icon;

   procedure Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             )  is
   begin
      Add_Icon (Item);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Changed")
         )  );
   end Changed;

   procedure Classifier_Name_Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             )  is
      use Fuzzy.Feature.Handle.Container;
      use GLib.Values.Fuzzy_Feature;
      Classifier : Classifier_Handle;
   begin
      Classifier := Get_Classifier
                    (  Get_Path (Item.Classifier_Name),
                       Item,
                       Item.Classifier_Name_Hint
                    );
      if Is_Valid (Classifier) then
         Set_Model (Item.Input, Null_Gtk_Tree_Model);
         declare
            Store    : Gtk_List_Store;
            Iter     : Gtk_Tree_Iter := Null_Iter;
            Value    : GValue;
            Visited  : Fuzzy.Feature.Handle.Container.Set;
            To_Add   : Fuzzy.Feature.Handle.Container.Set;
            To_Check : Fuzzy.Feature.Handle.Container.Set :=
                          Get_Features (Classifier);
            Feature  : Feature_Handle;
         begin
            Gtk_New
            (  Store,
               (1 => GLib.Values.Fuzzy_Feature.GType_Feature)
            );
            Remove (To_Check, Get_Classes (Classifier));
            while not Is_Empty (To_Check) loop
               Feature := Ref (To_Check, 1);
               Remove (To_Check, 1);
               if not Is_In (Visited, Feature) then
                  Add (Visited, Feature);
                  if Is_Binary (Feature) then -- Binary feature
                     Add (To_Check, Get_Source (Feature));
                  elsif Is_Classificatory (Feature) then
                     Add -- Classification feature
                     (  To_Check,
                        Get_Features (Get_Classifier (Feature))
                     );
                  else -- Other feature
                     Add (To_Add, Feature);
                  end if;
               end if;
            end loop;
            for Index in 1..Get_Size (To_Add) loop
               Init (Value, GType_Feature);
               Set_Feature (Value, Ref (To_Add, Index));
               Append (Store, Iter);
               Set_Value (Store, Iter, 0, Value);
               Unset (Value);
            end loop;
            Set_Model (Item.Input, To_Interface (Store));
            Unref (Store);
         end;
      end if;
      Add_Icon (Item);
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
             (  Item : not null access
                       Interactive_Classification_Box_Record
             )  is
   begin
      Item.Classifier := Get_Classifier
                         (  Get_Path (Item.Classifier_Name),
                            Item,
                            Item.Classifier_Name_Hint
                         );
      if not Is_Valid (Item.Classifier) then
         return;
      end if;
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
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Commit")
         )  );
   end Commit;

   procedure Commit
             (  Cell : access GObject_Record'Class;
                Item : Interactive_Classification_Box
             )  is
   begin
      declare
         Value   : constant Classification := Get (Item.Value_Renderer);
         Feature : constant Feature_Handle :=
                            Get_feature (Item.Value_Renderer);
      begin
         Put (Item.Lesson, 1, Feature, Has_In, Value.Possibility);
         Put (Item.Lesson, 1, Feature, Has_Not, not Value.Necessity);
         Add_Icon (Item);
      end;
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

   procedure Completed
             (  Item : not null access
                       Interactive_Classification_Box_Record
             )  is
      use Fuzzy.Abstract_Edit;
      use Fuzzy.Abstract_Edit.Handle;
      Frame  : Gtk_Frame;
      Domain : Entry_Domain;
      Result : Gtk_Fuzzy_Set;
   begin
      Domain := Create (Get_Classes (Item.Classifier));
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Gtk_New
      (  Result,
         Ptr (Get_Domain (Domain.Ptr.all)).all,
         Item.Data.all
      );
      Erase (Item.Right);
      Add (Frame, Result);
      Pack_Start (Item.Right, Frame, False, False);
      Show_All (Frame);
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

   procedure Finalize
             (  Item : not null access
                       Interactive_Classification_Box_Record
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

   procedure Gtk_New
             (  Item    : out Interactive_Classification_Box;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class;
                Set_Classifier : Boolean
             )  is
   begin
      Item :=
         new Interactive_Classification_Box_Record
             (  Browser.all'Unchecked_Access
             );
      begin
         Initialize (Item, Set_Classifier);
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
             (  Item : not null access
                       Interactive_Classification_Box_Record'Class;
                Set_Classifier : Boolean
             )  is
      Label  : Gtk_Label;
      Scroll : Gtk_Scrolled_Window;
      Frame  : Gtk_Frame;
  --  Button_Spacing : constant GUInt :=
  --                      Style_Get (Item.Browser, "button-spacing");
      Column_Spacing : constant GUInt :=
                          Style_Get (Item.Browser, "column-spacing");
      Row_Spacing    : constant GUInt :=
                          Style_Get (Item.Browser, "row-spacing");
   begin
      Initialize (Item, "classification box", True);
      Gtk_New (Item.Classification_Grid, 6, 3, False);
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
         Style_Get (Item.Browser, "classify-threshold-label")
      );
      Label.Set_Halign (Align_End); Label.Set_Valign (Align_Center);
      Attach
      (  Item.Classification_Grid,
         Label,
         0, 1,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New (Item.Threshold_Entry);
      Item.Threshold_Entry.Set_Width_Chars (10);
      if (  Find_Property (Item.Threshold_Entry, "max-width-chars")
         /= null
         )
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
         1, 2,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Threshold_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.Threshold_Hint,
         2, 3,
         1, 2,
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
         1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Gtk_New (Item.Generalize_Combo, Nearest);
      Attach
      (  Item.Classification_Grid,
         Item.Generalize_Combo,
         7, 8,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Gtk_New_HBox (Item.Generalize_Hint);
      Attach
      (  Item.Classification_Grid,
         Item.Generalize_Hint,
         8, 9,
         1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Set_Hint (Item.Generalize_Hint, Item.Browser, Checked, True);
         -- Row 3
      Gtk_New_Hpaned (Item.Classification_Box);
      Attach
      (  Item.Classification_Grid,
         Item.Classification_Box,
         0, 9,
         2, 3,
         Xoptions => Fill or Expand,
         Yoptions => Fill or Expand
      );
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Out);
      Pack1 (Item.Classification_Box, Frame, Shrink => False);

      Gtk_New_VBox (Item.Left);
      Add (Frame, Item.Left);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Pack_Start (Item.Left, Frame, False, False);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add (Frame, Scroll);

      Gtk_New (Item.Input);
      declare
         Column         : Gtk_Tree_View_Column;
         Text_Renderer  : Gtk_Cell_Renderer_Text;
         Image_Renderer : Gtk_Cell_Renderer_Pixbuf;
         No             : GInt;
      begin
         Gtk_New (Column);
         Set_Title
         (  Column,
            Style_Get
            (  Item.Browser,
               "classify-input-feature-name-title"
         )  );
         Gtk_New (Image_Renderer);
         Set_Property
         (  Image_Renderer,
            Cell_Background_Property,
            "light grey"
         );
         Pack_Start (Column, Image_Renderer, False);
         Gtk_New (Text_Renderer);
         Set_Property
         (  Text_Renderer,
            Cell_Background_Property,
            "light grey"
         );
         Pack_Start (Column, Text_Renderer, True);
         No := Append_Column (Item.Input, Column);
         Set_Cell_Data_Func
         (  Column,
            Image_Renderer,
            Set_Icon'Access,
            Item.all'Unchecked_Access
         );
         Set_Cell_Data_Func
         (  Column,
            Text_Renderer,
            Set_Name'Access,
            Item.all'Unchecked_Access
         );

         Gtk_New (Column);
         Set_Title
         (  Column,
            Style_Get
            (  Item.Browser,
               "classify-input-feature-value-title"
         )  );
         Set_Resizable (Column, True);
         Gtk_New
         (  Item.Value_Renderer,
            Value_Type => GLib.Values.Fuzzy.Intuitionistic.
                          GType_Classification
         );
         Set_Mode (Item.Value_Renderer, Cell_Renderer_Mode_Editable);
         Set_Editable_Undefined (Item.Value_Renderer, True);
         Pack_Start (Column, Item.Value_Renderer, True);
         GLib.Properties.Set_Property
         (  Item.Value_Renderer,
            Gtk.Cell_Renderer.Xalign_Property,
            0.0
         );
         No := Append_Column (Item.Input, Column);
         Set_Cell_Data_Func
         (  Column,
            Item.Value_Renderer,
            Set_Value'Access,
            Item.all'Unchecked_Access
         );
         Classification_Handlers.Connect
         (  Item.Value_Renderer,
            "commit",
            Commit'Access,
            Item.all'Unchecked_Access
         );
      end;
      Add (Scroll, Item.Input);

      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_Out);
      Pack2 (Item.Classification_Box, Frame, Shrink => False);

      Gtk_New_VBox (Item.Right);
      Add (Frame, Item.Right);
      Add_Icon (Item);

      Pack_Start (Item, Item.Classification_Grid, True, True);
      Show_All (Item);

      Add_Item
      (  Item.Browser,
         Style_Get (Item.Browser, "tab-classify-input-label"),
         Style_Get (Item.Browser, "tab-classify-input-icon"),
         Item
      );

      Classification_Handlers.Connect
      (  Get_Entry (Item.Classifier_Name),
         "changed",
         Classifier_Name_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Item.Threshold_Entry,
         "changed",
         Threshold_Changed'Access,
         Item.all'Access
      );
      Classification_Handlers.Connect
      (  Item.Generalize_Combo,
         "changed",
         Changed'Access,
         Item.all'Access
      );
      Item.Lesson := Create;
      if Set_Classifier then
         Classifier_Name_Changed (Item, Item.all'Access);
      end if;
      declare
         Size : Gtk_Requisition;
      begin
         Columns_Autosize (Item.Input);   -- Size columns
         Size_Request (Item.Input, Size); -- Query the integral size
         Set_Size_Request                 -- Set new size
         (  Item.Input,
            GInt'Min (Size.Width,  800),
            GInt'Min (Size.Height, 400)
         );
      end;
   end Initialize;

   procedure Service
             (  Item : not null access
                       Interactive_Classification_Box_Record
             )  is
   begin
      Free (Item.Data);
      Item.Data :=
         new Classification'
             (  Classify
                (  Item.Classifier,
                   Item.Lesson,
                   1,
                   Item.Generalize,
                   Item.Threshold
             )  );
   end Service;

   procedure Set_Icon
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             )  is
      use GLib.Values.Fuzzy_Feature;
      Feature : GValue;
   begin
      Get_Value (Model, Iter, 0, Feature);
      Set_Property
      (  Cell,
         Stock_Id_Property,
         Get_Feature_Icon (Get_Class (Get_Feature (Feature)))
      );
      Unset (Feature);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Set_Icon")
         )  );
   end Set_Icon;

   procedure Set_Name
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             )  is
      use GLib.Values.Fuzzy_Feature;
      Feature : GValue;
   begin
      Get_Value (Model, Iter, 0, Feature);
      Set_Property
      (  Cell,
         Gtk.Cell_Renderer_Text.Text_Property,
         Get_Name (Get_Feature (Feature))
      );
      Unset (Feature);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Set_Name")
         )  );
   end Set_Name;

   procedure Set_Value
             (  Column : not null access
                         Gtk_Tree_View_Column_Record'Class;
                Cell   : not null access Gtk_Cell_Renderer_Record'Class;
                Model  : Gtk_Tree_Model;
                Iter   : Gtk_Tree_Iter;
                Data   : Interactive_Classification_Box
             )  is
      use GLib.Values.Fuzzy_Feature;
      Value   : GValue;
      Feature : Feature_Handle;
   begin
      Get_Value (Model, Iter, 0, Value);
      Feature := Get_Feature (Value);
      Unset (Value);
      declare
          Renderer : constant Gtk_Cell_Renderer_Fuzzy_Feature_Value :=
                     Gtk_Cell_Renderer_Fuzzy_Feature_Value_Record'Class
                     (  Cell.all
                     ) 'Unchecked_Access;
      begin
         Set_Feature (Renderer, Feature);
         if (  Is_Defined (Data.Lesson, 1, Feature, Has_In)
            or else
               Is_Defined (Data.Lesson, 1, Feature, Has_Not)
            )
         then
            Put
            (  Renderer,
               Classification'
               (  Cardinality =>
                     Get_Cardinality (Feature),
                  Possibility =>
                     Get (Data.Lesson, 1, Feature, Has_In),
                  Necessity =>
                     not Get (Data.Lesson, 1, Feature, Has_Not)
            )  );
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Set_Value")
         )  );
   end Set_Value;

   procedure Threshold_Changed
             (  Object : access GObject_Record'Class;
                Item   : Interactive_Classification_Box
             )  is
      Threshold : Confidence;
   begin
      Threshold := Value (Get_Text (Item.Threshold_Entry));
      Set_Hint (Item.Threshold_Hint, Item.Browser, Checked, True);
      Add_Icon (Item);
   exception
      when others =>
         Set_Hint (Item.Threshold_Hint, Item.Browser, Erroneous, True);
   end Threshold_Changed;

end Gtk.Fuzzy_Catalogue.Interactive_Classification_Boxes;
