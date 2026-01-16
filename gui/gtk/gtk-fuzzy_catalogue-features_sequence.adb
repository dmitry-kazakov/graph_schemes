--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Features_Sequence                        Winter, 2009       --
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

with Deposit_Handles;            use Deposit_Handles;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with GLib.Messages;              use GLib.Messages;
with GLib.Values.Fuzzy_Feature;  use GLib.Values.Fuzzy_Feature;
with GLib.Values;                use GLib.Values;
with Gtk.Alignment;              use Gtk.Alignment;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Missed;                 use Gtk.Missed;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;          use Gtk.Widget.Styles;

with Gtk.Fuzzy_Catalogue.Feature_Pane;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Catalogue.Features_Sequence is
   use Backward_Link_Handles.Sets;
   use Down_Buttons;
   use Fuzzy.Feature;
   use Gtk.Fuzzy_Catalogue.Feature_Pane;
   use Left_Buttons;
   use Right_Buttons;
   use Up_Buttons;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Catalogue.Features_Sequence." & Name;
   end Where;

   procedure Deleted
             (  Link  : in out Feature_Observer;
                Temps : in out Deposit_Container'Class
             )  is
      Data : Deleted_Data (Link'Access);
   begin
      Request (Data);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Deleted")
         )  );
   end Deleted;

   --  procedure Deselected_Changed
   --            (  Selection : not null access
   --                           Gtk_Tree_Selection_Record'Class;
   --               List      : Gtk_Fuzzy_Features_Sequence
   --            )  is
   --     Enable : Boolean;
   --  begin
   --     Enable :=
   --        (  Check
   --           (  List.Constraint,
   --              List.Browser.Features.List.Get_Storage
   --           )
   --        and then
   --           Selection.Count_Selected_Rows > 0
   --        );
   --     List.Right_Button.Set_Sensitive (Enable);
   --  exception
   --     when Error : others =>
   --        Log
   --        (  Fuzzy_ML_Domain,
   --           Log_Level_Critical,
   --           (  "Fault: "
   --           &  Exception_Information (Error)
   --           &  Where ("Deselected_Changed")
   --        )  );
   --  end Deselected_Changed;
   --
   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
   begin
      Free (List.Constraint);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy")
         )  );
   end Destroy;

   procedure Destroyed (Link : in out Feature_Observer) is
      Data : Deleted_Data (Link'Access);
   begin
      Request (Data);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroyed")
         )  );
   end Destroyed;

   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (List.Selected.Get_Selection, Row);
      end loop;
      Ref (List.Selected_List);
      Set_Model (List.Selected, Null_Gtk_Tree_Model);
      for Index in reverse 0..Selection'Last - 1 loop
         if Selection (Index) then
            Swap
            (  List.Selected_List,
               Nth_Child (List.Selected_List, Null_Iter, Index),
               Nth_Child (List.Selected_List, Null_Iter, Index + 1)
            );
         end if;
      end loop;
      Set_Model (List.Selected, To_Interface (List.Selected_List));
      Unref (List.Selected_List);
      Row := Null_Iter;
      for Index in 0..Selection'Last - 1 loop
         if Selection (Index) then
            Row := Nth_Child (List.Selected_List, Null_Iter, Index + 1);
            Select_Iter (List.Selected.Get_Selection, Row);
         end if;
      end loop;
      if Row /= Null_Iter then
         Path := Get_Path (List.Selected_List, Row);
         List.Selected.Scroll_To_Cell (Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
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

   procedure Features_Selection_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
   begin
      declare
         View : constant Feature_Panel :=
                         Feature_Panel
                         (  List.Browser.Features
                         ) .all'Unchecked_Access;
         Selected : constant Selection := View.List.Get_Selection;
         Enable   : Boolean;
      begin
         Enable :=
            (  Selected'Length > 0
            and then
               Check (List.Constraint, View.List.Get_Storage)
            );
         List.Right_Button.Set_Sensitive (Enable);
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Features_Selection_Changed")
         )  );
   end Features_Selection_Changed;

   function Get_Button_Box
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Gtk_VBox is
   begin
      return Widget.Buttons_Box;
   end Get_Button_Box;

   function Get_Constraint
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Picker_Constraint is
   begin
      return Widget.Constraint;
   end Get_Constraint;

   function Get_Selected
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Bounded_Array is
      Result : Bounded_Array
               (  1,
                  Natural (N_Children (Widget.Selected_List))
               );
      Row   : Gtk_Tree_Iter := Get_Iter_First (Widget.Selected_List);
      Value : GValue;
   begin
      for Index in Result.First..Result.Last loop
         Get_Value (Widget.Selected_List, Row, 1, Value);
         Put (Result, Index, Get_Feature (Value));
         Unset (Value);
         Next (Widget.Selected_List, Row);
      end loop;
      return Result;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Get_Selected")
         )  );
         return To_Bounded_Array ((1..0 => No_Feature));
   end Get_Selected;

   function Get_Selected_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Selected;
   end Get_Selected_Tree_View;

   function Get_Type return GType is
   begin
      Initialize_Class_Record
      (  Ancestor     => Gtk.Box.Get_HBox_Type,
         Class_Record => Class_Record,
         Type_Name    => Class_Name & "FeaturesSequence"
      );
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Features_Sequence;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             )  is
   begin
      Widget :=
         new Gtk_Fuzzy_Features_Sequence_Record
             (  Browser.all'Unchecked_Access
             );
      begin
         Gtk.Fuzzy_Catalogue.Features_Sequence.Initialize (Widget);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Features_Sequence_Record
             )  is
      Button_Spacing : constant GUInt :=
                          Style_Get (Widget.Browser, "button-spacing");
      Scroll     : Gtk_Scrolled_Window;
      Alignment  : Gtk_Alignment;
      Frame      : Gtk_Frame;
   begin
      G_New (Widget, Get_Type);
      Initialize_HBox (Widget);

      Gtk_New (Alignment, 0.5, 0.5, 0.0, 0.0);
      Gtk_New_VBox (Widget.Buttons_Box);
      Widget.Buttons_Box.Set_Spacing (GInt (Button_Spacing));
      Alignment.Add (Widget.Buttons_Box);

      Gtk_New (Widget.Left_Button);
      Widget.Buttons_Box.Pack_Start
      (  Widget.Left_Button,
         False,
         False
      );
      Widget.Left_Button.Set_Sensitive (False);

      Gtk_New (Widget.Right_Button);
      Widget.Buttons_Box.Pack_Start
      (  Widget.Right_Button,
         False,
         False
      );
      Widget.Right_Button.Set_Sensitive (False);

      Gtk_New (Widget.Up_Button);
      Widget.Buttons_Box.Pack_Start
      (  Widget.Up_Button,
         False,
         False
      );
      Widget.Up_Button.Set_Sensitive (False);

      Gtk_New (Widget.Down_Button);
      Pack_Start
      (  Widget.Buttons_Box,
         Widget.Down_Button,
         False,
         False
      );
      Widget.Down_Button.Set_Sensitive (False);

      Widget.Pack_Start (Alignment, False, False);
      Show_All (Alignment);

      Gtk_New (Widget.Selected);
      Widget.Selected.Get_Selection.Set_Mode (Selection_Multiple);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Column_No : GInt;
      begin
         Gtk_New (Column);
         Gtk_New (Icon);
         Pack_Start (Column, Icon, False);
         Add_Stock_Attribute (Column, Icon, 2);
         Gtk_New (Text);
         Pack_Start (Column, Text, True);
         Add_Attribute (Column, Text, "text", 0);
         Column_No := Append_Column (Widget.Selected, Column);
         Set_Resizable (Column, True);
         Set_Title
         (  Column,
            Style_Get (Widget.Browser, "features-sequence-title")
         );
      end;
      Gtk_New
      (  Widget.Selected_List,
         (GType_String, GType_Feature, GType_String)
      );
      Set_Model (Widget.Selected, To_Interface (Widget.Selected_List));
      Unref (Widget.Selected_List);
      Gtk_New (Scroll);
      Add (Scroll, Widget.Selected);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Scroll);
      Widget.Pack_Start (Frame);
      Show_All (Frame);

      Widget.Constraint := Create ("features sequence");
      Widget_Handlers.Connect
      (  Widget,
         "destroy",
         Destroy'Access,
         Widget.all'Access
      );
      Widget_Handlers.Connect
      (  Widget.Down_Button,
         "clicked",
         Down'Access,
         Widget.all'Access
      );
      Widget_Handlers.Connect
      (  Widget.Left_Button,
         "clicked",
         Left'Access,
         Widget.all'Access
      );
      Widget_Handlers.Connect
      (  Widget.Right_Button,
         "clicked",
         Right'Access,
         Widget.all'Access
      );
      Widget_Handlers.Connect
      (  Widget.Up_Button,
         "clicked",
         Up'Access,
         Widget.all'Access
      );
      Selection_Handlers.Connect
      (  Widget.Selected.Get_Selection,
         "changed",
         Selected_Changed'Access,
         Widget.all'Access
      );
      if Widget.Browser.Features /= null then
         declare
            View : constant Feature_Panel :=
                            Feature_Panel
                            (  Widget.Browser.Features
                            ) .all'Unchecked_Access;
         begin
            Widget_Handlers.Connect
            (  View.List,
               "selection-changed",
               Features_Selection_Changed'Access,
               Widget.all'Access
            );
            Features_Selection_Changed
            (  Widget.all'Access,
               Widget.all'Access
            );
         end;
      end if;
   end Initialize;

   procedure Left
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
      Row   : Gtk_Tree_Iter;
      Value : GValue;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (List.Selected.Get_Selection, Row);
      end loop;
      Ref (List.Selected_List);
      Set_Model (List.Selected, Null_Gtk_Tree_Model);
      for Index in reverse Selection'Range loop
         if Selection (Index) then
            Row := Nth_Child (List.Selected_List, Null_Iter, Index);
            Get_Value (List.Selected_List, Row, 1, Value);
            declare -- Removing observer from the set of
               Feature : constant Deposit_Ptr :=
                         To_Deposit_Ptr (Ptr (Get_Feature (Value)));
            begin
               for Item in 1..Get_Size (List.Observers) loop
                  if This (Get (List.Observers, Item).all) = Feature then
                     Remove (List.Observers, Item);
                     exit;
                  end if;
               end loop;
            end;
            Unset (Value);
            Remove (List.Selected_List, Row);
         end if;
      end loop;
      Set_Model (List.Selected, To_Interface (List.Selected_List));
      Unref (List.Selected_List);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Left")
         )  );
   end Left;

   procedure Renamed
             (  Link     : in out Feature_Observer;
                Old_Name : String;
                New_Name : String
             )  is
      Data : Renamed_Data (Link'Access, New_Name'Length);
   begin
      Data.Name := New_Name;
      Request (Data);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Renamed")
         )  );
   end Renamed;

   procedure Right
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
      View : constant Feature_Panel :=
             Feature_Panel (List.Browser.Features).all'Unchecked_Access;
      Selected : constant Selection := View.List.Get_Selection;
   begin
      Unselect_All (List.Selected.Get_Selection);
      for Index in Selected'Range loop
         declare
            Row     : Gtk_Tree_Iter;
            Value   : GValue;
            Feature : Feature_Handle;
            Storage : Storage_Handle;
            Object  : Deposit_Handle;
            Link    : Backward_Link_Ptr;
         begin
            Browse
            (  Get_Directory_Cache (View.List),
               Get_Path (View.List, Selected (Index)),
               Storage,
               Object
            );
            Feature := Ref (To_Feature_Object_Ptr (Ptr (Object)));
            Append (List.Selected_List, Row);
            Gtk.Missed.Set
            (  List.Selected_List,
               Row,
               0,
               Get_Name (Feature)
            );
            Init (Value, GType_Feature);
            Set_Feature (Value, Feature);
            Set_Value (List.Selected_List, Row, 1, Value);
            Gtk.Missed.Set
            (  List.Selected_List,
               Row,
               2,
               Get_Feature_Icon (Get_Class (Feature))
            );
            Unset (Value);
            Select_Iter (List.Selected.Get_Selection, Row);
               -- Adding new observer to the set
            Link := new Feature_Observer (List);
            declare
               Constraint : Picker_Constraint renames
                            Feature_Observer (Link.all).Constraint;
            begin
               Set (Constraint, Storage);
               Combine (List.Constraint, Constraint);
            end;
            Add (List.Observers, Link);
            Attach (Link, To_Deposit_Ptr (Ptr (Feature)));
         exception
            when others =>
               null;
         end;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Right")
         )  );
   end Right;

   procedure Selected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_Sequence
             )  is
   begin
      if Selection.Count_Selected_Rows > 0 then
         List.Left_Button.Set_Sensitive (True);
         List.Down_Button.Set_Sensitive
         (  not Selection.Iter_Is_Selected
                (  Nth_Child
                   (  List.Selected_List,
                      Null_Iter,
                      N_Children (List.Selected_List) - 1
         )      )  );
         List.Up_Button.Set_Sensitive
         (  not Selection.Iter_Is_Selected
                (  Get_Iter_First (List.Selected_List)
         )      );
      else
         List.Left_Button.Set_Sensitive (False);
         List.Down_Button.Set_Sensitive (False);
         List.Up_Button.Set_Sensitive (False);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selected_Changed")
         )  );
   end Selected_Changed;

   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             )  is
      Path      : Gtk_Tree_Path;
      Row       : Gtk_Tree_Iter;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (List.Selected.Get_Selection, Row);
      end loop;
      Ref (List.Selected_List);
      Set_Model (List.Selected, Null_Gtk_Tree_Model);
      for Index in 1..Selection'Last loop
         if Selection (Index) then
            Swap
            (  List.Selected_List,
               Nth_Child (List.Selected_List, Null_Iter, Index - 1),
               Nth_Child (List.Selected_List, Null_Iter, Index)
            );
         end if;
      end loop;
      Set_Model (List.Selected, To_Interface (List.Selected_List));
      Unref (List.Selected_List);
      Row := Null_Iter;
      for Index in reverse 1..Selection'Last loop
         if Selection (Index) then
            Row := Nth_Child (List.Selected_List, Null_Iter, Index - 1);
            Select_Iter (List.Selected.Get_Selection, Row);
         end if;
      end loop;
      if Row /= Null_Iter then
         Path := Get_Path (List.Selected_List, Row);
         List.Selected.Scroll_To_Cell (Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Up")
         )  );
   end Up;

   procedure Service (Data : in out Deleted_Data) is
      List  : constant Gtk_List_Store :=
                       Data.Observer.Sequence.Selected_List;
      Value : GValue;
      Row   : Gtk_Tree_Iter;
   begin
      for Index in 0..N_Children (List) - 1 loop
         Row := Nth_Child (List, Null_Iter, Index);
         Get_Value (List, Row, 1, Value);
         if (  To_Deposit_Ptr (Ptr (Get_Feature (Value)))
            =  This (Data.Observer.all)
            )
         then
            Unset (Value);
            Row := Nth_Child (List, Null_Iter, Index);
            Remove (List, Row);
            exit;
         else
            Unset (Value);
         end if;
      end loop;
      Remove
      (  Data.Observer.Sequence.Observers,
         Self (Data.Observer.all)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service Deleted")
         )  );
   end Service;

   procedure Service (Data : in out Renamed_Data) is
      List  : constant Gtk_List_Store :=
                       Data.Observer.Sequence.Selected_List;
      Value : GValue;
      Row   : Gtk_Tree_Iter;
   begin
      for Index in 0..N_Children (List) - 1 loop
         Row := Nth_Child (List, Null_Iter, Index);
         Get_Value (List, Row, 1, Value);
         if (  To_Deposit_Ptr (Ptr (Get_Feature (Value)))
            =  This (Data.Observer.all)
            )
         then
            Unset (Value);
            Gtk.Missed.Set
            (  List,
               Row,
               0,
               Get_Name
               (  Feature_Object'Class
                  (  This (Data.Observer.all).all
            )  )  );
            exit;
         else
            Unset (Value);
         end if;
      end loop;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service Renamed")
         )  );
   end Service;

end Gtk.Fuzzy_Catalogue.Features_Sequence;
