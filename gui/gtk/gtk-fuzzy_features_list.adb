--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Features_List                     Luebeck            --
--  Implementation                                 Autumn, 2008       --
--                                                                    --
--                                Last revision :  14:14 01 May 2021  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Gtk_Icon_Factory;     use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;              use GLib.Messages;
with GLib.Values.Fuzzy_Feature;  use GLib.Values.Fuzzy_Feature;
with GLib.Values;                use GLib.Values;
with Gtk.Alignment;              use Gtk.Alignment;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Frame;                  use Gtk.Frame;
with Gtk.Missed;                 use Gtk.Missed;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;

with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Features_List is
   use Backward_Link_Handles.Sets;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Features_List." & Name;
   end Where;

   function "/"
            (  Store : not null access Gtk_List_Store_Record'Class;
               Row   : Gtk_Tree_Iter
            )  return String is
      Value : GValue;
   begin
      Store.Get_Value (Row, 0, Value);
      return Result : constant String := Get_String (Value) do
         Unset (Value);
      end return;
   end "/";

   procedure Check_Features
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record;
                List   : Fuzzy.Feature.Handle.Container.Set
             )  is
      Selected   : constant Fuzzy.Feature.Handle.Container.Set :=
                            Get_Selected (Widget);
      Selectable : Boolean := Get_Size (List) > 0;
   begin
      for Feature in 1..Get_Size (List) loop
         Selectable := False;
         for Source in 1..Get_Size (Widget.Features_List) loop
            Selectable :=
               (  not Is_In (Selected, Ref (List, Feature))
               and then
                  Is_Computed
                  (  Feature => Ref (List, Feature),
                     Source  => Ref (Widget.Features_List, Source)
               )  );
            exit when Selectable;
         end loop;
         exit when not Selectable;
      end loop;
      if Widget.Take_Button /= null then
         Set_Sensitive (Widget.Take_Button, Selectable);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Check_Features")
         )  );
   end Check_Features;

   procedure Deleted
             (  Link  : in out Feature_Observer;
                Temps : in out Deposit_Container'Class
             )  is
      Data : Deleted_Data (Link'Access);
   begin
      Request (Data);
   end Deleted;

   procedure Deselected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_List
             )  is
   begin
      Set_Sensitive
      (  List.Right_Button,
         Count_Selected_Rows (Selection) > 0
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Deselected_Changed")
         )  );
   end Deselected_Changed;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             )  is
   begin
      Erase (List.Observers);
      if List.Release_Button /= null then
         Unref (List.Release_Button);
      end if;
      if List.Take_Button /= null then
         Unref (List.Take_Button);
      end if;
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
   end Destroyed;

   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             )  is
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (Get_Selection (List.Selected), Row);
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
            Select_Iter (Get_Selection (List.Selected), Row);
         end if;
      end loop;
      if Row /= Null_Iter then
         Path := Get_Path (List.Selected_List, Row);
         Scroll_To_Cell (List.Selected, Path, null, False, 0.0, 0.0);
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

   function Get_Button_Box
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_VBox is
   begin
      return Widget.Buttons_Box;
   end Get_Button_Box;

   procedure Get_External_Buttons
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record;
                Up     : out Gtk_Button;
                Down   : out Gtk_Button
             )  is
   begin
      if Widget.Release_Button = null then
         Gtk_New (Widget.Release_Button);
         Gtk.Handlers.References.Set
         (  Widget.Release_Signal,
            Widget_Handlers.Connect
            (  Widget.Release_Button,
               "clicked",
               Widget_Handlers.To_Marshaller (Left'Access),
               Widget.all'Access
         )  );
         Ref (Widget.Release_Button);
      end if;
      if Widget.Take_Button = null then
         Gtk_New (Widget.Take_Button);
         Ref (Widget.Take_Button);
      end if;
      Up   := Widget.Release_Button.all'Unchecked_Access;
      Down := Widget.Take_Button.all'Unchecked_Access;
      Set_Sensitive (Up,   False);
      Set_Sensitive (Down, False);
      Selected_Changed
      (  Get_Selection (Widget.Selected),
         Widget.all'Unchecked_Access
      );
   end Get_External_Buttons;

   function Get_Deselected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Bounded_Array is
      Result : Bounded_Array
               (  1,
                  Natural (N_Children (Widget.Deselected_List))
               );
      Row   : Gtk_Tree_Iter := Get_Iter_First (Widget.Deselected_List);
      Value : GValue;
   begin
      for Index in Result.First..Result.Last loop
         Get_Value (Widget.Deselected_List, Row, 1, Value);
         Put (Result, Index, Get_Feature (Value));
         Unset (Value);
         Next (Widget.Deselected_List, Row);
      end loop;
      return Result;
   end Get_Deselected;

   function Get_Type return GType is
   begin
      Initialize_Class_Record
      (  Ancestor     => Gtk.Box.Get_HBox_Type,
         Class_Record => Class_Record,
         Type_Name    => Class_Name
      );
      return Class_Record.The_Type;
   end Get_Type;

   function Get_Deselected_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Deselected;
   end Get_Deselected_Tree_View;

   function Get_Selected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
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
   end Get_Selected;

   function Get_Selected
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Fuzzy.Feature.Handle.Container.Set is
      Result : Fuzzy.Feature.Handle.Container.Set;
      Row    : Gtk_Tree_Iter := Get_Iter_First (Widget.Selected_List);
      Value  : GValue;
   begin
      while Row /= Null_Iter loop
         Get_Value (Widget.Selected_List, Row, 1, Value);
         Add (Result, Get_Feature (Value));
         Unset (Value);
         Next (Widget.Selected_List, Row);
      end loop;
      return Result;
   end Get_Selected;

   function Get_Selected_Tree_View
            (  Widget : not null access Gtk_Fuzzy_Features_List_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Selected;
   end Get_Selected_Tree_View;

   procedure Gtk_New (Widget : out Gtk_Fuzzy_Features_List) is
   begin
      Widget := new Gtk_Fuzzy_Features_List_Record;
      begin
         Gtk.Fuzzy_Features_List.Initialize (Widget);
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
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record
             )  is
      Scroll    : Gtk_Scrolled_Window;
      Alignment : Gtk_Alignment;
      Frame     : Gtk_Frame;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize_HBox (Widget);

      Gtk_New (Widget.Deselected);
      Set_Mode (Get_Selection (Widget.Deselected), Selection_Multiple);
      declare
         Column    : Gtk_Tree_View_Column;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Text      : Gtk_Cell_Renderer_Text;
         Column_No : Gint;
      begin
         Gtk_New (Column);
         Gtk_New (Icon);
         Pack_Start (Column, Icon, False);
         Add_Stock_Attribute (Column, Icon, 2);
         Gtk_New (Text);
         Pack_Start (Column, Text, True);
         Add_Attribute (Column, Text, "text", 0);
         Column_No := Append_Column (Widget.Deselected, Column);
         Set_Resizable (Column, True);
      end;
      Gtk_New
      (  Widget.Deselected_List,
         (GType_String, GType_Feature, GType_String)
      );
      Set_Model
      (  Widget.Deselected,
         To_Interface (Widget.Deselected_List)
      );
      Unref (Widget.Deselected_List);
      Gtk_New (Scroll);
      Add (Scroll, Widget.Deselected);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Scroll);
      Pack_Start (Widget, Frame);
      Show_All (Frame);

      Gtk_New (Alignment, 0.5, 0.5, 0.0, 0.0);
      Gtk_New_VBox (Widget.Buttons_Box);
      Add (Alignment, Widget.Buttons_Box);

      Gtk_New (Widget.Left_Button);
      Pack_Start
      (  Widget.Buttons_Box,
         Widget.Left_Button,
         False,
         False
      );
      Set_Sensitive (Widget.Left_Button, False);

      Gtk_New (Widget.Right_Button);
      Pack_Start
      (  Widget.Buttons_Box,
         Widget.Right_Button,
         False,
         False
      );
      Set_Sensitive (Widget.Right_Button, False);

      Gtk_New (Widget.Up_Button);
      Pack_Start
      (  Widget.Buttons_Box,
         Widget.Up_Button,
         False,
         False
      );
      Set_Sensitive (Widget.Up_Button, False);

      Gtk_New (Widget.Down_Button);
      Pack_Start
      (  Widget.Buttons_Box,
         Widget.Down_Button,
         False,
         False
      );
      Set_Sensitive (Widget.Down_Button, False);

      Pack_Start (Widget, Alignment, False, False);
      Show_All (Alignment);

      Gtk_New (Widget.Selected);
      Set_Mode (Get_Selection (Widget.Selected), Selection_Multiple);
      declare
         Column    : Gtk_Tree_View_Column;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Text      : Gtk_Cell_Renderer_Text;
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
      end;
      Gtk_New
      (  Widget.Selected_List,
         (GType_String, GType_Feature, GType_String)
      );
      Set_Model
      (  Widget.Selected,
         To_Interface (Widget.Selected_List)
      );
      Unref (Widget.Selected_List);
      Gtk_New (Scroll);
      Add (Scroll, Widget.Selected);
      Gtk_New (Frame);
      Set_Shadow_Type (Frame, Shadow_In);
      Add (Frame, Scroll);
      Pack_Start (Widget, Frame);
      Show_All (Frame);

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
      (  Get_Selection (Widget.Selected),
         "changed",
         Selected_Changed'Access,
         Widget.all'Access
      );
      Selection_Handlers.Connect
      (  Get_Selection (Widget.Deselected),
         "changed",
         Deselected_Changed'Access,
         Widget.all'Access
      );
      Widget_Handlers.Connect
      (  Widget,
         "destroy",
         Destroy'Access,
         Widget.all'Access
      );
   end Initialize;

   procedure Left
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             )  is
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      Unselect_All (Get_Selection (List.Deselected));
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (Get_Selection (List.Selected), Row);
         if Selection (Index) then
            declare
               Name  : constant String := List.Selected_List / Row;
               Value : GValue;
            begin
               Get_Value (List.Selected_List, Row, 1, Value);
               if Is_In (List.Features_List, Get_Feature (Value)) then
                  Append (List.Deselected_List, Row);
                  Gtk.Missed.Set (List.Deselected_List, Row, 0, Name);
                  Set_Value (List.Deselected_List, Row, 1, Value);
                  Gtk.Missed.Set
                  (  List.Deselected_List,
                     Row,
                     2,
                     Get_Feature_Icon (Get_Class (Get_Feature (Value)))
                  );
                  Select_Iter (Get_Selection (List.Deselected), Row);
                  Path := Get_Path (List.Deselected_List, Row);
               end if;
               Unset (Value);
            end;
         end if;
      end loop;
      Ref (List.Selected_List);
      Set_Model (List.Selected, Null_Gtk_Tree_Model);
      for Index in reverse Selection'Range loop
         if Selection (Index) then
            Row := Nth_Child (List.Selected_List, Null_Iter, Index);
            Remove (List.Selected_List, Row);
         end if;
      end loop;
      Set_Model (List.Selected, To_Interface (List.Selected_List));
      Unref (List.Selected_List);
      if Path /= Null_Gtk_Tree_Path then
         Scroll_To_Cell (List.Deselected, Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Left")
         )  );
   end Left;

   procedure Put
             (  Widget   : not null access
                           Gtk_Fuzzy_Features_List_Record;
                Selected : Fuzzy.Feature.Handle.Container.Set
             )  is
      List : Bounded_Array (1, Get_Size (Selected));
   begin
      for Index in List.First..List.Last loop
         Put (List, Index, Get (Selected, Index));
      end loop;
      Put (Widget, List);
   end Put;

   procedure Put
             (  Widget     : not null access
                             Gtk_Fuzzy_Features_List_Record;
                Selected   : Bounded_Array;
                Deselected : Bounded_Array := Empty
             )  is
      Value   : GValue;
      Feature : Feature_Handle;
      Row     : Gtk_Tree_Iter;
      Link    : Backward_Link_Ptr;
   begin
      Erase (Widget.Observers);
      Erase (Widget.Features_List);
      Init (Value, GType_Feature);

      Ref (Widget.Selected_List);
      Set_Model (Widget.Selected, Null_Gtk_Tree_Model);
      Clear (Widget.Selected_List);
      for Index in Selected.First..Selected.Last loop
         Append (Widget.Selected_List, Row);
         Add (Widget.Features_List, Get (Selected, Index));
         Feature := Ref (Get (Selected, Index));
         Gtk.Missed.Set
         (  Widget.Selected_List,
            Row,
            0,
            Get_Name (Feature)
         );
         Set_Feature (Value, Feature);
         Set_Value (Widget.Selected_List, Row, 1, Value);
         Gtk.Missed.Set
         (  Widget.Selected_List,
            Row,
            2,
            Get_Feature_Icon (Get_Class (Feature))
         );
            -- Adding new observer to the set
         Link := new Feature_Observer (Widget.all'Unchecked_Access);
         Add (Widget.Observers, Link);
         Attach (Link, To_Deposit_Ptr (Ptr (Feature)));
      end loop;
      Set_Model
      (  Widget.Selected,
         To_Interface (Widget.Selected_List)
      );
      Unref (Widget.Selected_List);

      Ref (Widget.Deselected_List);
      Set_Model (Widget.Deselected, Null_Gtk_Tree_Model);
      Clear (Widget.Deselected_List);
      for Index in Deselected.First..Deselected.Last loop
         Append (Widget.Deselected_List, Row);
         Add (Widget.Features_List, Get (Selected, Index));
         Feature := Ref (Get (Deselected, Index));
         Gtk.Missed.Set
         (  Widget.Deselected_List,
            Row,
            0,
            Get_Name (Feature)
         );
         Set_Feature (Value, Feature);
         Set_Value (Widget.Deselected_List, Row, 1, Value);
         Gtk.Missed.Set
         (  Widget.Deselected_List,
            Row,
            2,
            Get_Feature_Icon (Get_Class (Feature))
         );
            -- Adding new observer to the set
         Link := new Feature_Observer (Widget.all'Unchecked_Access);
         Add (Widget.Observers, Link);
         Attach (Link, To_Deposit_Ptr (Ptr (Feature)));
      end loop;
      Set_Model
      (  Widget.Deselected,
         To_Interface (Widget.Deselected_List)
      );
      Unref (Widget.Deselected_List);

      Unset (Value);
   end Put;

   procedure Renamed
             (  Link     : in out Feature_Observer;
                Old_Name : String;
                New_Name : String
             )  is
      Data : Renamed_Data (Link'Access, New_Name'Length);
   begin
      Data.Name := New_Name;
      Request (Data);
   end Renamed;

   procedure Right
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             )  is
      Row  : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Selection : array (0..N_Children (List.Deselected_List) - 1)
                     of Boolean;
   begin
      Unselect_All (Get_Selection (List.Selected));
      for Index in Selection'Range loop
         Row := Nth_Child (List.Deselected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (Get_Selection (List.Deselected), Row);
         if Selection (Index) then
            declare
               Name  : constant String := List.Deselected_List / Row;
               Value : GValue;
            begin
               Get_Value (List.Deselected_List, Row, 1, Value);
               Append (List.Selected_List, Row);
               Gtk.Missed.Set (List.Selected_List, Row, 0, Name);
               Set_Value (List.Selected_List, Row, 1, Value);
               Gtk.Missed.Set
               (  List.Selected_List,
                  Row,
                  2,
                  Get_Feature_Icon (Get_Class (Get_Feature (Value)))
               );
               Unset (Value);
               Select_Iter (Get_Selection (List.Selected), Row);
               if Path = Null_Gtk_Tree_Path then
                  Path := Get_Path (List.Selected_List, Row);
               end if;
            end;
         end if;
      end loop;
      Ref (List.Deselected_List);
      Set_Model (List.Deselected, Null_Gtk_Tree_Model);
      for Index in reverse Selection'Range loop
         if Selection (Index) then
            Row := Nth_Child (List.Deselected_List, Null_Iter, Index);
            Remove (List.Deselected_List, Row);
         end if;
      end loop;
      Set_Model
      (  List.Deselected,
         To_Interface (List.Deselected_List)
      );
      Unref (List.Deselected_List);
      if Path /= Null_Gtk_Tree_Path then
         Scroll_To_Cell (List.Selected, Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Right")
         )  );
   end Right;

   procedure Selected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_List
             )  is
   begin
      if Count_Selected_Rows (Selection) > 0 then
         Set_Sensitive (List.Left_Button, True);
         if List.Release_Button /= null then
            Set_Sensitive (List.Release_Button, True);
         end if;
         Set_Sensitive
         (  List.Down_Button,
            not Iter_Is_Selected
                (  Selection,
                   Nth_Child
                   (  List.Selected_List,
                      Null_Iter,
                      N_Children (List.Selected_List) - 1
         )      )  );
         Set_Sensitive
         (  List.Up_Button,
            not Iter_Is_Selected
                (  Selection,
                   Get_Iter_First (List.Selected_List)
         )      );
      else
         Set_Sensitive (List.Left_Button, False);
         Set_Sensitive (List.Down_Button, False);
         Set_Sensitive (List.Up_Button,   False);
         if List.Release_Button /= null then
            Set_Sensitive (List.Release_Button, False);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selected_Changed")
         )  );
   end Selected_Changed;

   procedure Select_Features
             (  Widget : not null access Gtk_Fuzzy_Features_List_Record;
                List   : Fuzzy.Feature.Handle.Container.Set
             )  is
      Selected : constant Fuzzy.Feature.Handle.Container.Set :=
                          Get_Selected (Widget);
      Row   : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Value : GValue;
   begin
      Unselect_All (Get_Selection (Widget.Selected));
      Init (Value, GType_Feature);
      for Index in 1..Get_Size (List) loop
         declare
            Feature : constant Feature_Handle := Ref (List, Index);
         begin
            if not Is_In (Selected, Feature) then
               Set_Feature (Value, Feature);
               Append (Widget.Selected_List, Row);
               Gtk.Missed.Set
               (  Widget.Selected_List,
                  Row,
                  0,
                  Get_Name (Feature)
               );
               Set_Value (Widget.Selected_List, Row, 1, Value);
               Gtk.Missed.Set
               (  Widget.Selected_List,
                  Row,
                  2,
                  Get_Feature_Icon (Get_Class (Feature))
               );
               Select_Iter (Get_Selection (Widget.Selected), Row);
               if Path = Null_Gtk_Tree_Path then
                  Path := Get_Path (Widget.Selected_List, Row);
               end if;
            end if;
         end;
      end loop;
      Unset (Value);
      if Path /= Null_Gtk_Tree_Path then
         Scroll_To_Cell (Widget.Selected, Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Select_Features")
         )  );
   end Select_Features;

   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_List
             )  is
      Path      : Gtk_Tree_Path;
      Row       : Gtk_Tree_Iter;
      Selection : array (0..N_Children (List.Selected_List) - 1)
                     of Boolean;
   begin
      for Index in Selection'Range loop
         Row := Nth_Child (List.Selected_List, Null_Iter, Index);
         Selection (Index) :=
            Iter_Is_Selected (Get_Selection (List.Selected), Row);
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
            Select_Iter (Get_Selection (List.Selected), Row);
         end if;
      end loop;
      if Row /= Null_Iter then
         Path := Get_Path (List.Selected_List, Row);
         Scroll_To_Cell (List.Selected, Path, null, False, 0.0, 0.0);
         Path_Free (Path);
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Up")
         )  );
   end Up;

   procedure Service (Data : in out Deleted_Data) is
      procedure Delete (List : Gtk_List_Store) is
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
      end Delete;
   begin
      Delete (Data.Observer.List.Selected_List);
      Delete (Data.Observer.List.Deselected_List);
      Remove
      (  Data.Observer.List.Observers,
         Self (Data.Observer.all)
      );
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service Deleted")
         )  );
   end Service;

   procedure Service (Data : in out Renamed_Data) is
      procedure Rename (List : Gtk_List_Store) is
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
      end Rename;
   begin
      Rename (Data.Observer.List.Selected_List);
      Rename (Data.Observer.List.Deselected_List);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Service Renamed")
         )  );
   end Service;

end Gtk.Fuzzy_Features_List;
