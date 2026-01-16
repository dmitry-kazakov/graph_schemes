--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain.    Luebeck            --
--        Generic_Tree_View                        Winter, 2007       --
--  Implementation                                                    --
--                                Last revision :  14:34 08 Oct 2006  --
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
with Gdk.Cairo;                      use Gdk.Cairo;
with Gdk.Types;                      use Gdk.Types;
with Gdk.Types.Keysyms;              use Gdk.Types.Keysyms;
with GLib.Object;                    use GLib.Object;
with GLib.Messages;                  use GLib.Messages;
with GLib.Properties;                use GLib.Properties;
with GLib.Properties.Creation;       use GLib.Properties.Creation;
with GLib.Types;                     use GLib.Types;
with GLib.Values.Confidence_Factors; use GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Logic;        use GLib.Values.Fuzzy.Logic;
with GtkAda.Types;                   use GtkAda.Types;
with Gtk.Box;                        use Gtk.Box;
with Gtk.Cell_Renderer;              use Gtk.Cell_Renderer;
with Gtk.GEntry;                     use Gtk.GEntry;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Style;                      use Gtk.Style;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;
with Strings_Edit.Fields;            use Strings_Edit.Fields;

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;
with Name_Tables;

package body
   Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View is
   use Fuzzy_Linguistic_Sets.Unbounded_Arrays;
   use GLib.Values;
   use Gtk.Dialog;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   package Selection_Browsing is
      new Selected_Foreach_User_Data (Selection_State_Ptr);

   procedure Free is
      new Ada.Unchecked_Deallocation (Operation'Class, Operation_Ptr);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Generic_Fuzzy_Linguistic_Set_Domain." &
             "Generic_Tree_View." & Name;
   end Where;

   function Create
            (  Factory : Gtk_Fuzzy_Tree_View_Factory;
               Value   : Linguistic_Set
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View is
      Result : Gtk_Fuzzy_Linguistic_Set_Tree_View;
   begin
      Gtk_New (Result, Value);
      return Result;
   end Create;

   procedure Expand (Tree : Gtk_Tree_View; Row : Gtk_Tree_Iter) is
   begin
      if Row /= Null_Iter then
         declare
            Path   : constant Gtk_Tree_Path :=
                     Get_Path (Tree.Get_Model, Row);
            Result : Boolean;
         begin
            Result := Tree.Expand_Row (Path, True);
            Path_Free (Path);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Expand")
         )  );
   end Expand;

   function Get
            (  Store  : not null access Gtk_Tree_Store_Record'Class;
               Row    : Gtk_Tree_Iter;
               Column : GInt := 0
            )  return String is
      Value : GValue;
   begin
      Store.Get_Value (Row, Column, Value);
      return Result : constant String := Get_String (Value) do
         Unset (Value);
      end return;
   end Get;

   function Is_Expanded (Tree : Gtk_Tree_View; Row : Gtk_Tree_Iter)
      return Boolean is
   begin
      if Row = Null_Iter then
         return False;
      else
         declare
            Path   : constant Gtk_Tree_Path :=
                     Get_Path (Tree.Get_Model, Row);
            Result : constant Boolean := Tree.Row_Expanded (Path);
         begin
            Path_Free (Path);
            return Result;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Is_Expanded")
         )  );
         return False;
   end Is_Expanded;

   procedure Scroll
             (  Tree   : Gtk_Tree_View;
                Row    : Gtk_Tree_Iter;
                Column : GInt := 0
             )  is
   begin
      if Row /= Null_Iter then
         declare
            Path : constant Gtk_Tree_Path :=
                   Get_Path (Tree.Get_Model, Row);
         begin
            Tree.Scroll_To_Cell
            (  Path,
               Tree.Get_Column (Column),
               False,
               0.0,
               0.0
            );
            Path_Free (Path);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Scroll")
         )  );
   end Scroll;

   procedure On_Selected
             (  Model : Gtk_Tree_Model;
                Path  : Gtk_Tree_Path;
                Iter  : Gtk_Tree_Iter;
                Data  : Selection_State_Ptr
             )  is
      List     : constant GInt_Array := Get_Indices (Path);
      Points   : Points_Indices.Set;
      Var_No   : Variable_Index;
      Point_No : Point_Index;
   begin
      case List'Length is
         when 1 => -- A variable selected
            Var_No := Variable_Index (List (List'First) + 1);
            if not Data.Points.Is_In (Var_No) then
               Data.Points.Add (Var_No, Points);
            end if;
            Data.Variables.Add (Point_Index (Var_No));
         when 2 => -- A point selected
            Var_No   := Variable_Index (List (List'First) + 1);
            Point_No := Point_Index (List (List'First + 1) + 1);
            if Data.Points.Is_In (Var_No) then
               Points := Data.Points.Get (Var_No);
               if not Points.Is_In (Point_No) then
                  Points.Add (Point_No);
                  Data.Points.Replace (Var_No, Points);
               end if;
            else
               Points.Add (Point_No);
               Data.Points.Add (Var_No, Points);
            end if;
         when others =>
            null;
      end case;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Selected")
         )  );
   end On_Selected;

   procedure Update
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Store  : not null access Gtk_Tree_Store_Record'Class;
                Parent : Gtk_Tree_Iter;
                Var    : Variable;
                Color  : String
             );

   procedure Accumulate_Set
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Accumulate_Record'Class
             )  is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Button.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Domain  : Accumulation_Domain (Button.Data.Ptr);
   begin
      if Drawing.Cardinality = 0 then
         return;
      end if;
      if Data.Acc_Dialog = null then
         Gtk_New
         (  Data.Acc_Dialog,
            Style_Get (Data.Tree, "accumulation-dialog-title"),
            null,
            Modal
         );
         Gtk_New
         (  Data.Acc_Set,
            Domain,
            Fuzzy.Set'(1..Drawing.Cardinality => Confidence'First)
         );
         Data.Acc_Set.Set_Editable (True);
         Data.Acc_Dialog.Get_Content_Area.Pack_Start (Data.Acc_Set);
         Data.Acc_Set.Show_All;
         Add_Button_From_Stock
         (  Dialog   => Data.Acc_Dialog,
            Response => Gtk_Response_OK,
            Label    => Style_Get (Data.Tree, "accumulate-button-label")
         );
         Add_Button_From_Stock
         (  Dialog   => Data.Acc_Dialog,
            Response => Gtk_Response_Close,
            Label    => Style_Get (Data.Tree, "hide-button-label")
         );
         Add_Button_From_Stock
         (  Dialog   => Data.Acc_Dialog,
            Response => Gtk_Response_Cancel,
            Label    => Style_Get (Data.Tree, "cancel-button-label")
         );
      else
         declare
            Set : Fuzzy.Set (1..Drawing.Cardinality) :=
                     (others => Confidence'First);
            Old : Fuzzy.Set renames Data.Acc_Set.Get;
         begin
            for Index in Set'Range loop
               declare
                  Name : String renames Domain.Get_Name (Index);
               begin
                  for Other in 1..Data.Acc_Set.Get_Cardinality loop
                     if Data.Acc_Set.Get_Name (Other) = Name then
                        Set (Index) := Old (Old'First + Other - 1);
                        exit;
                     end if;
                  end loop;
               end;
            end loop;
            Data.Acc_Set.Put (Domain, Set);
         end;
      end if;
      Data.Acc_Dialog.Show_All;
      case Data.Acc_Dialog.Run is
         when Gtk_Response_OK =>
            Data.Set.Get.Show_Accumulated (Data.Acc_Set.Get);
         when Gtk_Response_Close =>
            Data.Set.Get.Hide_Accumulated;
         when others =>
            null;
      end case;
      Data.Acc_Dialog.Hide;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Accumulate_Set")
         )  );
   end Accumulate_Set;

   procedure Add
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record;
                Name   : UTF8_String;
                Op     : Operation'Class
             )  is
      Ref : Operation_Ref;
   begin
      Ref.Ptr := new Operation'Class'(Op);
      Button.Set.Add (Name, Ref);
      Button.Update_Combo;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Add")
         )  );
   end Add;

   procedure Adjust (Ref : in out Operation_Ref) is
   begin
      if Ref.Ptr /= null then
         Ref.Ptr := new Operation'Class'(Ref.Ptr.all);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Adjust")
         )  );
   end Adjust;

   function Check
            (  Op       : Unary_Operation;
               Selected : Selection_Subtype
            )  return Boolean is
   begin
       case Selected.Mode is
          when All_Points | Single_Variable =>
             return True;
          when others =>
             return False;
       end case;
   end Check;

   function Check
            (  Op       : Binary_Operation;
               Selected : Selection_Subtype
            )  return Boolean is
   begin
       case Selected.Mode is
          when All_Points | Single_Variable =>
             return True;
          when others =>
             return False;
       end case;
   end Check;

   function Check
            (  Op       : Multiple_Operation;
               Selected : Selection_Subtype
            )  return Boolean is
   begin
       case Selected.Mode is
          when Complete_Variables =>
             return Selected.Selected.Get_Size > 1;
          when Variables_Range =>
             return Selected.To_Variable - Selected.From_Variable = 1;
          when others =>
             return False;
       end case;
   end Check;

   procedure Commit_Left_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             )  is
      Selected : aliased Selection_State;
      First    : Boolean := True;
      Value    : Confidence;
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Widget.Data.Ptr.all;
      Set      : Unbounded_Arrays.Unbounded_Array renames
                    Data.Set.Get.Data.Ptr.Set;
   begin
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      if not Selected.Points.Is_Empty then
         Data.Set.Get.Data.Ptr.Hide_Accumulated;
         Value := Get (Cell);
         for Index in 1..Selected.Points.Get_Size loop
            declare
               Var_No   : constant Variable_Index :=
                             Selected.Points.Get_Key (Index);
               Var      : Variable := Get (Set, Positive (Var_No));
               Point_No : Positive;
               Domain   : Number'Base;
               Left     : Confidence;
               Right    : Confidence;
               Max      : Confidence;
               Min      : Confidence;
               Points   : Points_Indices.Set renames
                             Selected.Points.Get (Index);
            begin
               for No in 1..Points.Get_Size loop
                  Point_No := Positive (Get (Points, No));
                  if Point_No in 1..Get_Points_Number (Var) then
                     Get_Point
                     (  Var,
                        Point_No,
                        Domain,
                        Left,
                        Min,
                        Max,
                        Right
                     );
                     Left := Value;
                     Set_Point
                     (  Var,
                        Point_No,
                        Left,
                        Min,
                        Max,
                        Right,
                        False
                     );
                  end if;
               end loop;
               Data.Undo_Stack.Push
               (  (Replace, First, Var_No, Get (Set, Positive (Var_No)))
               );
               First := False;
               Set.Put (Positive (Var_No), Var);
            end;
         end loop;
         Data.Undo_Stack.Push ((Reselect, First, Selected));
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
         Widget.Update (Selected.Points);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Left_Edit")
         )  );
   end Commit_Left_Edit;

   procedure Commit_Right_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             )  is
      Selected : aliased Selection_State;
      First    : Boolean := True;
      Value    : Confidence;
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Widget.Data.Ptr.all;
      Set      : Unbounded_Arrays.Unbounded_Array renames
                    Data.Set.Get.Data.Ptr.Set;
   begin
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      if not Selected.Points.Is_Empty then
         Data.Set.Get.Data.Ptr.Hide_Accumulated;
         Value := Get (Cell);
         for Index in 1..Selected.Points.Get_Size loop
            declare
               Var_No   : constant Variable_Index :=
                             Selected.Points.Get_Key (Index);
               Point_No : Positive;
               Var      : Variable := Get (Set, Positive (Var_No));
               Domain   : Number'Base;
               Left     : Confidence;
               Right    : Confidence;
               Max      : Confidence;
               Min      : Confidence;
               Points   : Points_Indices.Set renames
                             Selected.Points.Get (Index);
            begin
               for No in 1..Points.Get_Size loop
                  Point_No := Positive (Points.Get (No));
                  if Point_No in 1..Get_Points_Number (Var) then
                     Get_Point
                     (  Var,
                        Point_No,
                        Domain,
                        Left,
                        Min,
                        Max,
                        Right
                     );
                     Right := Value;
                     Set_Point
                     (  Var,
                        Point_No,
                        Left,
                        Min,
                        Max,
                        Right,
                        False
                     );
                  end if;
               end loop;
               Data.Undo_Stack.Push
               (  (Replace, First, Var_No, Get (Set, Positive (Var_No)))
               );
               First := False;
               Put (Set, Positive (Var_No), Var);
            end;
         end loop;
         Data.Undo_Stack.Push ((Reselect, First, Selected));
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
         Widget.Update (Selected.Points);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Right_Edit")
         )  );
   end Commit_Right_Edit;

   procedure Commit_Span_Edit
             (  Cell   : access
                         Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             )  is
      Selected : aliased Selection_State;
      First    : Boolean := True;
      Value    : Fuzzy_Boolean;
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Widget.Data.Ptr.all;
      Set      : Unbounded_Arrays.Unbounded_Array renames
                    Data.Set.Get.Data.Ptr.Set;
   begin
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      if not Selected.Points.Is_Empty then
         Data.Set.Get.Data.Ptr.Hide_Accumulated;
         Value := Cell.Get;
         for Index in 1..Selected.Points.Get_Size loop
            declare
               Var_No   : constant Variable_Index :=
                             Selected.Points.Get_Key (Index);
               Point_No : Positive;
               Var      : Variable := Get (Set, Positive (Var_No));
               Domain   : Number'Base;
               Left     : Confidence;
               Right    : Confidence;
               Max      : Confidence;
               Min      : Confidence;
               Points   : Points_Indices.Set renames
                             Selected.Points.Get (Index);
            begin
               for No in 1..Points.Get_Size loop
                  Point_No := Positive (Get (Points, No));
                  if Point_No in 1..Get_Points_Number (Var) then
                     Get_Point
                     (  Var,
                        Point_No,
                        Domain,
                        Left,
                        Min,
                        Max,
                        Right
                     );
                     Max := Value.Possibility;
                     Min := Value.Necessity;
                     Set_Point
                     (  Var,
                        Point_No,
                        Confidence'Max
                        (  Confidence'Min (Max, Left),
                           Min
                        ),
                        Min,
                        Max,
                        Confidence'Max
                        (  Confidence'Min (Max, Right),
                           Min
                        ),
                        False
                     );
                  end if;
               end loop;
               Data.Undo_Stack.Push
               (  (Replace, First, Var_No, Get (Set, Positive (Var_No)))
               );
               First := False;
               Put (Set, Positive (Var_No), Var);
            end;
         end loop;
         Data.Undo_Stack.Push ((Reselect, First, Selected));
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
         Widget.Update (Selected.Points);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Span_Edit")
         )  );
   end Commit_Span_Edit;

   procedure Domain_Marked
             (  Widget : access Gtk_Widget_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
   begin
      if Data.Find /= null then
         case Drawing.Area_Selection.State is
            when None | Active =>
               Data.Find.Set_Sensitive (False);
            when Inactive =>
               Data.Find.Set_Sensitive (True);
         end case;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Domain_Marked")
         )  );
   end Domain_Marked;

   procedure Domain_Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
   begin
      if Data.X_Move /= null then
         Data.X_Move.Set_Tooltip_Text
         (  Style_Get (Data.Set.Get, "x-move-tip")
         );
      end if;
      if Data.Y_Move /= null then
         Data.Y_Move.Set_Tooltip_Text
         (  Style_Get (Data.Set.Get, "y-move-tip")
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Domain_Style_Updated")
         )  );
   end Domain_Style_Updated;

   procedure Edited
             (  Cell   : access Gtk_Cell_Renderer_Text_Record'Class;
                Params : GValues;
                Widget : Gtk_Fuzzy_Linguistic_Set_Tree_View
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Name : constant String := Get_String (Nth (Params, 2));
      Row  : constant Gtk_Tree_Iter :=
                Get_Iter_From_String
                (  Data.Store,
                   Get_String (Nth (Params, 1))
                );
   begin
      if Row /= Null_Iter then
         declare
            Path   : constant Gtk_Tree_Path :=
                     Get_Path (Data.Store, Row);
            List   : constant GLib.GInt_Array := Get_Indices (Path);
            Var_No : Variable_Index;
         begin
            Path_Free (Path);
            if Get (Data.Store, Row) /= Name then
               case List'Length is
                  when 1 => -- Change the name
                     Var_No := Variable_Index (List (List'First) + 1);
                     Data.Redo_Stack.Erase;
                     if Data.Redo /= null then
                        Data.Redo.Set_Sensitive (False);
                     end if;
                     Data.Undo_Stack.Push
                     (  (  Rename,
                           True,
                           Var_No,
                           Create (Get (Data.Store, Row))
                     )  );
                     if Data.Undo /= null then
                        Data.Undo.Set_Sensitive (True);
                     end if;
                     Set_Name
                     (  Data,
                        Natural (List (List'First) + 1),
                        Name
                     );
                  when 2 => -- Change the value
                     begin
                        Set_Value
                        (  Data,
                           Positive (List (List'First) + 1),
                           Positive (List (List'First + 1) + 1),
                           Value (Widget, Name)
                        );
                     exception
                        when others =>
                           null;
                     end;
                  when others =>
                     null;
               end case;
            end if;
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edited")
         )  );
   end Edited;

   procedure Edit_Add
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable =>
               Data.Insert
               (  Selected,
                  Positive (Current.Variable),
                  0
               );
            when Points_Range | All_Points =>
               Data.Insert
               (  Selected,
                  Positive (Current.Range_At),
                  Positive (Current.To_Point)
               );
            when others =>
               null;
         end case;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Add")
         )  );
   end Edit_Add;

   procedure Edit_Copy
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable =>
               Data.Insert
               (  Selected,
                  Positive (Current.Variable + 1),
                  Drawing.Set.Get (Positive (Current.Variable))
               );
            when others =>
               null;
         end case;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Copy")
         )  );
   end Edit_Copy;

   procedure Edit_Down
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable | All_Points |
                 Variables_Range | Complete_Variables =>
               null;
            when others =>
               return;
         end case;
      end;
      if (  Drawing.Cardinality
         >  Positive
            (  Selected.Points.Get_Key (Selected.Points.Get_Size)
         )  )
      then
         Data.Move (Selected.Points, 1);
         Data.Undo_Stack.Push ((Move, True, Drawing.Selected, -1));
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Down")
         )  );
   end Edit_Down;

   procedure Edit_Find
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record'Class
             )  is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Button.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Span    : Interval;
      Level   : Fuzzy_Boolean;
   begin
      if Drawing.Area_Selection.State = Active then
         return;
      end if;
      declare
         Area : Drawing_Area renames Drawing.Area;
         X1   : constant Number'Base :=
                         From_X (Area, Drawing.Area_Selection.X1);
         X2   : constant Number'Base :=
                         From_X (Area, Drawing.Area_Selection.X2);
         Y1   : constant Truth :=
                         From_Y (Area, Drawing.Area_Selection.Y1);
         Y2   : constant Truth :=
                         From_Y (Area, Drawing.Area_Selection.Y2);
      begin
         if X1 > X2 then
            Span.From := X2;
            Span.To   := X1;
         else
            Span.From := X1;
            Span.To   := X2;
         end if;
         if Y1 > Y2 then
            Level.Possibility := To_Confidence (Y1);
            Level.Necessity   := To_Confidence (Y2);
         else
            Level.Possibility := To_Confidence (Y2);
            Level.Necessity   := To_Confidence (Y1);
         end if;
      end;
      declare
         Points : Selection;
         List   : Points_Indices.Set;
      begin
         for Index in 1..Drawing.Cardinality loop
            declare
               Var   : Variable renames Drawing.Set.Get (Index);
               From  : Positive;
               To    : Natural;
               Value : Number;
               Left  : Confidence;
               Min   : Confidence;
               Max   : Confidence;
               Right : Confidence;
            begin
               Find (Var, Span, From, To);
               if From <= To then
                  Erase (List);
                  for Point in From..To loop
                     Get_Point
                     (  Var,
                        Point,
                        Value,
                        Left,
                        Min,
                        Max,
                        Right
                     );
                     if (  Min <= Level.Possibility
                        and then
                           Max >= Level.Necessity
                        )
                     then
                        List.Add (Point_Index (Point));
                     end if;
                  end loop;
                  if not Is_Empty (List) then
                     Expand (Data.Tree, Data.Get_Row (Index));
                     Points.Add (Variable_Index (Index), List);
                  end if;
               end if;
            end;
         end loop;
         Erase (List);
         declare
            Context : constant Cairo_Context :=
                      Create (Data.Set.Get.View.Get_Window);
         begin
            Drawing.Draw_Area_Selection (Context, False);
            Drawing.Draw_Selected (Context);
            Destroy (Context);
         exception
            when others =>
               Destroy (Context);
               raise;
         end;
         Drawing.Area_Selection.State := None;
         Data.Set_Selection (Points, List);
         Data.Set.Get.Marked;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Find")
         )  );
   end Edit_Find;

   procedure Edit_Exec
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Op : Operation'Class renames
                 Find
                 (  Button.Set,
                    Button.Get_Combo.Get_Active_Text
                 ) .Ptr.all;
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         if not Check (Op, Current) then
            return;
         end if;
         case Current.Mode is
            when Single_Variable =>
               Data.Insert
               (  Selected,
                  Positive (Current.Variable + 1),
                  Execute
                  (  Op,
                     (  1 =>
                           Get
                           (  Drawing.Set,
                              Positive (Current.Variable)
               )  )  )     );
            when All_Points =>
               Data.Insert
               (  Selected,
                  Positive (Current.Range_At + 1),
                  Execute
                  (  Op,
                     (  1 =>
                           Get
                           (  Drawing.Set,
                              Positive (Current.Range_At)
               )  )  )     );
            when Variables_Range =>
               declare
                  Arguments : Array_Of_Variables
                              (  Positive (Current.From_Variable)
                              .. Positive (Current.To_Variable)
                              );
               begin
                  for Index in Arguments'Range loop
                     Arguments (Index) := Drawing.Set.Get (Index);
                  end loop;
                  Data.Insert
                  (  Selected,
                     Positive (Current.To_Variable + 1),
                     Execute (Op, Arguments)
                  );
               end;
            when Complete_Variables =>
               declare
                  Arguments : Array_Of_Variables
                                 (1.. Get_Size (Current.Selected));
                  Var_No : Variable_Index;
               begin
                  for Index in Arguments'Range loop
                     Var_No := Current.Selected.Get_Key (Index);
                     Arguments (Index) :=
                        Drawing.Set.Get (Positive (Var_No));
                  end loop;
                  Data.Insert
                  (  Selected,
                     Positive (Var_No + 1),
                     Execute (Op, Arguments)
                  );
               end;
            when others =>
               null;
         end case;
      end;
   exception
      when Ada.IO_Exceptions.End_Error =>
         null;
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Exec")
         )  );
   end Edit_Exec;

   procedure Edit_Exec_Changed
             (  Combo_Entry : access Gtk_Combo_Box_Text_Record'Class;
                Data        : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      Data.Exec.Set_Sensitive
      (  Data.Exec.Set.Find
         (  Combo_Entry.Get_Active_Text
         ) .Ptr.Check (Drawing.Get_Selection (Selected.Points))
      );
   exception
      when Ada.IO_Exceptions.End_Error =>
         Data.Exec.Set_Sensitive (False);
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Exec_Changed")
         )  );
   end Edit_Exec_Changed;

   procedure Edit_New
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_New_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable =>
               Data.Insert
               (  Selected,
                  Positive (Current.Variable + 1)
               );
            when All_Points =>
               Data.Insert
               (  Selected,
                  Positive (Current.Range_At + 1)
               );
            when Variables_Range =>
               Data.Insert
               (  Selected,
                  Positive (Current.To_Variable + 1)
               );
            when Empty =>
               Data.Insert (Selected, Drawing.Cardinality + 1);
            when others =>
               null;
         end case;
      end;
   end Edit_New;

   procedure Edit_Purge
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable | All_Points | Variables_Range |
                 Complete_Variables =>
               Purge (Data, Selected);
            when others =>
               null;
         end case;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Purge")
         )  );
   end Edit_Purge;

   procedure Edit_Redo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record'Class
             )  is
      Data  : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                 Button.Data.Ptr.all;
      First : Boolean := True;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      while not Data.Redo_Stack.Is_Empty loop
         declare
            This : constant Edit_Undo_Item := Top (Data.Redo_Stack);
         begin
            Redo (Data, This, First, Data.Undo_Stack);
            Data.Redo_Stack.Pop;
            exit when This.First;
         end;
      end loop;
      if Data.Redo_Stack.Is_Empty and then Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      if not First then
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Redo")
         )  );
   end Edit_Redo;

   procedure Edit_Remove
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record'Class
             )  is
      Data  : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                 Button.Data.Ptr.all;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Remove (Data);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Remove")
         )  );
   end Edit_Remove;

   procedure Edit_Undo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record'Class
             )  is
      Data  : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                 Button.Data.Ptr.all;
      First : Boolean := True;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      while not Data.Undo_Stack.Is_Empty loop
         if Data.Redo_Stack.Is_Empty and then Data.Redo /= null then
            Data.Redo.Set_Sensitive (True);
         end if;
         declare
            This : constant Edit_Undo_Item := Top (Data.Undo_Stack);
         begin
            Redo (Data, This, First, Data.Redo_Stack);
            Data.Undo_Stack.Pop;
            exit when This.First;
         end;
      end loop;
      if Data.Undo_Stack.Is_Empty and then Data.Undo /= null then
         Data.Undo.Set_Sensitive (False);
      end if;
      if not First then
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Undo")
         )  );
   end Edit_Undo;

   procedure Edit_Up
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record'Class
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Button.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if not Data.Is_Editable then
         return;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      declare
         Current : constant Selection_Subtype :=
                      Drawing.Get_Selection (Selected.Points);
      begin
         case Current.Mode is
            when Single_Variable | All_Points |
                 Variables_Range | Complete_Variables =>
               null;
            when others =>
               return;
         end case;
      end;
      if Selected.Points.Get_Key (1) > 1 then
         Move (Data, Selected.Points, -1);
         Data.Undo_Stack.Push
         (  (Move, True, Drawing.Selected, 1)
         );
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Up")
         )  );
   end Edit_Up;

   procedure Edit_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
   begin
      if not Is_Editable (Data.all) or else Data.X_Move.Modified then
         return;
      end if;
      declare
         Shift : constant Number'Base :=
                          (  Number'Base (Adjustment.Get_Value)
                          -  Data.X_Move.Page.From
                          );
         First    : Boolean := True;
         Coalesce : constant Boolean :=
                       (  not Data.Undo_Stack.Is_Empty
                       and then
                          Top (Data.Undo_Stack).Action = Slide_X
                       );
         Drawing  : Linguistic_Set_Data'Class renames
                       Data.Set.Get.Data.Ptr.all;
         Current  : constant Selection_Subtype :=
                       Drawing.Get_Selection (Drawing.Selected);
      begin
         case Current.Mode is
            when Single_Variable | Variables_Range | All_Points =>
               Drawing.Hide_Accumulated;
               for Index in 1..Drawing.Selected.Get_Size loop
                  declare
                     Var_No : constant Variable_Index :=
                              Drawing.Selected.Get_Key (Index);
                     Var : constant Variable :=
                              Drawing.Set.Get (Positive (Var_No));
                  begin
                     if not Coalesce then
                        Data.Undo_Stack.Push
                        (  (Replace, First, Var_No, Var)
                        );
                        First := False;
                     end if;
                     Drawing.Set.Put
                     (  Positive (Var_No),
                        Var + Shift
                     );
                     Drawing.Updated := True;
                  end;
               end loop;
            when Points_Range =>
               Drawing.Hide_Accumulated;
               declare
                  Var : Variable :=
                        Drawing.Set.Get (Positive (Current.Range_At));
               begin
                  for Index in Current.From_Point..Current.To_Point loop
                     begin
                        Move_Point
                        (  Var,
                           Positive (Index),
                           Get_Value (Var, Positive (Index)) + Shift
                        );
                     exception
                        when Constraint_Error =>
                           Data.X_Move.Modified := True;
                           Set_Value
                           (  Adjustment,
                              GDouble (Data.X_Move.Page.From)
                           );
                           Data.X_Move.Modified := False;
                           return;
                     end;
                  end loop;
                  if not Coalesce then
                     Data.Undo_Stack.Push
                     (  (  Replace,
                           First,
                           Current.Range_At,
                           Get
                           (  Drawing.Set,
                              Positive (Current.Range_At)
                     )  )  );
                     First := False;
                  end if;
                  Drawing.Set.Put
                  (  Positive (Current.Range_At),
                     Var
                  );
                  Drawing.Updated := True;
               end;
            when others =>
               return;
         end case;
         if not Coalesce then
            Data.Undo_Stack.Push
            (  (Slide_X, First, Data.X_Move.Page.From)
            );
            First := False;
         end if;
         Data.Widget.Update (Drawing.Selected);
         Data.X_Move.Page.From :=
            Number'Base (Adjustment.Get_Value);
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Refresh;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_X")
         )  );
   end Edit_X;

   procedure Edit_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
   begin
      if not Is_Editable (Data.all) or else Data.Y_Move.Modified then
         return;
      end if;
      declare
         Scroll : Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record'Class renames
                     Data.Y_Move.all;
         To : constant Confidence :=
                 To_Confidence (1.0 - Truth (Adjustment.Get_Value));
         First    : Boolean := True;
         Coalesce : constant Boolean :=
                       (  not Data.Undo_Stack.Is_Empty
                       and then
                          Top (Data.Undo_Stack).Action = Slide_Y
                       );
         Drawing  : Linguistic_Set_Data'Class renames
                       Data.Set.Get.Data.Ptr.all;
      begin
         if (  Scroll.Var_No > Drawing.Cardinality
            or else
               To = Scroll.Max
            )
         then
            return;
         end if;
         if not Coalesce then
            declare
               Current : constant Selection_Subtype :=
                            Drawing.Get_Selection (Drawing.Selected);
            begin
               case Current.Mode is
                  when Points_Range | All_Points =>
                     Drawing.Hide_Accumulated;
                     Data.Y_Move.Set_Sensitive (True);
                     Data.Y_Move.Set_Points
                     (  Current.Range_At,
                        Current.From_Point,
                        Current.To_Point
                     );
                  when others =>
                     return;
               end case;
            end;
         end if;
         declare
            Shift : constant Truth := Scale (To) - Scale (Scroll.Max);
            Var   : Variable := Drawing.Set.Get (Scroll.Var_No);
         begin
            for Index in Scroll.First_Point..Scroll.Last_Point loop
               declare
                  Value : Number;
                  Left  : Confidence;
                  Min   : Confidence;
                  Max   : Confidence;
                  Right : Confidence;
               begin
                  Get_Point
                  (  Var,
                     Index,
                     Value,
                     Left,
                     Min,
                     Max,
                     Right
                  );
                  Left  := To_Confidence (Scale (Left)  + Shift);
                  Min   := To_Confidence (Scale (Min)   + Shift);
                  Max   := To_Confidence (Scale (Max)   + Shift);
                  Right := To_Confidence (Scale (Right) + Shift);
                  Set_Point
                  (  Var,
                     Index,
                     Left,
                     Min,
                     Max,
                     Right,
                     False
                  );
               end;
            end loop;
            if not Coalesce then
               Data.Undo_Stack.Push
               (  (  Replace,
                     First,
                     Variable_Index (Scroll.Var_No),
                     Drawing.Set.Get (Scroll.Var_No)
               )  );
               First := False;
            end if;
            Drawing.Set.Put (Scroll.Var_No, Var);
            Drawing.Updated := True;
            if not Coalesce then
               Data.Undo_Stack.Push ((Slide_Y, First, Scroll.Max));
               First := False;
            end if;
            Scroll.Max := To;
            Data.Widget.Update
            (  Data.Store,
               Data.Get_Row (Scroll.Var_No),
               Var,
               Data.Checked.Ptr.Text
            );
         end;
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Refresh;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Edit_Y")
         )  );
   end Edit_Y;

   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Boolean is
   begin
      return not Widget.Data.Ptr.Undo_Stack.Is_Empty;
   end Edited;

   function Execute (Op : And_Operation; Arguments : Array_Of_Variables)
      return Variable is
      Result : Variable := Arguments (Arguments'First);
   begin
      for Index in Arguments'First + 1..Arguments'Last loop
         Result := Result and Arguments (Index);
      end loop;
      return Result;
   end Execute;

   function Execute (Op : Not_Operation; Arguments : Array_Of_Variables)
      return Variable is
   begin
      return not Arguments (Arguments'First);
   end Execute;

   function Execute (Op : Or_Operation; Arguments : Array_Of_Variables)
      return Variable is
      Result : Variable := Arguments (Arguments'First);
   begin
      for Index in Arguments'First + 1..Arguments'Last loop
         Result := Result or Arguments (Index);
      end loop;
      return Result;
   end Execute;

   function Execute (Op : Xor_Operation; Arguments : Array_Of_Variables)
      return Variable is
      Result : Variable := Arguments (Arguments'First);
   begin
      for Index in Arguments'First + 1..Arguments'Last loop
         Result := Result xor Arguments (Index);
      end loop;
      return Result;
   end Execute;

   procedure Finalize (Monitor : in out Selection_Monitor) is
   begin
      Monitor.Data.Selection_Pending := Monitor.Nested;
      if not Monitor.Nested then
         declare
            Selected : aliased Selection_State;
         begin
            Selection_Browsing.Selected_Foreach
            (  Get_Selection (Monitor.Data.Tree),
               On_Selected'Access,
               Selected'Unchecked_Access
            );
            Monitor.Data.Set.Get.Set_Selection (Selected.Points);
            Monitor.Data.Set_Buttons (Selected.Points);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Selection_Monitor)")
         )  );
   end Finalize;

   procedure Finalize (Ref : in out Edit_Data_Handle) is
   begin
      if Ref.Is_Valid then
         declare
            Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class
                      renames Ref.Ptr.all;
         begin
            case Ref.Widget is
               when Accumulate_Button => Data.Accumulate := null;
               when Add_Button        => Data.Add        := null;
               when Copy_Button       => Data.Copy       := null;
               when Down_Button       => Data.Down       := null;
               when Exec_Button       => Data.Exec       := null;
               when Find_Button       => Data.Find       := null;
               when Purge_Button      => Data.Purge      := null;
               when Redo_Button       => Data.Redo       := null;
               when Remove_Button     => Data.Remove     := null;
               when New_Button        => Data.New_Var    := null;
               when Undo_Button       => Data.Undo       := null;
               when Up_Button         => Data.Up         := null;
               when X_Move_Bar        => Data.X_Move     := null;
               when Y_Move_Bar        => Data.Y_Move     := null;
            end case;
         end;
      end if;
      Edit_Data_Handles.Handle (Ref).Finalize;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Edit_Data_Handle)")
         )  );
   end Finalize;

   procedure Finalize
             (  Data : in out Fuzzy_Linguistic_Set_Tree_View_Data
             )  is
   begin
      if Data.Acc_Dialog /= null then
         GLib.Object.Checked_Destroy (Data.Acc_Dialog);
         Data.Acc_Dialog := null;
      end if;
      Standard.Object.Entity (Data).Finalize;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Fuzzy_Linguistic_Set_Tree_View_Data)")
         )  );
   end Finalize;

   procedure Finalize (Ref : in out Operation_Ref) is
   begin
      Free (Ref.Ptr);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Operation_Ref)")
         )  );
   end Finalize;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Purge  : Boolean := True
            )  return Linguistic_Set is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Result  : Linguistic_Set;
   begin
      if Purge then
         declare
            Var : Variable;
         begin
            for Index in 1..Drawing.Cardinality loop
               Var := Drawing.Set.Get (Index);
               Fuzzy_Linguistics.Purge (Var);
               Add (Result, Data.Get_Name (Index), Var);
            end loop;
         end;
      else
         for Index in 1..Drawing.Cardinality loop
            Add
            (  Result,
               Data.Get_Name (Index),
               Drawing.Set.Get (Index)
            );
         end loop;
      end if;
      return Result;
   end Get;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Index  : Positive
            )  return Variable is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
   begin
      if Drawing.Cardinality < Index then
         raise Constraint_Error;
      else
         return Drawing.Set.Get (Index);
      end if;
   end Get;

   function Get_Accumulate_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Accumulate is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      if Data.Accumulate = null then
         -- Accumulate button
         Data.Accumulate :=
            new Gtk_Fuzzy_Linguistic_Set_Accumulate_Record;
         Set (Data.Accumulate.Data, Widget.Data.Ptr);
         Accumulate_Buttons.Initialize (Data.Accumulate);
         Accumulate_Callbacks.Connect
         (  Data.Accumulate,
            "clicked",
            Accumulate_Set'Access
         );
      end if;
      return Data.Accumulate;
   end Get_Accumulate_Button;

   function Get_Add_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Add is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Add = null then
         Data.Add := new Gtk_Fuzzy_Linguistic_Set_Edit_Add_Record;
         Set (Data.Add.Data, Widget.Data.Ptr);
         Edit_Add_Buttons.Initialize (Data.Add);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode in Points_Range..Single_Variable
            );
         Data.Add.Set_Sensitive (Enable);
         Add_Callbacks.Connect
         (  Data.Add,
            "clicked",
            Edit_Add'Access
         );
      end if;
      return Data.Add;
   end Get_Add_Button;

   function Get_Cardinality
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Natural is
   begin
      return Widget.Data.Ptr.Set.Get.Get_Cardinality;
   end Get_Cardinality;

   function Get_Cardinality (Domain : Accumulation_Domain)
      return Natural is
   begin
      return Ptr (Get (Domain.Data.Set).Data).Cardinality;
   end Get_Cardinality;

   function Get_Combo
            (  Button : not null access
                        Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record
            )  return not null access
                      Gtk_Combo_Box_Text_Record'Class is
   begin
      return Button.Combo.Get;
   end Get_Combo;

   function Get_Copy_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Copy is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Copy = null then
         Data.Copy := new Gtk_Fuzzy_Linguistic_Set_Edit_Copy_Record;
         Data.Copy.Data.Set (Widget.Data.Ptr);
         Edit_Copy_Buttons.Initialize (Data.Copy);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode = Single_Variable
            );
         Data.Copy.Set_Sensitive (Enable);
         Copy_Callbacks.Connect
         (  Data.Copy,
            "clicked",
            Edit_Copy'Access
         );
      end if;
      return Data.Copy;
   end Get_Copy_Button;

   function Get_Domain_View
            (  Widget : not null access
               Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return not null access
                      Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class is
   begin
      return Widget.Data.Ptr.Set.Get;
   end Get_Domain_View;

   function Get_Domain_Column_Note
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return UTF8_String is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      if Data.Domain_Note.Is_Valid then
         return Data.Domain_Note.Ptr.Text;
      else
         return "";
      end if;
   end Get_Domain_Column_Note;

   function Get_Down_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Down is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Widget.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Enable   : Boolean;
   begin
      if Data.Down = null then
         Data.Down := new Gtk_Fuzzy_Linguistic_Set_Edit_Down_Record;
         Data.Down.Data.Set (Widget.Data.Ptr);
         Edit_Down_Buttons.Initialize (Data.Down);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode in All_Points..Complete_Variables
            and then
               (  Drawing.Cardinality
               >  Positive
                  (  Drawing.Selected.Get_Key
                     (  Drawing.Selected.Get_Size
            )  )  )  );
         Data.Down.Set_Sensitive (Enable);
         Down_Callbacks.Connect
         (  Data.Down,
            "clicked",
            Edit_Down'Access
         );
      end if;
      return Data.Down;
   end Get_Down_Button;

   function Get_Exec_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Exec is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Exec = null then
         Data.Exec := new Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record;
         Data.Exec.Data.Set (Widget.Data.Ptr);
         Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View.
            Initialize (Data.Exec);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Exec.Set.Find
               (  Data.Exec.Get_Combo.Get_Active_Text
               ) .Ptr.Check (Data.Get_Selection)
            );
         Data.Exec.Set_Sensitive (Enable);
         Exec_Callbacks.Connect
         (  Data.Exec,
            "clicked",
            Edit_Exec'Access
         );
         Exec_Combo_Callbacks.Connect
         (  Data.Exec.Get_Combo.all'Unchecked_Access, -- Compiler bug
            "changed",                                -- work-around
            Edit_Exec_Changed'Access,
            Widget.Data.Ptr
         );
      end if;
      return Data.Exec;
   end Get_Exec_Button;

   function Get_Find_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Find is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      if Data.Find = null then
         Data.Find := new Gtk_Fuzzy_Linguistic_Set_Edit_Find_Record;
         Data.Find.Data.Set (Widget.Data.Ptr);
         Edit_Find_Buttons.Initialize (Data.Find);
         Data.Find.Set_Sensitive (False);
         Find_Callbacks.Connect
         (  Data.Find,
            "clicked",
            Edit_Find'Access
         );
      end if;
      return Data.Find;
   end Get_Find_Button;

   function Get_Name
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Index  : Positive
            )  return UTF8_String is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      if Data.Set.Get.Data.Ptr.Cardinality < Index then
         raise Constraint_Error;
      else
         return Data.Get_Name (Index);
      end if;
   end Get_Name;

   function Get_Name
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return UTF8_String is
      Row : constant Gtk_Tree_Iter := Data.Get_Row (Index);
   begin
      if Row = Null_Iter then
         return "";
      else
         return Get (Data.Store, Row);
      end if;
   end Get_Name;

   function Get_Name
            (  Domain : Accumulation_Domain;
               Index  : Integer
            )  return String is
   begin
      return Domain.Data.Get_Name (Index);
   end Get_Name;

   function Get_New_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_New is

      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Mode   : Selection_Mode;
      Enable : Boolean;
   begin
      if Data.New_Var = null then
         Data.New_Var := new Gtk_Fuzzy_Linguistic_Set_Edit_New_Record;
         Data.New_Var.Data.Set (Widget.Data.Ptr);
         Edit_New_Buttons.Initialize (Data.New_Var);
         Mode := Data.Get_Selection.Mode;
         Enable :=
            (  Widget.Is_Editable
            and then
               (  Mode in All_Points..Variables_Range
               or else
                  Mode = Empty
            )  );
         Data.New_Var.Set_Sensitive (Enable);
         New_Callbacks.Connect
         (  Data.New_Var,
            "clicked",
            Edit_New'Access
         );
      end if;
      return Data.New_Var;
   end Get_New_Button;

   function Get_Purge_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Purge is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Purge = null then
         Data.Purge := new Gtk_Fuzzy_Linguistic_Set_Edit_Purge_Record;
         Data.Purge.Data.Set (Widget.Data.Ptr);
         Edit_Purge_Buttons.Initialize (Data.Purge);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode in All_Points..Complete_Variables
            );
         Data.Purge.Set_Sensitive (Enable);
         Purge_Callbacks.Connect
         (  Data.Purge,
            "clicked",
            Edit_Purge'Access
         );
      end if;
      return Data.Purge;
   end Get_Purge_Button;

   function Get_Redo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Redo is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Redo = null then
         Data.Redo := new Gtk_Fuzzy_Linguistic_Set_Edit_Redo_Record;
         Data.Redo.Data.Set (Widget.Data.Ptr);
         Edit_Redo_Buttons.Initialize (Data.Redo);
         Enable :=
            (  Widget.Is_Editable
            and then
               not Data.Redo_Stack.Is_Empty
            );
         Data.Redo.Set_Sensitive (Enable);
         Redo_Callbacks.Connect
         (  Data.Redo,
            "clicked",
            Edit_Redo'Access
         );
      end if;
      return Data.Redo;
   end Get_Redo_Button;

   function Get_Remove_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Remove is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Remove = null then
         Data.Remove := new Gtk_Fuzzy_Linguistic_Set_Edit_Remove_Record;
         Data.Remove.Data.Set (Widget.Data.Ptr);
         Edit_Remove_Buttons.Initialize (Data.Remove);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode /= Empty
            );
         Data.Remove.Set_Sensitive (Enable);
         Remove_Callbacks.Connect
         (  Data.Remove,
            "clicked",
            Edit_Remove'Access
         );
      end if;
      return Data.Remove;
   end Get_Remove_Button;

   function Get_Row
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return Gtk_Tree_Iter is
   begin
      return Data.Store.Nth_Child (Null_Iter, GInt (Index - 1));
   end Get_Row;

   function Get_Selection
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Selection is
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
   begin
      return Drawing.Selected;
   end Get_Selection;

   function Get_Selection
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Selection_Subtype is
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
   begin
      return Drawing.Get_Selection (Drawing.Selected);
   end Get_Selection;

   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Selection is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      return Data.Get_Selection;
   end Get_Selection;

   function Get_Selection
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Selection_Subtype is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      return Data.Get_Selection;
   end Get_Selection;

   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Tree_View is
   begin
      return Widget.Data.Ptr.Tree;
   end Get_Tree_View;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name & "TreeView"
         )
      then
         Gtk.Cell_Renderer_Fuzzy_Boolean.Install_Style_Properties
         (  Class_Ref (Class_Record.The_Type)
         );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "accumulation-dialog-title",
               Nick    => "Accumulate",
               Blurb   => "Accumulation dialog title",
               Default => "Accumulate"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "accumulate-button-label",
               Nick    => "Accumulate button",
               Blurb   => "Accumulate button label",
               Default => Stock_OK
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "cancel-button-label",
               Nick    => "Cancel button",
               Blurb   => "Cancel button label",
               Default => Stock_Cancel
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "checked-color",
               Nick    => "Text color",
               Blurb   => "Checked names color",
               Default => "black"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "domain-column-title",
               Nick    => "Domain",
               Blurb   => "Domain values column title",
               Default => "Domain"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "hide-button-label",
               Nick    => "Hide button",
               Blurb   => "Hide button label",
               Default => Stock_Delete
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "illegal-color",
               Nick    => "Wrong name color",
               Blurb   => "The color to paint wrong names",
               Default => "red"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "range-column-title",
               Nick    => "Range",
               Blurb   => "The column title of membership range",
               Default => "Range"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "left-column-title",
               Nick    => "Left",
               Blurb   => "The column title of left limit",
               Default => "Left"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "right-column-title",
               Nick    => "Right",
               Blurb   => "The column title of right limit",
               Default => "Right"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   function Get_Undo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Undo is
      Data   : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                  Widget.Data.Ptr.all;
      Enable : Boolean;
   begin
      if Data.Undo = null then
         Data.Undo := new Gtk_Fuzzy_Linguistic_Set_Edit_Undo_Record;
         Data.Undo.Data.Set (Widget.Data.Ptr);
         Edit_Undo_Buttons.Initialize (Data.Undo);
         Enable :=
            Widget.Is_Editable and then not Data.Undo_Stack.Is_Empty;
         Data.Undo.Set_Sensitive (Enable);
         Undo_Callbacks.Connect
         (  Data.Undo,
            "clicked",
            Edit_Undo'Access
         );
      end if;
      return Data.Undo;
   end Get_Undo_Button;

   function Get_Up_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Up is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Enable  : Boolean;
   begin
      if Data.Up = null then
         Data.Up := new Gtk_Fuzzy_Linguistic_Set_Edit_Up_Record;
         Data.Up.Data.Set (Widget.Data.Ptr);
         Edit_Up_Buttons.Initialize (Data.Up);
         Enable :=
            (  Widget.Is_Editable
            and then
               Data.Get_Selection.Mode in All_Points..Complete_Variables
            and then
               Drawing.Selected.Get_Key (1) > 1
            );
         Data.Up.Set_Sensitive (Enable);
         Up_Callbacks.Connect
         (  Data.Up,
            "clicked",
            Edit_Up'Access
         );
      end if;
      return Data.Up;
   end Get_Up_Button;

   function Get_X_Move_Bar
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_X is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                Data.Set.Get.Data.Ptr.all;
      Adjustment : Gtk_Adjustment;
   begin
      if Data.X_Move = null then
         Gtk_New
         (  Adjustment,
            Value => 0.0,
            Lower => 0.0,
            Upper => 1.0,
            Step_Increment => 0.01,
            Page_Increment => 1.0,
            Page_Size      => 1.0
         );
         Data.X_Move := new Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
         Data.X_Move.Data.Set (Widget.Data.Ptr);
         Initialize_HScrollbar (Data.X_Move, Adjustment);
         declare
            Current : Selection_Subtype renames Data.Get_Selection;
         begin
            case Current.Mode is
               when Single_Variable | Variables_Range | All_Points =>
                  Data.X_Move.Set_Sensitive (True);
                  Data.X_Move.Set_Variables (Drawing.Selected);
               when Points_Range =>
                  Data.X_Move.Set_Sensitive (True);
                  Data.X_Move.Set_Points
                  (  Current.Range_At,
                     Current.From_Point,
                     Current.To_Point
                  );
               when others =>
                  Data.X_Move.Set_Sensitive (False);
            end case;
         end;
         Adjustment_Callbacks.Connect
         (  Adjustment,
            "value_changed",
            Edit_X'Access,
            Widget.Data.Ptr
         );
      end if;
      return Data.X_Move;
   end Get_X_Move_Bar;

   function Get_Y_Move_Bar
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Edit_Y is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Adjustment : Gtk_Adjustment;
   begin
      if Data.Y_Move = null then
         Gtk_New
         (  Adjustment,
            Value          => GDouble (Confidence'First),
            Lower          => GDouble (Confidence'First),
            Upper          => 1.0,
            Step_Increment => GDouble (Confidence'Small),
            Page_Increment => GDouble (Confidence'Last) * 0.75,
            Page_Size      => GDouble (Confidence'Last)
         );
         Data.Y_Move := new Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record;
         Data.Y_Move.Data.Set (Widget.Data.Ptr);
         Initialize_VScrollbar (Data.Y_Move, Adjustment);
         declare
            Current : Selection_Subtype renames Data.Get_Selection;
         begin
            case Current.Mode is
               when Points_Range | All_Points =>
                  Data.Y_Move.Set_Sensitive (True);
                  Data.Y_Move.Set_Points
                  (  Current.Range_At,
                     Current.From_Point,
                     Current.To_Point
                  );
               when others =>
                  Data.Y_Move.Set_Sensitive (False);
            end case;
         end;
         Adjustment_Callbacks.Connect
         (  Adjustment,
            "value_changed",
            Edit_Y'Access,
            Widget.Data.Ptr
         );
      end if;
      return Data.Y_Move;
   end Get_Y_Move_Bar;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Linguistic_Set_Tree_View;
                Value  : Linguistic_Set
             )  is
      procedure Free is
         new Ada.Unchecked_Deallocation
             (  Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class,
                Gtk_Fuzzy_Linguistic_Set_Tree_View
             );
   begin
      Widget := new Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
      begin
         Initialize (Widget, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   function Image
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Value  : Number
            )  return UTF8_String is
   begin
      return Float_Edit.Image (Value);
   end Image;

   procedure Initialize (Monitor : in out Selection_Monitor) is
   begin
      Monitor.Nested := Monitor.Data.Selection_Pending;
      Monitor.Data.Selection_Pending := True;
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Value  : Linguistic_Set
             )  is
      use Gtk.Cell_Renderer_Text;
      use Gtk.Cell_Renderer_Fuzzy_Boolean;
      Column    : Gtk_Tree_View_Column;
      Column_No : Gint;
      Data_Ptr  : constant Fuzzy_Linguistic_Set_Tree_View_Data_Ptr :=
                     new Fuzzy_Linguistic_Set_Tree_View_Data;
      Data      : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                     Data_Ptr.all;
   begin
      Widget.Data := Ref (Data_Ptr);
      Data.Widget := Widget.all'Unchecked_Access;
      Gtk.Scrolled_Window.Initialize (Widget);
      declare
         Set : Gtk_Fuzzy_Linguistic_Set_Domain;
      begin
         Gtk_New (Set, Value);
         Data.Set := Ref (Set);
      end;
      Gtk_Scrolled_Window_Record'Class (Widget.all).Set_Policy
      (  Policy_Automatic,
         Policy_Automatic
      );
      Data.Tree := new Gtk_Tree_View_Record;
      G_New (Data.Tree, Get_Type);
      Gtk.Tree_View.Initialize (Data.Tree);
      Style_Callbacks.Connect
      (  Data.Tree,
         "style-updated",
         Style_Updated'Access,
         Data_Ptr
      );

      Data.Tree.Set_Rules_Hint (True);
      Data.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Widget.Add (Data.Tree);
      Gtk_New
      (  Data.Store,
         (  GType_String,         -- Names / values
            GType_Fuzzy_Boolean,  -- Range
            GType_Confidence,     -- Left limit
            GType_Confidence,     -- Right limit
            GType_String          -- The color of the names
      )  );
      Data.Tree.Set_Model (To_Interface (Data.Store));
      Data.Store.Unref;

      Gtk_New (Column);
      Gtk_New (Data.Domain);
      Column.Pack_Start (Data.Domain, True);
      Column.Add_Attribute (Data.Domain, "text", 0);
      Column.Add_Attribute (Data.Domain, "foreground", 4);
      Column_No := Data.Tree.Append_Column (Column);
      Column.Set_Resizable (True);
      Column.Set_Expand (True);
      Edited_Handlers.Connect
      (  Data.Domain,
         "edited",
         Edited'Access,
         Widget.all'Access
      );

      Gtk_New (Column);
      Gtk_New (Data.Span);
      Data.Span.Set_Mode (Cell_Renderer_Mode_Inert);
      Column.Pack_Start (Data.Span, True);
      Column.Add_Attribute (Data.Span, "fuzzy-boolean-value", 1);
      Column_No := Data.Tree.Append_Column (Column);
      Column.Set_Sizing (Tree_View_Column_Fixed);
      Column.Set_Resizable (True);
      Column.Set_Fixed_Width (60);
      Commit_Handlers.Connect
      (  Data.Span,
         "commit",
         Commit_Span_Edit'Access,
         Widget.all'Access
      );

      Gtk_New (Column);
      Gtk_New (Data.Left);
      Data.Left.Set_Mode (Cell_Renderer_Mode_Inert);
      Column.Pack_Start (Data.Left, True);
      Column.Add_Attribute (Data.Left, "confidence-value", 2);
      Column_No := Data.Tree.Append_Column (Column);
      Column.Set_Sizing (Tree_View_Column_Fixed);
      Column.Set_Resizable (True);
      Column.Set_Fixed_Width (60);
      Commit_Handlers.Connect
      (  Data.Left,
         "commit",
         Commit_Left_Edit'Access,
         Widget.all'Access
      );

      Gtk_New (Column);
      Gtk_New (Data.Right);
      Data.Right.Set_Mode (Cell_Renderer_Mode_Inert);
      Column.Pack_Start (Data.Right, True);
      Column.Add_Attribute (Data.Right, "confidence-value", 3);
      Column_No := Data.Tree.Append_Column (Column);
      Column.Set_Sizing (Tree_View_Column_Fixed);
      Column.Set_Resizable (True);
      Column.Set_Fixed_Width (60);
      Commit_Handlers.Connect
      (  Data.Right,
         "commit",
         Commit_Right_Edit'Access,
         Widget.all'Access
      );
      Widget.Reset (Value);
      Selection_Callbacks.Connect
      (  Data.Tree.Get_Selection,
         "changed",
         Selection_Changed'Access,
         Widget.all'Access
      );
      Key_Release_Callbacks.Connect
      (  Data.Tree,
         "key_release_event",
         Key_Release_Callbacks.To_Marshaller (Key_Release'Access),
         Data_Ptr,
         After => True
      );
      Set
      (  Data.Set_Style,
         Domain_Callbacks.Connect
         (  Data.Set.Get.all'Unchecked_Access, -- Compiler bug work-
            "style_updated",                   -- around
            Domain_Callbacks.
               To_Marshaller (Domain_Style_Updated'Access),
            Data_Ptr
      )  );
      Set
      (  Data.Marked,
         Domain_Callbacks.Connect
         (  Data.Set.Get.all'Unchecked_Access, -- Compiler bug work-
            "marked",                          -- around
            Domain_Callbacks.To_Marshaller (Domain_Marked'Access),
            Data_Ptr
      )  );
      Data.Tree.Show;
      Style_Updated (Data.Tree, Data_Ptr);
      Domain_Style_Updated (Data.Set.Get, Data_Ptr);
   end Initialize;

   procedure Initialize
             (  Button : not null access
                   Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record'Class
             )  is
      Combo  : Gtk_Combo_Box_Text;
      Label  : Gtk_Entry;
      Do_And : And_Operation;
      Do_Not : Not_Operation;
      Do_Or  : Or_Operation;
      Do_Xor : Xor_Operation;
      Op     : Operation_Ref;
   begin
      Edit_Exec_Buttons.Initialize (Button);
      Set (Op, Do_And); Button.Set.Add ("and", Op);
      Set (Op, Do_Not); Button.Set.Add ("not", Op);
      Set (Op, Do_Or);  Button.Set.Add ("or",  Op);
      Set (Op, Do_Xor); Button.Set.Add ("xor", Op);
      Gtk_New (Combo);
      Button.Combo := Ref (Combo);
      if Combo.Get_Has_Entry then
         Label :=
            Gtk_Entry_Record'Class
            (  Combo.Get_Child.all
            ) 'Unchecked_Access;
         Label.Set_Editable (False);
         Label.Set_Width_Chars (4);
         if Find_Property (Label, "max-width-chars") /= null then
            Set_Property
            (  Label,
               Build ("max-width-chars"),
               GInt'(4)
            );
         end if;
         Label.Set_Activates_Default (False);
      end if;
      Button.Update_Combo;
   end Initialize;

   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive
             )  is
      Var : Variable;
   begin
      Data.Insert (Selected, Var_No, Var);
   end Insert;

   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive;
                Var      : Variable
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Row     : Gtk_Tree_Iter := Null_Iter;
      Monitor : Selection_Monitor (Data'Access);
   begin
      Drawing.Insert (Var_No, Var);
      Data.Store.Insert (Row, Null_Iter, GInt (Var_No) - 1);
      Data.Undo_Stack.Push
      (  (Delete, True, Variable_Index (Var_No))
      );
      Data.Undo_Stack.Push ((Reselect, False, Selected));
      -- Changing the tree
      Data.Widget.Update
      (  Data.Store,
         Data.Get_Row (Var_No),
         Var,
         Data.Checked.Ptr.Text
      );
      -- Selecting the row added
      Data.Tree.Get_Selection.Unselect_All;
      Scroll (Data.Tree, Row);
      Data.Tree.Get_Selection.Select_Iter (Row);
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (True);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Insert")
         )  );
   end Insert;

   procedure Insert
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable
             )  is
      Data     : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                    Widget.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Selected : aliased Selection_State;
   begin
      if Index > Drawing.Cardinality + 1 then
         raise Constraint_Error;
      end if;
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      Data.Insert (Selected, Index, Value);
      Data.Set_Name (Index, Name);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Insert")
         )  );
   end Insert;

   procedure Insert
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State;
                Var_No   : Positive;
                Position : Natural
             )  is
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Parent   : Gtk_Tree_Iter;
      Row      : Gtk_Tree_Iter;
      Point_No : Positive;
      Var      : Variable := Drawing.Set.Get (Var_No);
      Size     : constant Natural := Get_Points_Number (Var);
      X        : Number;
      Y        : Confidence := Confidence'Last;
      Monitor  : Selection_Monitor (Data'Access);
   begin
      Data.Set.Get.Hide_Accumulated;
      if Position in 0..Get_Points_Number (Var) then
         Point_No := Position + 1;
      else
         Point_No := Get_Points_Number (Var) + 1;
      end if;
      if Size = 0 then
         -- Empty variable so far
         if 0.0 in Number'Range then
            X := 0.0;
         else
            X := Number'First;
         end if;
      elsif Point_No = 1 then
         -- The first point to add
         declare
            X_2   : Number'Base;
            Right : Confidence;
            Dummy : array (1..3) of Confidence;
         begin
            Get_Point
            (  Var,
               Point_No,
               X_2,
               Right,
               Dummy (1),
               Dummy (2),
               Dummy (3)
            );
            if Right - Confidence'First < 0.3 then
               Y := Confidence'Last;
            else
               Y := Confidence'First;
            end if;
            X := X_2 - 1.0;
         exception
            when Constraint_Error =>
               if X_2 <= Number'First then
                  return;
               end if;
               X := Number'First;
         end;
      elsif Point_No = Size + 1 then
         -- The last point to add
         declare
            X_1   : Number'Base;
            Left  : Confidence;
            Dummy : array (1..3) of Confidence;
         begin
            Get_Point
            (  Var,
               Point_No - 1,
               X_1,
               Dummy (1),
               Dummy (2),
               Dummy (3),
               Left
            );
            if Left - Confidence'First < 0.3 then
               Y := Confidence'Last;
            else
               Y := Confidence'First;
            end if;
            X := X_1 + 1.0;
         exception
            when Constraint_Error =>
               if X_1 >= Number'Last then
                  return;
               end if;
               X := Number'Last;
         end;
      else
         declare
            X_1, X_2    : Number'Base;
            Left, Right : Confidence;
            Dummy       : array (1..3) of Confidence;
         begin
            Get_Point
            (  Var,
               Point_No - 1,
               X_1,
               Right,
               Dummy (1),
               Dummy (2),
               Dummy (3)
            );
            Get_Point
            (  Var,
               Point_No,
               X_2,
               Dummy (1),
               Dummy (2),
               Dummy (3),
               Left
            );
            X := (X_1 + X_2) / 2.0;
            Y := Confidence (Float (Left + Right) / 3.0);
            if Y - Left < 0.3 or else Y - Right < 0.3 then
               Y := Confidence'Last;
               if Y - Left < 0.3 or else Y - Right < 0.3 then
                  Y := Confidence'First;
               end if;
            end if;
         end;
      end if;
      -- Dealing with the undo stack
      Data.Undo_Stack.Push
      (  (Replace, True, Variable_Index (Var_No), Var)
      );
      Data.Undo_Stack.Push ((Reselect, False, Selected));
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (True);
      end if;
      -- Changing the variable
      Insert_Point (Var, Point_No, X, Y, False);
      Put (Drawing.Set, Var_No, Var);
      Data.Set.Get.Data.Ptr.Updated := True;
      -- Changing the tree
      Parent := Data.Get_Row (Var_No);
      Data.Widget.Update
      (  Data.Store,
         Parent,
         Var,
         Data.Checked.Ptr.Text
      );
      -- Selecting the row added
      Data.Tree.Get_Selection.Unselect_All;
      Expand (Data.Tree, Parent);
      Row := Data.Store.Nth_Child (Parent, GInt (Point_No - 1));
      Scroll (Data.Tree, Row);
      Data.Tree.Get_Selection.Select_Iter (Row);
      Scroll (Data.Set.Get, (X, X), (Y, Y));
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Insert")
         )  );
   end Insert;

   function Is_Editable
            (  Data : Fuzzy_Linguistic_Set_Tree_View_Data
            )  return Boolean is
   begin
      return Get_Mode (Data.Span) /= Cell_Renderer_Mode_Inert;
   end Is_Editable;

   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
            )  return Boolean is
   begin
      return Widget.Data.Ptr.Is_Editable;
   end Is_Editable;

   function Is_Expanded
            (  Data  : Fuzzy_Linguistic_Set_Tree_View_Data;
               Index : Positive
            )  return Boolean is
   begin
      return Is_Expanded (Data.Tree, Data.Get_Row (Index));
   end Is_Expanded;

   function Key_Release
            (  Widget : access Gtk_Tree_View_Record'Class;
               Event  : Gdk_Event;
               Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
            )  return Boolean is
   begin
      case Get_Key_Val (Event) is
         when GDK_Insert =>
            if Data.Is_Editable then
               declare
                  Drawing  : Linguistic_Set_Data'Class renames
                                Data.Set.Get.Data.Ptr.all;
                  Selected : aliased Selection_State;
               begin
                  Selection_Browsing.Selected_Foreach
                  (  Data.Tree.Get_Selection,
                     On_Selected'Access,
                     Selected'Unchecked_Access
                  );
                  declare
                     Current : constant Selection_Subtype :=
                               Drawing.Get_Selection (Selected.Points);
                  begin
                     case Current.Mode is
                        when Single_Variable =>
                           if Get_Column (Data.Tree, 1).Get_Visible
                           then -- Insert new point
                              Data.Insert
                              (  Selected,
                                 Positive (Current.Variable),
                                 0
                              );
                           else -- Insert new variable
                              Data.Insert
                              (  Selected,
                                 Positive (Current.Variable + 1)
                              );
                           end if;
                        when Points_Range | All_Points =>
                           if Get_Column (Data.Tree, 1).Get_Visible
                           then -- Insert new point
                              Data.Insert
                              (  Selected,
                                 Positive (Current.Range_At),
                                 Positive (Current.To_Point)
                              );
                           else -- Insert new variable
                              Data.Insert
                              (  Selected,
                                 Positive (Current.Range_At + 1)
                              );
                           end if;
                        when Variables_Range =>
                           Data.Insert
                           (  Selected,
                              Positive (Current.To_Variable + 1)
                           );
                        when Empty =>
                           Data.Insert
                           (  Selected,
                              Drawing.Cardinality + 1
                           );
                        when others =>
                           null;
                     end case;
                  end;
               end;
            end if;
         when GDK_Delete =>
            if Data.Is_Editable then
               Remove (Data.all);
            end if;
         when others =>
            null;
      end case;
      return False;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Key_Release")
         )  );
         return False;
   end Key_Release;

   procedure Move
             (  Data      : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Variables : Selection;
                By        : Integer
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
   begin
      if By = 0 then
         return;
      end if;
      declare
         Var_No  : Positive;
         Monitor : Selection_Monitor (Data'Access);
      begin
         Data.Tree.Get_Selection.Unselect_All;
         if By > 0 then
            for Index in reverse 1..Variables.Get_Size loop
               Var_No := Positive (Variables.Get_Key (Index));
               if Var_No + By <= Drawing.Cardinality then
                  Drawing.Move (Var_No, Var_No + By);
                  Data.Store.Move_After
                  (  Data.Get_Row (Var_No),
                     Data.Get_Row (Var_No + By)
                  );
               end if;
            end loop;
         else
            for Index in 1..Variables.Get_Size loop
               Var_No := Positive (Variables.Get_Key (Index));
               if Var_No <= Drawing.Cardinality then
                  Drawing.Move (Var_No, Var_No + By);
                  Data.Store.Move_Before
                  (  Data.Get_Row (Var_No),
                     Data.Get_Row (Var_No + By)
                  );
               end if;
            end loop;
         end if;
         for Index in 1..Variables.Get_Size loop
            Var_No := Positive (Variables.Get_Key (Index)) + By;
            if (  Var_No - By <= Drawing.Cardinality
               and then
                  Var_No <= Drawing.Cardinality
               )
            then
               Data.Tree.Get_Selection.Select_Iter
               (  Data.Get_Row (Var_No)
               );
            end if;
         end loop;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Move")
         )  );
   end Move;

   procedure Purge
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection_State
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      First   : Boolean := True;
      Var_No  : Positive;
   begin
      for Index in 1..Selected.Points.Get_Size loop
         Var_No := Positive (Selected.Points.Get_Key (Index));
         if Var_No <= Drawing.Cardinality then
            declare
               Var : Variable := Drawing.Set.Get (Var_No);
            begin
               Push
               (  Data.Undo_Stack,
                  (Replace, First, Variable_Index (Var_No), Var)
               );
               First := False;
               Purge (Var);
               Put (Drawing.Set, Var_No, Var);
            end;
         end if;
      end loop;
      if not First then
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         --
         -- We don't need to update the domain area  because  it  should
         -- have retained its look after purging.
         --
         for Index in 1..Selected.Points.Get_Size loop
            Var_No := Positive (Selected.Points.Get_Key (Index));
            if Var_No <= Drawing.Cardinality then
               Update
               (  Data.Widget,
                  Data.Store,
                  Data.Get_Row (Var_No),
                  Drawing.Set.Get (Var_No),
                  Data.Checked.Ptr.Text
               );
            end if;
         end loop;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Purge")
         )  );
   end Purge;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Domain      : Accumulation_Domain;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
         Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text : Output renames
         Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      Put (Text, Index, Get_Name (Domain.Data.all, From));
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Value  : Variable
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
   begin
      if Drawing.Cardinality < Index then
         raise Constraint_Error;
      else
         Drawing.Hide_Accumulated;
         Remove (Drawing.Selected, Variable_Index (Index));
         Put (Drawing.Set, Index, Value);
         Drawing.Updated := True;
         Update
         (  Widget,
            Data.Store,
            Data.Get_Row (Index),
            Value,
            Data.Checked.Ptr.Text
         );
         Data.Redo_Stack.Erase;
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (False);
         end if;
         Erase (Data.Undo_Stack);
         if Data.Undo /= null then
            Data.Undo.Set_Sensitive (True);
         end if;
         Data.Set.Get.Refresh;
      end if;
   end Put;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Value  : Linguistic_Set
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Monitor : Selection_Monitor (Data'Access);
      Row     : Gtk_Tree_Iter := Null_Iter;
   begin
      Data.Store.Ref;
      Data.Tree.Set_Model (Null_Gtk_Tree_Model);
      Data.Store.Clear;
      for Index in reverse 1..Drawing.Cardinality loop
         Drawing.Remove (Index);
      end loop;
      for Index in 1..Get_Cardinality (Value) loop
         Drawing.Insert (Index, Get (Value, Index));
         Data.Store.Insert (Row, Null_Iter, GInt (Index) - 1);
         Data.Set_Name (Index, Get_Name (Value, Index));
         Data.Widget.Update
         (  Data.Store,
            Data.Get_Row (Index),
            Get (Value, Index),
            Data.Checked.Ptr.Text
         );
      end loop;
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      Data.Undo_Stack.Erase;
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (False);
      end if;
      Data.Tree.Set_Model (To_Interface (Data.Store));
      Data.Store.Unref;
      Drawing.Rescale_X (Data.Set.Get.Get_X_Adjustment);
      Data.Set.Get.Refresh;
   end Put;

   procedure Redo
             (  Data  : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Item  : Edit_Undo_Item;
                First : in out Boolean;
                Stack : in out Edit_Stacks.Stack
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Set : Unbounded_Arrays.Unbounded_Array renames Drawing.Set;
   begin
      case Item.Action is
         when Noop =>
            null;
         when Slide_X =>
            if Data.X_Move /= null then
               declare
                  Save : constant Boolean := Data.X_Move.Modified;
               begin
                  Data.X_Move.Modified := True;
                  Stack.Push ((Slide_X, First, Data.X_Move.Page.From));
                  First := False;
                  Data.X_Move.Page.From := Item.Shift_X;
                  Set_Value
                  (  Data.X_Move.Get_Adjustment,
                     GDouble (Item.Shift_X)
                  );
                  Data.X_Move.Modified := Save;
               end;
            end if;
         when Slide_Y =>
            if Data.Y_Move /= null then
               declare
                  Save : constant Boolean := Data.Y_Move.Modified;
               begin
                  Data.Y_Move.Modified := True;
                  Stack.Push ((Slide_Y, First, Data.Y_Move.Max));
                  First := False;
                  Data.Y_Move.Max := Item.Shift_Y;
                  Set_Value
                  (  Data.Y_Move.Get_Adjustment,
                     GDouble (Confidence'Last) - GDouble (Item.Shift_Y)
                  );
                  Data.Y_Move.Modified := Save;
               end;
            end if;
         when Rename =>
            Stack.Push
            (  (  Rename,
                  First,
                  Item.Rename_At,
                  Create
                  (  Get
                     (  Data.Store,
                        Data.Get_Row (Positive (Item.Rename_At))
            )  )  )  );
            First := False;
            Data.Set_Name
            (  Positive (Item.Rename_At),
               Item.Renamed.Ptr.Text
            );
         when Delete =>
            if Integer (Item.Delete_At) in 1..Drawing.Cardinality then
               declare
                  Old : constant Variable :=
                           Set.Get (Positive (Item.Delete_At));
                  Row : Gtk_Tree_Iter :=
                           Data.Get_Row (Positive (Item.Delete_At));
               begin
                  Push
                  (  Stack,
                     (  Insert,
                        First,
                        Item.Delete_At,
                        Old,
                        Create (Get (Data.Store, Row)),
                        Is_Expanded (Data.Tree, Row)
                  )  );
                  First := False;
                  Drawing.Remove (Positive (Item.Delete_At));
                  Data.Store.Remove (Row);
               end;
            end if;
         when Move =>
            Move (Data, Item.Moved, Item.Move_By);
            Push
            (  Stack,
               (Move, First, Drawing.Selected, -Item.Move_By)
            );
            First := False;
         when Replace =>
            if Integer (Item.Replace_At) in 1..Drawing.Cardinality then
               declare
                  Color : String renames Data.Checked.Ptr.Text;
                  Old   : constant Variable :=
                             Get (Set, Positive (Item.Replace_At));
               begin
                  Put (Set, Positive (Item.Replace_At), Item.Replaced);
                  Update
                  (  Data.Widget,
                     Data.Store,
                     Data.Get_Row (Positive (Item.Replace_At)),
                     Get (Set, Positive (Item.Replace_At)),
                     Color
                  );
                  Push (Stack, (Replace, First, Item.Replace_At, Old));
                  First := False;
               end;
            end if;
         when Insert =>
            declare
               Row : Gtk_Tree_Iter := Null_Iter;
            begin
               Insert
               (  Drawing,
                  Positive (Item.Insert_At),
                  Item.Inserted
               );
               Insert
               (  Data.Store,
                  Row,
                  Null_Iter,
                  GInt (Item.Insert_At - 1)
               );
               Set_Name
               (  Data,
                  Positive (Item.Insert_At),
                  Ptr (Item.Name).Text
               );
               Update
               (  Data.Widget,
                  Data.Store,
                  Row,
                  Get (Set, Positive (Item.Insert_At)),
                  Data.Checked.Ptr.Text
               );
               if Item.Expand then
                  Expand (Data.Tree, Row);
               end if;
               Push (Stack, (Delete, First, Item.Insert_At));
               First := False;
            end;
         when Reselect =>
            declare
               Selected : aliased Selection_State;
            begin
               Selection_Browsing.Selected_Foreach
               (  Data.Tree.Get_Selection,
                  On_Selected'Access,
                  Selected'Unchecked_Access
               );
               Push (Stack, (Reselect, First, Selected));
               First := False;
               Set_Selection
               (  Data,
                  Item.Selected.Points,
                  Item.Selected.Variables
               );
            end;
      end case;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Redo")
         )  );
   end Redo;

   function Ref (Object : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr)
      return Edit_Data_Handle is
   begin
      return (Edit_Data_Handles.Ref (Object) with Undo_Button);
   end Ref;

   procedure Remove
             (  Data : in out Fuzzy_Linguistic_Set_Tree_View_Data
             )  is
      Selected : aliased Selection_State;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Var_No   : Positive;
      Row      : Gtk_Tree_Iter;
      First    : Boolean := True;
   begin
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      for Item in reverse 1..Selected.Points.Get_Size loop
         Var_No := Positive (Selected.Points.Get_Key (Item));
         Row    := Data.Get_Row (Var_No);
         if Var_No in 1..Drawing.Cardinality then
            if Data.Undo_Stack.Is_Empty then
               if Data.Undo /= null then
                  Data.Undo.Set_Sensitive (True);
               end if;
            end if;
            if First then
               Data.Undo_Stack.Push ((Reselect, First, Selected));
               First := False;
            end if;
            declare
               Var    : Variable := Drawing.Set.Get (Var_No);
               Points : constant Points_Indices.Set :=
                           Selected.Points.Get (Item);
               Point  : Point_Index;
            begin
               if (  Points.Is_Empty
                  or else
                     Points.Get_Size >= Get_Points_Number (Var)
                  )
               then
                  -- Deleting this variable
                  Data.Undo_Stack.Push
                  (  (  Insert,
                        False,
                        Variable_Index (Var_No),
                        Var,
                        Create (Get (Data.Store, Row)),
                        Is_Expanded (Data.Tree, Row)
                  )  );
                  Drawing.Remove (Var_No);
                  Data.Store.Remove (Row);
               else
                  -- Deleting points
                  Push
                  (  Data.Undo_Stack,
                     (Replace, False, Variable_Index (Var_No), Var)
                  );
                  First := False;
                  for No in reverse 1..Points.Get_Size loop
                     Point := Get (Points, No);
                     if Natural (Point) in 1..Get_Points_Number (Var) then
                        Remove_Point (Var, Natural (Point), False);
                     end if;
                  end loop;
                  Drawing.Set.Put (Var_No, Var);
                  Data.Widget.Update
                  (  Data.Store,
                     Row,
                     Var,
                     Data.Checked.Ptr.Text
                  );
               end if;
            end;
         else
            Data.Store.Remove (Row);
         end if;
      end loop;
      if not First then
         if Data.Redo /= null then
            Data.Redo.Set_Sensitive (True);
         end if;
         Drawing.Selected.Erase;
         Data.Set.Get.Data.Ptr.Updated := True;
         Data.Set.Get.Refresh;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove")
         )  );
   end Remove;

   procedure Remove
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive
             )  is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
   begin
      if Index > Drawing.Cardinality then
         return;
      end if;
      declare
         First : constant Boolean := True;
         Row   : Gtk_Tree_Iter := Data.Get_Row (Index);
      begin
         Data.Set.Get.Hide_Accumulated;
         Data.Undo_Stack.Push
         (  (  Insert,
               False,
               Variable_Index (Index),
               Drawing.Set.Get (Index),
               Create (Get (Data.Store, Row)),
               Is_Expanded (Data.Tree, Row)
         )  );
         Drawing.Remove (Index);
         Data.Store.Remove (Row);
         if not First then
            if Data.Redo /= null then
               Data.Redo.Set_Sensitive (True);
            end if;
            Drawing.Selected.Erase;
            Data.Set.Get.Data.Ptr.Updated := True;
            Data.Set.Get.Refresh;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove")
         )  );
   end Remove;

   procedure Remove
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record;
                Name   : UTF8_String
             )  is
   begin
      if Button.Set.IsIn (Name) then
         Button.Set.Delete (Name);
         Button.Update_Combo;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Remove")
         )  );
   end Remove;

   procedure Replace
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String;
                Value  : Variable
             )  is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      First   : Boolean := True;
   begin
      if Index > Drawing.Cardinality then
         raise Constraint_Error;
      end if;
      Data.Set.Get.Hide_Accumulated;
      Data.Undo_Stack.Push
      (  (  Replace,
            First,
            Variable_Index (Index),
            Drawing.Set.Get (Index)
      )  );
      First := False;
      Data.Undo_Stack.Push
      (  (  Rename,
            First,
            Variable_Index (Index),
            Create (Data.Get_Name (Index))
      )  );
      Drawing.Set.Put (Index, Value);
      Data.Set_Name (Index, Name);
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (True);
      end if;
      Data.Set.Get.Data.Ptr.Updated := True;
      Data.Set.Get.Refresh;
      Data.Widget.Update
      (  Data.Store,
         Data.Get_Row (Index),
         Value,
         Data.Checked.Ptr.Text
       );
   end Replace;

   procedure Reset
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Set : Linguistic_Set
             )  is
      Data  : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                 Widget.Data.Ptr.all;
      Tree  : constant Gtk_Tree_View  := Data.Tree;
      Store : constant Gtk_Tree_Store := Data.Store;
   begin
      Store.Ref;
      Tree.Set_Model (Null_Gtk_Tree_Model);
      Store.Clear;
      declare
         Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         Parent  : Gtk_Tree_Iter := Null_Iter;
      begin
         for No in 1..Get_Cardinality (Set) loop
            Store.Append (Parent, Null_Iter);
            Store.Set (Parent, 0, Get_Name (Set, No));
            Store.Set (Parent, 4, Data.Checked.Ptr.Text);
            declare
               Var   : Variable renames Get (Set, No);
               Row   : Gtk_Tree_Iter := Null_Iter;
               Value : Number'Base;
               Left  : Confidence;
               Min   : Confidence;
               Max   : Confidence;
               Right : Confidence;
               Cell  : GValue;
            begin
               for Point in 1..Get_Points_Number (Var) loop
                  Get_Point
                  (  Var   => Var,
                     Index => Point,
                     Value => Value,
                     Left  => Left,
                     Min   => Min,
                     Max   => Max,
                     Right => Right
                  );
                  Store.Append (Row, Parent);
                  Store.Set (Row, 0, Image (Widget, Value));
                  Init (Cell, GType_Fuzzy_Boolean);
                  GLib.Values.Fuzzy.Logic.Set
                  (  Cell,
                     (Possibility => Max, Necessity => Min)
                  );
                  Store.Set_Value (Row, 1, Cell);
                  Unset (Cell);
                  Init (Cell, GType_Confidence);
                  GLib.Values.Confidence_Factors.Set (Cell, Left);
                  Store.Set_Value (Row, 2, Cell);
                  GLib.Values.Confidence_Factors.Set (Cell, Right);
                  Store.Set_Value (Row, 3, Cell);
                  Unset (Cell);
                  Store.Set (Row, 4, Data.Checked.Ptr.Text);
               end loop;
            end;
         end loop;
      end;
      Tree.Set_Model (To_Interface (Store));
      Unref (Store);
   end Reset;

   procedure Selection_Changed
             (  Widget : access Gtk_Tree_Selection_Record'Class;
                View   : Gtk_Fuzzy_Linguistic_Set_Tree_View
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                View.Data.Ptr.all;
   begin
      if not Data.Selection_Pending then
         declare
            Selected : aliased Selection_State;
         begin
            Selection_Browsing.Selected_Foreach
            (  Widget,
               On_Selected'Access,
               Selected'Unchecked_Access
            );
            View.Data.Ptr.Set.Get.Set_Selection (Selected.Points);
            Data.Set_Buttons (Selected.Points);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Selection_Changed")
         )  );
   end Selection_Changed;

   procedure Select_Duplicated
             (  Data : in out Fuzzy_Linguistic_Set_Tree_View_Data
             )  is
      use Duplication_Tables;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      List     : Duplication_Tables.Table;
      Selected : aliased Selection_State;
      Monitor  : Selection_Monitor (Data'Access);
   begin
      for Index in 1..Drawing.Cardinality loop
         declare
            Name : constant String := Data.Get_Name (Index);
         begin
            begin
               List.Add (Name, False);
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  List.Replace (Name, True);
            end;
         end;
      end loop;
      -- Selecting the duplicated rows
      Selection_Browsing.Selected_Foreach
      (  Data.Tree.Get_Selection,
         On_Selected'Access,
         Selected'Unchecked_Access
      );
      Data.Undo_Stack.Push ((Reselect, True, Selected));
      Data.Tree.Get_Selection.Unselect_All;
      for Index in 1..Drawing.Cardinality loop
         if List.Find (Data.Get_Name (Index)) then
            Data.Tree.Get_Selection.Select_Iter
            (  Data.Get_Row (Index)
            );
         end if;
      end loop;
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (True);
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Select_Duplicated")
         )  );
   end Select_Duplicated;

   procedure Select_Duplicated
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      Select_Duplicated (Data);
   end Select_Duplicated;

   procedure Set (Ref : in out Operation_Ref; Op : Operation'Class) is
   begin
      Free (Ref.Ptr);
      Ref.Ptr := new Operation'Class'(Op);
   end Set;

   procedure Set
             (  Ref    : in out Edit_Data_Handle;
                Object : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
   begin
      Edit_Data_Handles.Set (Edit_Data_Handles.Handle (Ref), Object);
   end Set;

   procedure Set_Adjustment
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Lower  : Number'Base;
                Upper  : Number'Base;
                From   : Number'Base;
                To     : Number'Base
             )  is
      Adjustment : constant Gtk_Adjustment := Widget.Get_Adjustment;
   begin
      if Lower >= Upper then
         raise Constraint_Error with
               "Lower is not less than upper limit";
      end if;
      if Lower /= Widget.Span.From then
         Widget.Span.From := Lower;
         Adjustment.Set_Lower (GDouble (Lower));
         Widget.Modified := True;
      end if;
      if Upper /= Widget.Span.To then
         Widget.Span.To := Upper;
         Adjustment.Set_Upper (GDouble (Upper));
         Widget.Modified := True;
      end if;
      if From /= Widget.Page.From or else To /= Widget.Page.To then
         if From >= To then
            if From >= Widget.Span.To then
               Widget.Page.From :=
                  Widget.Span.To - Length (Widget.Span) / 100.0;
               Widget.Page.To := Widget.Span.To;
            else
               Widget.Page.From := From;
               Widget.Page.To   := From + Length (Widget.Span) / 100.0;
            end if;
         else
            Widget.Page := (From, To);
         end if;
         declare
            Page : constant GDouble :=
                      GDouble'Max
                      (  GDouble (Length (Widget.Page)),
                         GDouble (Length (Widget.Span) / 500.0)
                      );
         begin
            Adjustment.Set_Page_Size (Page);
            Adjustment.Set_Page_Increment (Page * 0.75);
            Adjustment.Set_Step_Increment (Page * 0.01);
            Changed (Adjustment);
         end;
         Widget.Modified := True;
      end if;
      if Adjustment.Get_Value /= GDouble (Widget.Page.From) then
         Widget.Modified := True;
         Adjustment.Set_Value (GDouble (Widget.Page.From));
      end if;
      Widget.Modified := False;
   end Set_Adjustment;

   procedure Set_Adjustment
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record;
                From   : Confidence;
                To     : Confidence
             )  is
      Adjustment : constant Gtk_Adjustment := Widget.Get_Adjustment;
   begin
      if To - From /= Widget.Page or else To /= Widget.Max then
         Widget.Page := To - From;
         Widget.Max  := To;
         Widget.Modified := True;
      end if;
      if Widget.Modified then
         declare
            Page : constant GDouble := GDouble (Widget.Page);
         begin
            Adjustment.Set_Page_Size (Page);
            Adjustment.Set_Page_Increment (Page * 0.75);
         end;
         Changed (Adjustment);
      end if;
      if (  Adjustment.Get_Value
         /= GDouble (Confidence'Last) - GDouble (Widget.Max)
         )
      then
         Widget.Modified := True;
         Adjustment.Set_Value
         (  GDouble (Confidence'Last) - GDouble (Widget.Max)
         );
      end if;
      Widget.Modified := False;
   end Set_Adjustment;

   procedure Set_Buttons
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Selected : Selection
             )  is
   begin
      if Is_Empty (Selected) then
         if Data.Remove /= null then
            Data.Remove.Set_Sensitive (False);
         end if;
         if Data.New_Var /= null then
            Data.New_Var.Set_Sensitive (True);
         end if;
         if Data.Add /= null then
            Data.Add.Set_Sensitive (False);
         end if;
         if Data.Copy /= null then
            Data.Copy.Set_Sensitive (False);
         end if;
         if Data.Down /= null then
            Data.Down.Set_Sensitive (False);
         end if;
         if Data.Exec /= null then
            Data.Exec.Set_Sensitive (False);
         end if;
         if Data.Up /= null then
            Data.Up.Set_Sensitive (False);
         end if;
         if Data.Purge /= null then
            Data.Purge.Set_Sensitive (False);
         end if;
         if Data.X_Move /= null then
            Data.X_Move.Set_Sensitive (False);
         end if;
         if Data.Y_Move /= null then
            Data.Y_Move.Set_Sensitive (False);
         end if;
      else
         declare
            Drawing : Linguistic_Set_Data'Class renames
                         Data.Set.Get.Data.Ptr.all;
            Current : constant Selection_Subtype :=
                         Drawing.Get_Selection (Selected);
         begin
            if Data.New_Var /= null then
               Data.New_Var.Set_Sensitive
               (  Current.Mode = Empty
               or else
                  Current.Mode = Single_Variable
               or else
                  Current.Mode = All_Points
               or else
                  Current.Mode = Variables_Range
               );
            end if;
            if Data.Add /= null then
               Data.Add.Set_Sensitive
               (  Current.Mode = Single_Variable
               or else
                  Current.Mode = All_Points
               or else
                  Current.Mode = Points_Range
               );
            end if;
            if Data.Copy /= null then
               Data.Copy.Set_Sensitive (Current.Mode = Single_Variable);
            end if;
            if Data.Down /= null then
               Data.Down.Set_Sensitive
               (  (  Current.Mode = Single_Variable
                  or else
                     Current.Mode = Variables_Range
                  or else
                      Current.Mode = All_Points
                  or else
                     Current.Mode = Complete_Variables
                  )
               and then
                  (  Drawing.Cardinality
                  >  Positive
                     (  Selected.Get_Key (Selected.Get_Size)
               )  )  );
            end if;
            if Data.Exec /= null then
               begin
                  Data.Exec.Set_Sensitive
                  (  Data.Exec.Set.Find
                     (  Data.Exec.Get_Combo.Get_Active_Text
                     ) .Ptr.Check (Current)
                  );
               exception
                  when Ada.IO_Exceptions.End_Error =>
                     Data.Exec.Set_Sensitive (False);
               end;
            end if;
            if Data.Purge /= null then
               Data.Purge.Set_Sensitive
               (  Current.Mode = Single_Variable
               or else
                  Current.Mode = Variables_Range
               or else
                   Current.Mode = All_Points
               or else
                  Current.Mode = Complete_Variables
               );
            end if;
            if Data.Remove /= null then
               Data.Remove.Set_Sensitive (Current.Mode /= Empty);
            end if;
            if Data.Up /= null then
               Data.Up.Set_Sensitive
               (  (  Current.Mode = Single_Variable
                  or else
                     Current.Mode = Variables_Range
                  or else
                     Current.Mode = All_Points
                  or else
                     Current.Mode = Complete_Variables
                  )
               and then
                  Selected.Get_Key (1) > 1
               );
            end if;
            if Data.X_Move /= null then
               case Current.Mode is
                  when Single_Variable | Variables_Range | All_Points =>
                     Data.X_Move.Set_Sensitive (True);
                     Data.X_Move.Set_Variables (Selected);
                  when Points_Range =>
                     Data.X_Move.Set_Sensitive (True);
                     Data.X_Move.Set_Points
                     (  Current.Range_At,
                        Current.From_Point,
                        Current.To_Point
                     );
                  when others =>
                     Data.X_Move.Set_Sensitive (False);
               end case;
            end if;
            if Data.Y_Move /= null then
               case Current.Mode is
                  when Points_Range | All_Points =>
                     Data.Y_Move.Set_Sensitive (True);
                     Data.Y_Move.Set_Points
                     (  Current.Range_At,
                        Current.From_Point,
                        Current.To_Point
                     );
                  when others =>
                     Data.Y_Move.Set_Sensitive (False);
               end case;
            end if;
         end;
      end if;
   end Set_Buttons;

   procedure Set_Domain_Column_Note
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Note   : UTF8_String
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
   begin
      if Data.Domain_Note.Is_Valid then
         if Data.Domain_Note.Ptr.Text /= Note then
            Data.Domain_Note := Create (Note);
            Data.Set_Domain_Column_Title
            (  Style_Get
               (  Widget.Get_Tree_View,
                  "domain-column-title"
               ),
               Note
            );
         end if;
      else
         if Note'Length /= 0 then
            Data.Domain_Note := Create (Note);
            Data.Set_Domain_Column_Title
            (  Style_Get
               (  Widget.Get_Tree_View,
                  "domain-column-title"
               ),
               Note
            );
         end if;
      end if;
   end Set_Domain_Column_Note;

   procedure Set_Domain_Column_Title
             (  Data   : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Title  : UTF8_String;
                Note   : UTF8_String
             )  is
      Column : constant Gtk_Tree_View_Column :=
               Data.Tree.Get_Column (0);
   begin
      if Title'Length = 0 then
         Column.Set_Title (Note);
      elsif Note'Length = 0 then
         Column.Set_Title (Title);
      else
         Column.Set_Title (Title & ' ' & Note);
      end if;
   end Set_Domain_Column_Title;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         declare
            Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                      Widget.Data.Ptr.all;
         begin
            Set_Property (Data.Domain, Build ("editable"), Editable);
            if Editable then
               Set_Property (Data.Domain, Build ("editable"), True);
               Data.Span.Set_Mode (Cell_Renderer_Mode_Editable);
               Data.Left.Set_Mode (Cell_Renderer_Mode_Editable);
               Data.Right.Set_Mode (Cell_Renderer_Mode_Editable);
               if Data.Undo /= null then
                  Data.Undo.Set_Sensitive
                  (  not Data.Undo_Stack.Is_Empty
                  );
               end if;
               if Data.Redo /= null then
                  Data.Redo.Set_Sensitive
                  (  not Data.Redo_Stack.Is_Empty
                  );
               end if;
               if Data.X_Move /= null then
                  Data.X_Move.Show;
               end if;
               if Data.Y_Move /= null then
                  Data.Y_Move.Show;
               end if;
               declare
                  Selected : aliased Selection_State;
               begin
                  Selection_Browsing.Selected_Foreach
                  (  Data.Tree.Get_Selection,
                     On_Selected'Access,
                     Selected'Unchecked_Access
                  );
                  Set_Buttons (Data, Selected.Points);
               end;
            else
               Set_Property (Data.Domain, Build ("editable"), False);
               Data.Span.Set_Mode (Cell_Renderer_Mode_Inert);
               Data.Left.Set_Mode (Cell_Renderer_Mode_Inert);
               Data.Right.Set_Mode (Cell_Renderer_Mode_Inert);
               if Data.Add /= null then
                  Data.Add.Set_Sensitive (False);
               end if;
               if Data.Copy /= null then
                  Data.Copy.Set_Sensitive (False);
               end if;
               if Data.Down /= null then
                  Data.Down.Set_Sensitive (False);
               end if;
               if Data.Exec /= null then
                  Data.Exec.Set_Sensitive (False);
               end if;
               if Data.New_Var /= null then
                  Data.New_Var.Set_Sensitive (False);
               end if;
               if Data.Purge /= null then
                  Data.Purge.Set_Sensitive (False);
               end if;
               if Data.Redo /= null then
                  Data.Redo.Set_Sensitive (False);
               end if;
               if Data.Remove /= null then
                  Data.Remove.Set_Sensitive (False);
               end if;
               if Data.Up /= null then
                  Data.Up.Set_Sensitive (False);
               end if;
               if Data.Undo /= null then
                  Data.Undo.Set_Sensitive (False);
               end if;
               if Data.X_Move /= null then
                  Data.X_Move.Set_Sensitive (False);
                  Hide (Data.X_Move);
               end if;
               if Data.Y_Move /= null then
                  Data.Y_Move.Set_Sensitive (False);
                  Hide (Data.Y_Move);
               end if;
            end if;
         end;
      end if;
   end Set_Editable;

   procedure Set_Name
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Index  : Positive;
                Name   : UTF8_String
             )  is
      Data : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                Data.Set.Get.Data.Ptr.all;
   begin
      if Drawing.Cardinality < Index then
         raise Constraint_Error;
      else
         Data.Set_Name (Index, Name);
      end if;
   end Set_Name;

   procedure Set_Name
             (  Data  : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Index : Positive;
                Name  : String
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
   begin
      if Index in 1..Drawing.Cardinality then
         declare
            This : constant Gtk_Tree_Iter := Data.Get_Row (Index);
         begin
            if This /= Null_Iter then
               begin
                  Name_Tables.Check_Name (Name);
                  Data.Store.Set
                  (  This,
                     4,
                     Data.Checked.Ptr.Text
                  );
               exception
                  when Constraint_Error =>
                     Data.Store.Set
                     (  This,
                        4,
                        Data.Error.Ptr.Text
                     );
               end;
               Data.Store.Set (This, 0, Name);
            end if;
         end;
      end if;
   end Set_Name;

   procedure Set_Points
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Var_No : Variable_Index;
                From   : Point_Index;
                To     : Point_Index
             )  is
   begin
      if Widget.Modified then
         return;
      end if;
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.Set.Get.Data.Ptr.all;
         Var : Variable;
      begin
         Var := Drawing.Set.Get (Positive (Var_No));
         if From > 1 then
            if Positive (To) < Get_Points_Number (Var) then
               Widget.Set_Adjustment
               (  Get_Value (Var, Positive (From - 1)),
                  Get_Value (Var, Positive (To + 1)),
                  Get_Value (Var, Positive (From)),
                  Get_Value (Var, Positive (To))
               );
            else
               Widget.Set_Adjustment
               (  Get_Value (Var, Positive (From - 1)),
                  Drawing.X_Range.To,
                  Get_Value (Var, Positive (From)),
                  Get_Value (Var, Positive (To))
               );
            end if;
         else
            if Positive (To) < Get_Points_Number (Var) then
               Widget.Set_Adjustment
               (  Drawing.X_Range.From,
                  Get_Value (Var, Positive (To + 1)),
                  Get_Value (Var, Positive (From)),
                  Get_Value (Var, Positive (To))
               );
            else
               Widget.Set_Adjustment
               (  Drawing.X_Range.From,
                  Drawing.X_Range.To,
                  Get_Value (Var, Positive (From)),
                  Get_Value (Var, Positive (To))
               );
            end if;
         end if;
      end;
   exception
      when Constraint_Error =>
         Widget.Set_Sensitive (False);
   end Set_Points;

   procedure Set_Points
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Y_Record;
                Var_No : Variable_Index;
                From   : Point_Index;
                To     : Point_Index
             )  is
   begin
      if Widget.Modified then
         return;
      end if;
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.Set.Get.Data.Ptr.all;
         Inf     : Confidence := Confidence'Last;
         Sup     : Confidence := Confidence'First;
         Left    : Confidence;
         Min     : Confidence;
         Max     : Confidence;
         Right   : Confidence;
         Value   : Number'Base;
         Var     : Variable renames Drawing.Set.Get (Positive (Var_No));
      begin
         Widget.Var_No := Positive (Var_No);
         Widget.Last_Point :=
            Positive'Min (Positive (To), Get_Points_Number (Var));
         Widget.First_Point :=
            Positive'Min (Positive (From), Widget.Last_Point);
         -- Determine the range of truth values
         for Index in Widget.First_Point..Widget.Last_Point loop
            Get_Point (Var, Index, Value, Left, Min, Max, Right);
            Inf := Confidence'Min (Inf, Min);
            Sup := Confidence'Max (Sup, Max);
         end loop;
         Widget.Set_Adjustment (Inf, Sup);
      end;
   exception
      when Constraint_Error =>
         Widget.Set_Sensitive (False);
   end Set_Points;

   procedure Set_Variables
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Edit_X_Record;
                Selected : Selection
             )  is
   begin
      if Widget.Modified then
         return;
      end if;
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.Set.Get.Data.Ptr.all;
         Span : Interval;
         From : Number'Base := Number'Base'Last;
         To   : Number'Base := Number'Base'First;
      begin
         for Index in 1..Selected.Get_Size loop
            Span :=
               Get_Span
               (  Get
                  (  Drawing.Set,
                     Positive (Selected.Get_Key (Index))
               )  );
            From := Number'Base'Min (From, Span.From);
            To   := Number'Base'Max (To,   Span.To);
         end loop;
         Widget.Set_Adjustment
         (  Drawing.X_Range.From,
            Drawing.X_Range.To,
            From,
            To
         );
      end;
   exception
      when Constraint_Error =>
         Widget.Set_Sensitive (False);
   end Set_Variables;

   procedure Set_Selection
             (  Data      : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Points    : Selection;
                Variables : Points_Indices.Set
             )  is
      Selector : constant Gtk_Tree_Selection := Data.Tree.Get_Selection;
      Parent   : Gtk_Tree_Iter;
      Drawing  : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Monitor  : Selection_Monitor (Data'Access);
   begin
      Selector.Unselect_All;
      for Index in 1..Points.Get_Size loop
         Parent := Data.Get_Row (Positive (Points.Get_Key (Index)));
         if Parent /= Null_Iter then
            if Variables.Is_In
               (  Point_Index (Points.Get_Key (Index))
               )
            then
               Selector.Select_Iter (Parent);
            end if;
            declare
               Row  : Gtk_Tree_Iter;
               List : Points_Indices.Set renames Get (Points, Index);
            begin
               for Point in 1..List.Get_Size loop
                  Row :=
                     Data.Store.Nth_Child
                     (  Parent,
                        GInt (List.Get (Point)) - 1
                     );
                  if Row /= Null_Iter then
                     Selector.Select_Iter (Row);
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Set_Selection;

   procedure Set_Value
             (  Data     : in out Fuzzy_Linguistic_Set_Tree_View_Data;
                Var_No   : Positive;
                Point_No : Positive;
                Value    : Number
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                    Data.Set.Get.Data.Ptr.all;
      Var     : Variable := Drawing.Set.Get (Var_No);
      Left    : Confidence;
      Right   : Confidence;
      Min     : Confidence;
      Max     : Confidence;
   begin
      Drawing.Hide_Accumulated;
      declare
         X : Number;
      begin
         Get_Point (Var, Point_No, X, Left, Min, Max, Right);
      end;
      Move_Point (Var, Point_No, Value, False);
      Data.Redo_Stack.Erase;
      if Data.Redo /= null then
         Data.Redo.Set_Sensitive (False);
      end if;
      Data.Undo_Stack.Push
      (  (  Replace,
            True,
            Variable_Index (Var_No),
            Drawing.Set.Get (Var_No)
      )  );
      if Data.Undo /= null then
         Data.Undo.Set_Sensitive (True);
      end if;
      Drawing.Set.Put (Var_No, Var);
      Data.Set.Get.Data.Ptr.Updated := True;
      Data.Widget.Update
      (  Data.Store,
         Data.Get_Row (Var_No),
         Var,
         Data.Checked.Ptr.Text
      );
      Scroll
      (  Data.Set.Get,
         (Value, Value),
         (Possibility => Max, Necessity => Min)
      );
   end Set_Value;

   procedure Style_Updated
             (  Widget : access Gtk_Tree_View_Record'Class;
                Data   : Fuzzy_Linguistic_Set_Tree_View_Data_Ptr
             )  is
      procedure Change_Color
                (  Old_Color : in out Text_Handles.Handle;
                   New_Color : String
                )  is
      begin
         declare
            Color : String renames Old_Color.Ptr.Text;
            Row   : Gtk_Tree_Iter;
            Root  : Boolean := True;
         begin
            if Color = New_Color then
               return;
            end if;
            Row := Data.Store.Get_Iter_First;
            while Row /= Null_Iter loop
               if Get (Data.Store, Row, 4) = Color then
                  Data.Store.Set (Row, 4, New_Color);
               end if;
               if Root and then Data.Store.Has_Child (Row) then
                  Row  := Data.Store.Children (Row);
                  Root := False;
               else
                  Data.Store.Next (Row);
               end if;
               if Row = Null_Iter then
                  Row  := Data.Store.Parent (Row);
                  Root := True;
               end if;
            end loop;
         end;
         Old_Color := Create (New_Color);
      end Change_Color;
   begin
      if Is_Valid (Data.Domain_Note) then
         Data.Set_Domain_Column_Title
         (  Style_Get (Widget, "domain-column-title"),
            Data.Domain_Note.Ptr.Text
         );
      else
         Data.Set_Domain_Column_Title
         (  Style_Get (Widget, "domain-column-title"),
            ""
         );
      end if;
      Widget.Get_Column (1).Set_Title
      (  Style_Get (Widget, "range-column-title")
      );
      Widget.Get_Column (2).Set_Title
      (  Style_Get (Widget, "left-column-title")
      );
      Widget.Get_Column (3).Set_Title
      (  Style_Get (Widget, "right-column-title")
      );
      Change_Color (Data.Error,   Style_Get (Widget, "illegal-color"));
      Change_Color (Data.Checked, Style_Get (Widget, "checked-color"));
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

   procedure Update
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Tree_View_Record'Class;
                Store  : not null access Gtk_Tree_Store_Record'Class;
                Parent : Gtk_Tree_Iter;
                Var    : Variable;
                Color  : String
             )  is
      Row   : Gtk_Tree_Iter;
      Value : Number'Base;
      Left  : Confidence;
      Min   : Confidence;
      Max   : Confidence;
      Right : Confidence;
      Cell  : GValue;
   begin
      for Index in 1..Get_Points_Number (Var) loop
         -- Set the rows corresponding to each of the points
         Get_Point
         (  Var,
            Index,
            Value,
            Left,
            Min,
            Max,
            Right
         );
         Row := Store.Nth_Child (Parent, GInt (Index - 1));
         if Row = Null_Iter then
            -- Insert a new row for this point
            Store.Insert (Row, Parent, GInt (Index - 1));
            Store.Set (Row, 0, Image (Widget, Value));
            Init (Cell, GType_Fuzzy_Boolean);
            Set (Cell, (Possibility => Max, Necessity => Min));
            Store.Set_Value (Row, 1, Cell);
            Unset (Cell);
            Init (Cell, GType_Confidence);
            Set (Cell, Left);
            Store.Set_Value (Row, 2, Cell);
            Set (Cell, Right);
            Store.Set_Value (Row, 3, Cell);
            Unset (Cell);
            Store.Set (Row, 4, Color);
         else
            -- Set the changed fields of the row
            if Get (Store, Row) /= Image (Widget, Value) then
               Store.Set (Row, 0, Image (Widget, Value));
            end if;
            Store.Get_Value (Row, 1, Cell);
            if Get (Cell) /= (Possibility => Max, Necessity => Min) then
               Set (Cell, (Possibility => Max, Necessity => Min));
               Store.Set_Value (Row, 1, Cell);
            end if;
            Unset (Cell);
            Store.Get_Value (Row, 2, Cell);
            if Get (Cell) /= Left then
               Set (Cell, Left);
               Store.Set_Value (Row, 2, Cell);
            end if;
            Unset (Cell);
            Store.Get_Value (Row, 3, Cell);
            if Get (Cell) /= Right then
               Set (Cell, Right);
               Store.Set_Value (Row, 3, Cell);
            end if;
            Unset (Cell);
         end if;
      end loop;
      declare
         To_Remove : constant GInt := GInt (Get_Points_Number (Var));
      begin
         loop
            Row := Store.Nth_Child (Parent, To_Remove);
            exit when Row = Null_Iter;
            Remove (Store, Row);
         end loop;
      end;
   end Update;

   procedure Update
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
                Selected : Selection
             )  is
      Data    : Fuzzy_Linguistic_Set_Tree_View_Data'Class renames
                   Widget.Data.Ptr.all;
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      Set     : Unbounded_Arrays.Unbounded_Array renames Drawing.Set;
      Color   : String renames Data.Checked.Ptr.Text;
      Var_No  : Positive;
   begin
      for Index in 1..Selected.Get_Size loop
         Var_No := Positive (Get_Key (Selected, Index));
         exit when Var_No > Drawing.Cardinality;
         Update
         (  Widget,
            Data.Store,
            Data.Get_Row (Var_No),
            Get (Set, Var_No),
            Color
         );
      end loop;
   end Update;

   procedure Update_Combo
             (  Button : not null access
                         Gtk_Fuzzy_Linguistic_Set_Edit_Exec_Record
             )  is
      Combo : constant not null access
              Gtk_Combo_Box_Text_Record'Class := Button.Combo.Get;
   begin
      Combo.Remove_All;
      for Index in 1..Button.Set.GetSize loop
         Combo.Append_Text (Button.Set.GetName (Index));
      end loop;
      Combo.Set_Active (0);
   end Update_Combo;

   function Value
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Tree_View_Record;
               Source : String
            )  return Number is
   begin
      return Float_Edit.Value (Source);
   end Value;

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View;
