--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Domain.    Luebeck            --
--        Generic_Zoom_Panel                       Winter, 2007       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Gdk.Cairo;                 use Gdk.Cairo;
with Gdk.Types;                 use Gdk.Types;
with GLib.Messages;             use GLib.Messages;
with GLib.Object;               use GLib.Object;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with GtkAda.Types;              use GtkAda.Types;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with Ada.Unchecked_Conversion;
with Confidence_Factors.Edit;
with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;

with Ada.Numerics.Elementary_Functions;
use  Ada.Numerics.Elementary_Functions;

package body Gtk.Generic_Fuzzy_Linguistic_Set_Domain.
             Generic_Zoom_Panel is
   use Float_Edit;
   use Gdk.Window;
   use Gtk.Enums;
   use Zoom_Undo;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return
      (  " in Gtk.Generic_Fuzzy_Linguistic_Set_Domain."
      &  "Generic_Zoom_Panel."
      &  Name
      );
   end Where;

   Zoom_Inc     : constant GDouble := 1.5;
   Upper_Y_Zoom : constant GDouble :=
                     GDouble (log (Float (Zoom_Y_Range'Last)));

   function From_Scale (Value : GDouble) return Zoom_X_Range;
   function From_Scale (Value : GDouble) return Zoom_Y_Range;
   function To_Scale (Value : Zoom_X_Range) return GDouble;
   function To_Scale (Value : Zoom_Y_Range) return GDouble;

   procedure Mouse_Event
             (  Area   : not null access Gtk_Drawing_Area_Record'Class;
                Event  : Gdk_Event;
                Data   : Fuzzy_Linguistic_Set_Zoom_Data_Ptr;
                X      : out X_Axis;
                Y      : out Y_Axis;
                Inside : out Boolean;
                Hint   : Boolean := False
             );

   function Button_Press
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      begin
         if Get_Button (Event) = 1 then
            declare
               X      : X_Axis;
               Y      : Y_Axis;
               Inside : Boolean;
            begin
               Mouse_Event (Area, Event, Data, X, Y, Inside);
               case Drawing.Area_Selection.State is
                  when None =>
                     if Inside then -- Start new selection
                        Drawing.Area_Selection.X1 := X;
                        Drawing.Area_Selection.X2 := X;
                        Drawing.Area_Selection.Y1 := Y;
                        Drawing.Area_Selection.Y2 := Y;
                        declare
                           Context : constant Cairo_Context :=
                                     Create (Area.Get_Window);
                        begin
                           Drawing.Draw_Area_Selection (Context, True);
                           Destroy (Context);
                        exception
                           when others =>
                              Destroy (Context);
                              raise;
                        end;
                        Drawing.Area_Selection.State := Active;
                     end if;
                  when Active =>
                     declare -- Erase selection
                        Context : constant Cairo_Context :=
                                  Create (Area.Get_Window);
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
                  when Inactive =>
                     if Inside then -- Erase old selection and start
                        declare     -- new one
                           Context : constant Cairo_Context :=
                                     Create (Area.Get_Window);
                        begin
                           Drawing.Draw_Area_Selection (Context, False);
                           Drawing.Area_Selection.X1 := X;
                           Drawing.Area_Selection.X2 := X;
                           Drawing.Area_Selection.Y1 := Y;
                           Drawing.Area_Selection.Y2 := Y;
                           Drawing.Draw_Area_Selection (Context, True);
                           Drawing.Draw_Selected (Context);
                           Destroy (Context);
                        exception
                           when others =>
                              Destroy (Context);
                              raise;
                        end;
                        Drawing.Area_Selection.State := Active;
                     end if;
               end case;
            end;
         end if;
      end;
      return True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Button_Press")
         )  );
         return True;
   end Button_Press;

   function Button_Release
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
         X       : X_Axis;
         Y       : Y_Axis;
         Inside  : Boolean;
      begin
         if (  Active = Drawing.Area_Selection.State
            and then
               Get_Button (Event) = 1
            )
         then
            Mouse_Event (Area, Event, Data, X, Y, Inside);
            if (  Drawing.Area_Selection.X2 /= X
               or else
                  Drawing.Area_Selection.Y2 /= Y
               )
            then
               declare
                  Context : constant Cairo_Context :=
                            Create (Area.Get_Window);
               begin
                  Drawing.Draw_Area_Selection (Context, False);
                  Drawing.Area_Selection.X2 := X;
                  Drawing.Area_Selection.Y2 := Y;
                  Drawing.Draw_Area_Selection (Context, True);
                  Drawing.Draw_Selected (Context);
                  Destroy (Context);
               exception
                  when others =>
                     Destroy (Context);
                     raise;
               end;
            end if;
            Drawing.Area_Selection.State := Inactive;
            Data.Set.Get.Marked;
         end if;
      end;
      return True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Button_Release")
         )  );
         return True;
   end Button_Release;

   procedure Commit_Zoom
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             )  is
      Data : Linguistic_Set_Data'Class renames Widget.Data.Ptr.all;
      Area : Drawing_Area renames Data.Area;
   begin
      Data.Scroll_X
      (  From_X (Area, 1),
         From_X (Area, Get_Width (Area)),
         Widget.Get_X_Adjustment
      );
      Data.Scroll_Y
      (  From_Y (Area, 1),
         From_Y (Area, Get_Height (Area)),
         Widget.Get_Y_Adjustment
      );
      Refresh (Widget);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Commit_Zoom")
         )  );
   end Commit_Zoom;

   procedure Finalize (State : in out Zoom_Undo_State) is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                   State.Data.all;
         Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         Area : Drawing_Area renames Drawing.Area;
      begin
         if not State.Inactive then
            if (  State.Item.X_Gain   /= Area.X_Gain
               or else
                  State.Item.X_Offset /= Area.X_Offset
               or else
                  State.Item.Y_Gain   /= Area.Y_Gain
               or else
                  State.Item.Y_Offset /= Area.Y_Offset
               )
            then
               if not Data.Redo_Stack.Is_Empty then
                  Erase (Data.Redo_Stack);
                  if Data.Redo /= null then
                     Data.Redo.Set_Sensitive (False);
                  end if;
               end if;
               if Data.Undo_Stack.Is_Empty then
                  if Data.Undo /= null then
                     Data.Undo.Set_Sensitive (True);
                  end if;
                  Push (Data.Undo_Stack, State.Item);
               elsif (  (  Clock
                        >= Data.Last_Undo + Drawing.Merge_Threshold
                        )
                     and then
                        Top (Data.Undo_Stack) /= State.Item
                     )
               then
                  Push (Data.Undo_Stack, State.Item);
               end if;
               Data.Last_Undo := Clock;
            end if;
            Data.Pending_Zoom := False;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Zoom_Undo_State)")
         )  );
   end Finalize;

   procedure Finalize (Data : in out Fuzzy_Linguistic_Set_Zoom_Data) is
   begin
      if Data.Set.Get.Data.Is_Valid then
         declare
            Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         begin
            Drawing.Zooming := null;
         end;
      end if;
      Standard.Object.Finalize (Standard.Object.Entity (Data));
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Fuzzy_Linguistic_Set_Zoom_Data)")
         )  );
   end Finalize;

   procedure Finalize (Ref : in out Zoom_Data_Handle) is
   begin
      if Is_Valid (Ref) then
         declare
            Data : Fuzzy_Linguistic_Set_Zoom_Data'Class
                      renames Ptr (Ref).all;
         begin
            case Ref.Widget is
               when Undo_Button        => Data.Undo     := null;
               when Redo_Button        => Data.Redo     := null;
               when Zoom_100_Button    => Data.Zoom_100 := null;
               when Zoom_Fit_Button    => Data.Zoom_Fit := null;
               when Zoom_In_Button     => Data.Zoom_In  := null;
               when Zoom_Out_Button    => Data.Zoom_Out := null;
               when Zoom_X_Scale       => Data.Zoom_X   := null;
               when Zoom_Y_Scale       => Data.Zoom_Y   := null;
               when Value_Tracker      => Data.Value    := null;
               when Membership_Tracker => Data.Level    := null;
            end case;
         end;
      end if;
      Finalize (Zoom_Data_Handles.Handle (Ref));
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize (Zoom_Data_Handle)")
         )  );
   end Finalize;

   function From_Scale (Value : GDouble) return Zoom_X_Range is
      Result : constant Zoom_X_Factor :=
                  Zoom_X_Factor (exp (Float (Value)));
   begin
      if Result < Zoom_X_Range'First then
         return Zoom_X_Range'First;
      elsif Result > Zoom_X_Range'Last then
         return Zoom_X_Range'Last;
      else
         return Result;
      end if;
   end From_Scale;

   function From_Scale (Value : GDouble) return Zoom_Y_Range is
      Result : constant Zoom_Y_Factor :=
               Zoom_Y_Factor (exp (Float (Upper_Y_Zoom - Value)));
   begin
      if Result < Zoom_Y_Range'First then
         return Zoom_Y_Range'First;
      elsif Result > Zoom_Y_Range'Last then
         return Zoom_Y_Range'Last;
      else
         return Result;
      end if;
   end From_Scale;

   function Get_Data
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Fuzzy_Linguistic_Set_Zoom_Data_Ptr is
   begin
      if not Widget.Data.Is_Valid then
         return null;
      end if;
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Widget.Data.Ptr.all;
      begin
         if Drawing.Zooming = null then
            declare
               Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                           new Fuzzy_Linguistic_Set_Zoom_Data;
            begin
               Drawing.Zooming := Data.all'Unchecked_Access;
               Data.Set := Ref (Widget);
               --
               -- Enabling scrolls
               --
               Widget.Set_X_Scroll (True);
               Widget.Set_Y_Scroll (True);

               Adjustment_Callbacks.Connect
               (  Widget.X_Scroll.Get_Adjustment,
                  "value_changed",
                  Scroll_X'Access,
                  Data
               );
               Adjustment_Callbacks.Connect
               (  Widget.Y_Scroll.Get_Adjustment,
                  "value_changed",
                  Scroll_Y'Access,
                  Data
               );
               --
               -- Mouse tracking and buttons clicks events
               --
               Widget.View.Set_Can_Focus (True);
               Widget.View.Set_Events
               (  Exposure_Mask
               or Leave_Notify_Mask
               or Button_Press_Mask
               or Button_Release_Mask
               or Pointer_Motion_Mask
               or Pointer_Motion_Hint_Mask
               );
               Data.Style_Updated.Set
               (  Style_Callbacks.Connect
                  (  Widget.all'Unchecked_Access, -- Compiler bug
                     "style-updated",             -- work-around
                     Style_Callbacks.To_Marshaller
                     (  Style_Updated'Access
                     ),
                     Data
               )  );
               Data.Button_Press.Set
               (  Area_Callbacks.Connect
                  (  Widget.View,
                     "button_press_event",
                     Area_Callbacks.To_Marshaller
                     (  Button_Press'Access
                     ),
                     Data
               )  );
               Data.Button_Release.Set
               (  Area_Callbacks.Connect
                  (  Widget.View,
                     "button_release_event",
                     Area_Callbacks.To_Marshaller
                     (  Button_Release'Access
                     ),
                     Data
               )  );
               Data.Leave_Notify.Set
               (  Area_Callbacks.Connect
                  (  Widget.View,
                     "leave_notify_event",
                     Area_Callbacks.To_Marshaller
                     (  Leave_Notify'Access
                     ),
                     Data
               )  );
               Data.Motion_Notify.Set
               (  Area_Callbacks.Connect
                  (  Widget.View,
                     "motion_notify_event",
                     Area_Callbacks.To_Marshaller
                     (  Motion_Notify'Access
                     ),
                     Data
               )  );
               Data.Zoomed.Set
               (  Zoomed_Callbacks.Connect
                  (  Widget.all'Unchecked_Access, -- Compiler bug
                     "zoomed",                    -- work-around
                     Zoomed_Callbacks.To_Marshaller (Zoomed'Access),
                     Data
               )  );
               return Data;
            end;
         else
            return Fuzzy_Linguistic_Set_Zoom_Data'Class
                   (  Drawing.Zooming.all
                   ) 'Unchecked_Access;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Data")
         )  );
         raise;
   end Get_Data;

   function Get_Domain_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record
            )  return not null access
                      Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class is
   begin
      return Widget.Data.Ptr.Set.Get;
   end Get_Domain_View;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name & "ZoomPanel"
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "column-spacings",
               Nick    => "Column spacings",
               Blurb   => "Spacing between columns",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "row-spacings",
               Nick    => "Row spacings",
               Blurb   => "Spacing between rows",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   function Get_X_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Value is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Value = null then
         Data.Value := new Gtk_Fuzzy_Linguistic_Set_Value_Record;
         Data.Value.Data.Set (Data);
         Initialize (Data.Value, "");
      end if;
      return Data.Value;
   end Get_X_Tracker;

   function Get_Y_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Membership is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Level = null then
         Data.Level := new Gtk_Fuzzy_Linguistic_Set_Membership_Record;
         Data.Level.Data.Set (Data);
         Initialize (Data.Level, "");
      end if;
      return Data.Level;
   end Get_Y_Tracker;

   function Get_Zoom_100_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_100 is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Zoom_100 = null then
         Data.Zoom_100 := new Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record;
         Data.Zoom_100.Data.Set (Data);
         Zoom_100_Buttons.Initialize (Data.Zoom_100);
         Zoom_100_Callbacks.Connect
         (  Data.Zoom_100,
            "clicked",
            Zoomed_100'Access
         );
      end if;
      return Data.Zoom_100;
   end Get_Zoom_100_Button;

   function Get_Zoom_Fit_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Fit is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Zoom_Fit = null then
         Data.Zoom_Fit := new Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record;
         Data.Zoom_Fit.Data.Set (Data);
         Zoom_Fit_Buttons.Initialize (Data.Zoom_Fit);
         Zoom_Fit_Callbacks.Connect
         (  Data.Zoom_Fit,
            "clicked",
            Zoomed_Fit'Access
         );
      end if;
      return Data.Zoom_Fit;
   end Get_Zoom_Fit_Button;

   function Get_Zoom_In_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_In is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Zoom_In = null then
         Data.Zoom_In := new Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record;
         Data.Zoom_In.Data.Set (Data);
         Zoom_In_Buttons.Initialize (Data.Zoom_In);
         Zoom_In_Callbacks.Connect
         (  Data.Zoom_In,
            "clicked",
            Zoomed_In'Access
         );
      end if;
      return Data.Zoom_In;
   end Get_Zoom_In_Button;

   function Get_Zoom_Out_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Out is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Zoom_Out = null then
         Data.Zoom_Out := new Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record;
         Data.Zoom_Out.Data.Set (Data);
         Zoom_Out_Buttons.Initialize (Data.Zoom_Out);
         Zoom_Out_Callbacks.Connect
         (  Data.Zoom_Out,
            "clicked",
            Zoomed_Out'Access
         );
      end if;
      return Data.Zoom_Out;
   end Get_Zoom_Out_Button;

   function Get_Zoom_Redo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Redo is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Redo = null then
         Data.Redo := new Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record;
         Data.Redo.Data.Set (Data);
         Zoom_Redo_Buttons.Initialize (Data.Redo);
         Data.Redo.Set_Sensitive (not Data.Redo_Stack.Is_Empty);
         Redo_Callbacks.Connect
         (  Data.Redo,
            "clicked",
            Zoomed_Redo'Access
         );
      end if;
      return Data.Redo;
   end Get_Zoom_Redo_Button;

   function Get_Zoom_Undo_Button
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Undo is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
   begin
      if Data.Undo = null then
         Data.Undo := new Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record;
         Data.Undo.Data.Set (Data);
         Zoom_Undo_Buttons.Initialize (Data.Undo);
         Data.Undo.Set_Sensitive (not Data.Undo_Stack.Is_Empty);
         Undo_Callbacks.Connect
         (  Data.Undo,
            "clicked",
            Zoomed_Undo'Access
         );
      end if;
      return Data.Undo;
   end Get_Zoom_Undo_Button;

   function Get_Zoom_X_Scale
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_X is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
      Adjustment : Gtk_Adjustment;
   begin
      if Data.Zoom_X = null then
         Gtk_New
         (  Adjustment,
            Value => To_Scale (Zoom_X_Range'First),
            Lower => To_Scale (Zoom_X_Range'First),
            Upper => To_Scale (Zoom_X_Range'Last) + 1.0,
            Step_Increment => 1.0,
            Page_Increment => 1.0,
            Page_Size      => 1.0
         );
         Data.Zoom_X := new Gtk_Fuzzy_Linguistic_Set_Zoom_X_Record;
         Data.Zoom_X.Data.Set (Data);
         Initialize_HScale
         (  Data.Zoom_X.all'Unchecked_Access,
            Adjustment
         );
         Data.Zoom_X.Set_Draw_Value (False);
         Adjustment_Callbacks.Connect
         (  Adjustment,
            "value_changed",
            Zoomed_X'Access,
            Data
         );
      end if;
      return Data.Zoom_X;
   end Get_Zoom_X_Scale;

   function Get_Zoom_Y_Scale
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
            )  return Gtk_Fuzzy_Linguistic_Set_Zoom_Y is
      Data : constant Fuzzy_Linguistic_Set_Zoom_Data_Ptr :=
                Get_Data (Widget);
      Adjustment : Gtk_Adjustment;
   begin
      if Data.Zoom_Y = null then
         Gtk_New
         (  Adjustment,
            Value => To_Scale (Zoom_Y_Range'First),
            Lower => To_Scale (Zoom_Y_Range'Last),
            Upper => To_Scale (Zoom_Y_Range'First) + 1.0,
            Step_Increment => 1.0,
            Page_Increment => 1.0,
            Page_Size      => 1.0
         );
         Data.Zoom_Y := new Gtk_Fuzzy_Linguistic_Set_Zoom_Y_Record;
         Data.Zoom_Y.Data.Set (Data);
         Initialize_VScale (Data.Zoom_Y, Adjustment);
         Data.Zoom_Y.Set_Draw_Value (False);
         Adjustment_Callbacks.Connect
         (  Adjustment,
            "value_changed",
            Zoomed_Y'Access,
            Data
         );
      end if;
      return Data.Zoom_Y;
   end Get_Zoom_Y_Scale;

   procedure Gtk_New
             (  Panel  : out Gtk_Fuzzy_Linguistic_Set_Zoom_Panel;
                Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             )  is
   begin
      Panel := new Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record;
      begin
         Initialize (Panel, Widget);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Panel);
            Panel := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Panel  : not null access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record'Class;
                Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
             )  is
   begin
      G_New (Panel, Get_Type);
      Gtk.Table.Initialize (Panel, 3, 3, False);
      Panel.Set_Col_Spacings (3);
      Panel.Set_Row_Spacings (3);
      Zoom_Callbacks.Connect
      (  Panel,
         "style-updated",
         Style_Updated'Access
      );
      Panel.Attach
      (  Get_Zoom_X_Scale (Widget),
         1, 3, 2, 3,
         Gtk.Enums.Fill, Gtk.Enums.Fill
      );
      Panel.Attach
      (  Get_Zoom_Y_Scale (Widget),
         0, 1, 0, 2,
         Gtk.Enums.Fill, Gtk.Enums.Fill
      );
      Panel.Attach
      (  Get_Zoom_Undo_Button (Widget),
         1, 2, 0, 1,
         0, 0
      );
      Panel.Attach
      (  Get_Zoom_In_Button (Widget),
         2, 3, 0, 1,
         0, 0
      );
      Panel.Attach
      (  Get_Zoom_Out_Button (Widget),
         1, 2, 1, 2,
         0, 0
      );
      Panel.Attach
      (  Get_Zoom_Redo_Button (Widget),
         2, 3, 1, 2,
         0, 0
      );
      Panel.Attach
      (  Get_Zoom_100_Button (Widget),
         0, 1, 2, 3,
         0, 0
      );
      Panel.Show_All;
      Panel.Data.Set (Get_Data (Widget));
      Style_Updated (Panel);
   end Initialize;

   procedure Initialize (State : in out Zoom_Undo_State) is
      Drawing : Linguistic_Set_Data'Class renames
                State.Data.Set.Get.Data.Ptr.all;
   begin
      if State.Data.Pending_Zoom then
         State.Inactive := True;
      else
         State.Data.Pending_Zoom := True;
         State.Item := Get (Drawing.Area);
      end if;
   end Initialize;

   function Leave_Notify
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean is
   begin
      if Data.Value /= null then
         Data.Value.Set_Text ("");
      end if;
      if Data.Level /= null then
         Data.Level.Set_Text ("");
      end if;
      return False;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Leave_Notify")
         )  );
         return False;
   end Leave_Notify;

   function Motion_Notify
            (  Area  : access Gtk_Drawing_Area_Record'Class;
               Event : Gdk_Event;
               Data  : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
            )  return Boolean is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         X       : X_Axis;
         Y       : Y_Axis;
         Inside  : Boolean;
      begin
         Mouse_Event (Area, Event, Data, X, Y, Inside, True);
         if (  Active = Drawing.Area_Selection.State
            and then
               (  X /= Drawing.Area_Selection.X2
               or else
                  Y /= Drawing.Area_Selection.Y2
            )  )
         then
            declare
               Context : constant Cairo_Context :=
                         Create (Area.Get_Window);
            begin
               Drawing.Draw_Area_Selection (Context, False);
               Drawing.Area_Selection.X2 := X;
               Drawing.Area_Selection.Y2 := Y;
               Drawing.Draw_Area_Selection (Context, True);
               Drawing.Draw_Selected (Context);
               Destroy (Context);
            exception
               when others =>
                  Destroy (Context);
                  raise;
            end;
         end if;
         if Data.Value /= null then
            if Inside then
               declare
                  Note  : constant String :=
                             Get_Domain_Note (Drawing, Drawing.X_Power);
                  Value : constant String :=
                             Float_Edit.Image
                             (  (  (  Float_Intervals_Of.Number'Base (X)
                                   /  Drawing.Area.X_Gain
                                   +  Drawing.Area.X_Offset
                                   )
                                /  10.0 ** Drawing.X_Power
                                ),
                                AbsSmall =>
                                   Integer'Min
                                   (  (  Drawing.X_Tracker_Small
                                       - Drawing.X_Power
                                      ),
                                      0
                             )     );
               begin
                  if Note'Length = 0 then
                     Data.Value.Set_Text (Value);
                  else
                     Data.Value.Set_Text (Value & ' ' & Note);
                  end if;
               end;
            else
               Data.Value.Set_Text ("");
            end if;
         end if;
         if Data.Level /= null then
            if Inside then
               Data.Level.Set_Text
               (  Confidence_Factors.Edit.Image
                  (  To_Confidence
                     (  Drawing.Area.Y_Offset
                     -  Truth (Y)
                     /  Drawing.Area.Y_Gain
               )  )  );
            else
               Gtk.Label.Set_Text
               (  Gtk_Label_Record'Class (Data.Level.all)'Access,
                  ""
               );
            end if;
         end if;
         return True;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Motion_Notify")
         )  );
         return True;
   end Motion_Notify;

   procedure Mouse_Event
             (  Area   : not null access Gtk_Drawing_Area_Record'Class;
                Event  : Gdk_Event;
                Data   : Fuzzy_Linguistic_Set_Zoom_Data_Ptr;
                X      : out X_Axis;
                Y      : out Y_Axis;
                Inside : out Boolean;
                Hint   : Boolean := False
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                Data.Set.Get.Data.Ptr.all;
      Mouse_X : GDouble;
      Mouse_Y : GDouble;
   begin
      Get_Axis (Event, Axis_X, Mouse_X);
      Get_Axis (Event, Axis_Y, Mouse_Y);
      Inside := True;
      if (  Mouse_X in -32_000.0..32_000.0
         and then
            Mouse_Y in -32_000.0..32_000.0
         )
      then
         X := X_Axis (Mouse_X);
         Y := Y_Axis (Mouse_Y);
      else
         Inside := False;
         return;
      end if;
      X := X - Drawing.Left_Offs + 1;
      if X < 1 then
         Inside := False;
         X := 1;
      elsif X > Get_Width (Drawing.Area) then
         Inside := False;
         X := Get_Width (Drawing.Area);
      end if;
      Y := Y + 1;
      if Y < 1 then
         Inside := False;
         Y := 1;
      elsif Y > Get_Height (Drawing.Area) then
         Inside := False;
         Y := Get_Height (Drawing.Area);
      end if;
   exception
      when Error : others =>
         Inside := False;
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Mouse_Event")
         )  );
   end Mouse_Event;

   function Ref (Object : Fuzzy_Linguistic_Set_Zoom_Data_Ptr)
      return Zoom_Data_Handle is
   begin
      return (Zoom_Data_Handles.Ref (Object) with Undo_Button);
   end Ref;

   procedure Scroll_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
         State   : Zoom_Undo_State (Data);
      begin
         if not State.Inactive then
            Scroll_Left
            (  Drawing,
               Float_Intervals_Of.Number'Base (Adjustment.Get_Value),
               null -- Don't touch the adjustment
            );
            Refresh (Data.Set.Get);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Scroll_X")
         )  );
   end Scroll_X;

   procedure Scroll_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         State   : Zoom_Undo_State (Data);
      begin
         if not State.Inactive then
            Scroll_Top
            (  Drawing,
               1.0 - Truth (Adjustment.Get_Value),
               null -- Don't touch the adjustment
            );
            Refresh (Data.Set.Get);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Scroll_Y")
         )  );
   end Scroll_Y;

   procedure Set
             (  Ref    : in out Zoom_Data_Handle;
                Object : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      Zoom_Data_Handles.Set (Zoom_Data_Handles.Handle (Ref), Object);
   end Set;

   procedure Style_Updated
             (  Widget : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record'Class
             )  is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                   Widget.Data.Ptr.all;
      begin
         Set_Col_Spacings
         (  Widget,
            Style_Get (Widget, "column-spacings")
         );
         Set_Row_Spacings (Widget, Style_Get (Widget, "row-spacings"));
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where
               (  "Style_Updated "
               &  "(Gtk_Fuzzy_Linguistic_Set_Zoom_Panel_Record)"
         )  )  );
   end Style_Updated;

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Data : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      if Data.Zoom_X /= null then
         Data.Zoom_X.Set_Tooltip_Text
         (  Style_Get (Widget, "x-zoom-tip")
         );
      end if;
      if Data.Zoom_Y /= null then
         Data.Zoom_Y.Set_Tooltip_Text
         (  Style_Get (Widget, "y-zoom-tip")
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where
               (  "Style_Updated "
               &  "(Gtk_Fuzzy_Linguistic_Set_Domain_Record)"
         )  )  );
   end Style_Updated;

   function To_Scale (Value : Zoom_X_Range) return GDouble is
   begin
      return GDouble (log (Float (Value)));
   end To_Scale;

   function To_Scale (Value : Zoom_Y_Range) return GDouble is
   begin
      return Upper_Y_Zoom - GDouble (log (Float (Value)));
   end To_Scale;

   procedure Zoomed
             (  Widget : access
                         Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
                Data : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
      Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
      State : Zoom_Undo_State (Data);
   begin
      if not State.Inactive then
         if Data.Zoom_X /= null then
            Data.Zoom_X.Set_Value (To_Scale (Drawing.Get_X_Zoom));
         end if;
         if Data.Zoom_Y /= null then
            Data.Zoom_Y.Set_Value (To_Scale (Drawing.Get_Y_Zoom));
         end if;
      end if;
   end Zoomed;

   procedure Zoomed_By
             (  Data    : Fuzzy_Linguistic_Set_Zoom_Data'Class;
                Set     : not null access
                          Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class;
                Drawing : in out Linguistic_Set_Data'Class;
                By      : Natural;
                Inc     : GDouble
             )  is
   begin
      declare
         X : Zoom_X_Factor := Drawing.Get_X_Zoom;
         Y : Zoom_Y_Factor := Drawing.Get_Y_Zoom;
      begin
         X := X ** By * Zoom_X_Factor (Inc);
         Y := Y ** By * Zoom_Y_Factor (Inc);
         if Data.Zoom_X = null then
            if Data.Zoom_Y /= null then
               Drawing.Zoom
               (  Zoom_X_Factor'First,
                  Y,
                  null,
                  Set.Get_Y_Adjustment
               );
               Data.Zoom_Y.Set_Value (To_Scale (Drawing.Get_Y_Zoom));
            end if;
         else
            if Data.Zoom_Y = null then
               Drawing.Zoom
               (  X,
                  Zoom_Y_Factor'First,
                  Set.Get_X_Adjustment,
                  null
               );
               Data.Zoom_X.Set_Value (To_Scale (Drawing.Get_X_Zoom));
            else
               Drawing.Zoom
               (  X,
                  Y,
                  Set.Get_X_Adjustment,
                  Set.Get_Y_Adjustment
               );
               Data.Zoom_X.Set_Value (To_Scale (Drawing.Get_X_Zoom));
               Data.Zoom_Y.Set_Value (To_Scale (Drawing.Get_Y_Zoom));
            end if;
         end if;
         Refresh (Set);
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_By")
         )  );
   end Zoomed_By;

   procedure Zoomed_100
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_100_Record'Class
             )  is
   begin
      declare
         Data    : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                      Button.Data.Ptr.all;
         Set     : Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class renames
                      Data.Set.Get.all;
         Drawing : Linguistic_Set_Data'Class renames Set.Data.Ptr.all;
         State   : Zoom_Undo_State (Data'Access);
      begin
         Zoomed_By (Data, Set'Access, Drawing, 0, 0.0);
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_100")
         )  );
   end Zoomed_100;

   procedure Zoomed_Fit
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Fit_Record'Class
             )  is
   begin
      declare
         Data    : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                      Button.Data.Ptr.all;
         Drawing : Linguistic_Set_Data'Class renames
                      Data.Set.Get.Data.Ptr.all;
         State   : Zoom_Undo_State (Data'Access);
      begin
         Rescale_X (Drawing, Data.Set.Get.Get_X_Adjustment);
         Refresh (Data.Set.Get);
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_Fit")
         )  );
   end Zoomed_Fit;

   procedure Zoomed_In
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_In_Record'Class
             )  is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                Button.Data.Ptr.all;
         Set  : Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class renames
                Data.Set.Get.all;
         Drawing : Linguistic_Set_Data'Class renames Set.Data.Ptr.all;
         State   : Zoom_Undo_State (Data'Access);
      begin
         if Drawing.Area_Selection.State = None then
            Zoomed_By (Data, Set'Access, Drawing, 1, Zoom_Inc);
         else
            if not State.Inactive then
               declare
                  Area    : Drawing_Area renames Drawing.Area;
                  Context : constant Cairo_Context :=
                               Create (Set.View.Get_Window);
                  X1 : constant Float_Intervals_Of.Number'Base :=
                          From_X (Area, Drawing.Area_Selection.X1);
                  X2 : constant Float_Intervals_Of.Number'Base :=
                          From_X (Area, Drawing.Area_Selection.X2);
                  Y1 : constant Truth :=
                          From_Y (Area, Drawing.Area_Selection.Y1);
                  Y2 : constant Truth :=
                          From_Y (Area, Drawing.Area_Selection.Y2);
               begin
                  Drawing.Draw_Area_Selection (Context, False);
                  Drawing.Area_Selection.State := None;
                  if X1 > X2 then
                     Drawing.Rescale_X
                     (  (X2, X1),
                        Set.Get_X_Adjustment,
                        False
                     );
                  else
                     Drawing.Rescale_X
                     (  (X1, X2),
                        Set.Get_X_Adjustment,
                        False
                     );
                  end if;
                  if Y1 > Y2 then
                     Drawing.Rescale_Y (Y2, Y1, Set.Get_Y_Adjustment);
                  else
                     Drawing.Rescale_Y (Y1, Y2, Set.Get_Y_Adjustment);
                  end if;
                  Set.Refresh;
                  if Data.Zoom_X /= null then
                     Data.Zoom_X.Set_Value
                     (  To_Scale (Drawing.Get_X_Zoom)
                     );
                  end if;
                  if Data.Zoom_Y /= null then
                     Data.Zoom_Y.Set_Value
                     (  To_Scale (Drawing.Get_Y_Zoom)
                     );
                  end if;
                  Drawing.Draw_Selected (Context);
                  Destroy (Context);
               exception
                  when others =>
                     Destroy (Context);
                     raise;
               end;
               Data.Set.Get.Marked;
            end if;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_In")
         )  );
   end Zoomed_In;

   procedure Zoomed_Out
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Out_Record'Class
             )  is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                Button.Data.Ptr.all;
         Set  : Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class renames
                Data.Set.Get.all;
         Drawing : Linguistic_Set_Data'Class renames Set.Data.Ptr.all;
         State   : Zoom_Undo_State (Data'Access);
      begin
         if Drawing.Area_Selection.State = None then
            Zoomed_By (Data, Set'Access, Drawing, 1, -Zoom_Inc);
         else
            if not State.Inactive then
               declare
                  Area    : Drawing_Area renames Drawing.Area;
                  Context : constant Cairo_Context :=
                            Create (Set.View.Get_Window);
               begin
                  Drawing.Draw_Area_Selection (Context, False);
                  Drawing.Area_Selection.State := None;
                  declare
                     Gain   : Float_Intervals_Of.Number'Base;
                     Offset : Float_Intervals_Of.Number'Base;
                     Left   : constant Float_Intervals_Of.Number'Base :=
                              From_X (Area, 1);
                     Right  : constant Float_Intervals_Of.Number'Base :=
                              From_X (Area, Get_Width (Area));
                     X1     : constant Float_Intervals_Of.Number'Base :=
                              From_X (Area, Drawing.Area_Selection.X1);
                     X2     : constant Float_Intervals_Of.Number'Base :=
                              From_X (Area, Drawing.Area_Selection.X2);
                  begin
                     Gain   := (Right - Left) / abs (X2 - X1);
                     Offset :=
                        (  Left
                        -  Gain * Float_Intervals_Of.Number'Min (X1, X2)
                        );
                     Drawing.Rescale_X
                     (  (Gain * Left + Offset, Gain * Right + Offset),
                        Set.Get_X_Adjustment,
                        False
                     );
                  end;
                  declare
                     Gain   : Truth;
                     Offset : Truth;
                     Top    : constant Truth := From_Y (Area, 1);
                     Bottom : constant Truth :=
                                 From_Y (Area, Get_Height (Area));
                     Y1     : constant Truth :=
                                 From_Y
                                 (  Area,
                                    Drawing.Area_Selection.Y1
                                 );
                     Y2     : constant Truth :=
                                 From_Y
                                 (  Area,
                                    Drawing.Area_Selection.Y2
                                 );
                  begin
                     Gain   := (Top - Bottom) / abs (Y2 - Y1);
                     Offset := Bottom - Gain * Truth'Min (Y1, Y2);
                     Drawing.Rescale_Y
                     (  Gain * Bottom + Offset,
                        Gain * Top + Offset,
                        Set.Get_Y_Adjustment
                     );
                  end;
                  Set.Refresh;
                  if Data.Zoom_X /= null then
                     Data.Zoom_X.Set_Value
                     (  To_Scale (Drawing.Get_X_Zoom)
                     );
                  end if;
                  if Data.Zoom_Y /= null then
                     Data.Zoom_Y.Set_Value
                     (  To_Scale (Drawing.Get_Y_Zoom)
                     );
                  end if;
                  Drawing.Draw_Selected (Context);
                  Destroy (Context);
               exception
                  when others =>
                     Destroy (Context);
                     raise;
               end;
               Data.Set.Get.Marked;
            end if;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_Out")
         )  );
   end Zoomed_Out;

   procedure Zoomed_Redo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Redo_Record'Class
             )  is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                   Button.Data.Ptr.all;
      begin
         if Data.Redo_Stack.Is_Empty then
            if Data.Redo /= null then
               Data.Redo.Set_Sensitive (False);
            end if;
         else
            declare
               Set : Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
                        renames Data.Set.Get.all;
               Drawing : Linguistic_Set_Data'Class renames
                            Set.Data.Ptr.all;
               This    : constant Zoom_Undo_Item := Get (Drawing.Area);
               That    : constant Zoom_Undo_Item :=
                            Top (Data.Redo_Stack);
            begin
               Data.Pending_Zoom := True;
               if Is_Empty (Data.Undo_Stack) then
                  if Data.Undo /= null then
                     Data.Undo.Set_Sensitive (True);
                  end if;
               end if;
               Push (Data.Undo_Stack, This);
               Pop  (Data.Redo_Stack);
               if Data.Redo_Stack.Is_Empty then
                  if Data.Redo /= null then
                     Data.Redo.Set_Sensitive (False);
                  end if;
               end if;
               Scale
               (  Drawing,
                  That,
                  Get_X_Adjustment (Set'Access),
                  Get_Y_Adjustment (Set'Access)
               );
               if Data.Zoom_X /= null then
                  Set_Value
                  (  Data.Zoom_X,
                     To_Scale (Drawing.Get_X_Zoom)
                  );
               end if;
               if Data.Zoom_Y /= null then
                  Set_Value
                  (  Data.Zoom_Y,
                     To_Scale (Drawing.Get_Y_Zoom)
                  );
               end if;
               Commit_Zoom (Set'Access);
               Data.Pending_Zoom := False;
            end;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_Redo")
         )  );
   end Zoomed_Redo;

   procedure Zoomed_Undo
             (  Button : access
                   Gtk_Fuzzy_Linguistic_Set_Zoom_Undo_Record'Class
             )  is
   begin
      declare
         Data : Fuzzy_Linguistic_Set_Zoom_Data'Class renames
                Button.Data.Ptr.all;
      begin
         if Data.Undo_Stack.Is_Empty then
            if Data.Undo /= null then
               Data.Undo.Set_Sensitive (False);
            end if;
         else
            declare
               Set : Gtk_Fuzzy_Linguistic_Set_Domain_Record'Class
                        renames Data.Set.Get.all;
               Drawing : Linguistic_Set_Data'Class renames
                            Set.Data.Ptr.all;
               This    : constant Zoom_Undo_Item := Get (Drawing.Area);
               That    : constant Zoom_Undo_Item :=
                            Top (Data.Undo_Stack);
            begin
               Data.Pending_Zoom := True;
               if Data.Redo_Stack.Is_Empty then
                  if Data.Redo /= null then
                     Data.Redo.Set_Sensitive (True);
                  end if;
               end if;
               Data.Redo_Stack.Push (This);
               Data.Undo_Stack.Pop;
               if Data.Undo_Stack.Is_Empty then
                  if Data.Undo /= null then
                     Data.Undo.Set_Sensitive (False);
                  end if;
               end if;
               Scale
               (  Drawing,
                  That,
                  Set.Get_X_Adjustment,
                  Set.Get_Y_Adjustment
               );
               if Data.Zoom_X /= null then
                  Data.Zoom_X.Set_Value
                  (  To_Scale (Drawing.Get_X_Zoom)
                  );
               end if;
               if Data.Zoom_Y /= null then
                  Data.Zoom_Y.Set_Value
                  (  To_Scale (Drawing.Get_Y_Zoom)
                  );
               end if;
               Commit_Zoom (Set'Access);
               Data.Pending_Zoom := False;
            end;
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_Undo")
         )  );
   end Zoomed_Undo;

   procedure Zoomed_X
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
         State   : Zoom_Undo_State (Data);
      begin
         if not State.Inactive then
            Zoom
            (  Drawing,
               From_Scale (Adjustment.Get_Value),
               Zoom_Y_Range'First,
               Data.Set.Get.Get_X_Adjustment,
               null
            );
            Refresh (Data.Set.Get);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_X")
         )  );
   end Zoomed_X;

   procedure Zoomed_Y
             (  Adjustment : access Gtk_Adjustment_Record'Class;
                Data       : Fuzzy_Linguistic_Set_Zoom_Data_Ptr
             )  is
   begin
      declare
         Drawing : Linguistic_Set_Data'Class renames
                   Data.Set.Get.Data.Ptr.all;
         State   : Zoom_Undo_State (Data);
      begin
         if not State.Inactive then
            Drawing.Zoom
            (  Zoom_X_Range'First,
               From_Scale (Adjustment.Get_Value),
               null,
               Data.Set.Get.Get_Y_Adjustment
            );
            Refresh (Data.Set.Get);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Zoomed_Y")
         )  );
   end Zoomed_Y;

end Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Zoom_Panel;
