--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Set_Entry                         Luebeck            --
--  Implementation                                 Spring, 2007       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
with Confidence_Factors;        use Confidence_Factors;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Values.Handling;      use GLib.Values.Handling;
with GLib.Types;                use GLib.Types;
with Gdk.Main;                  use Gdk.Main;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Gdk.Window;                use Gdk.Window;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with GtkAda.Types;              use GtkAda.Types;

with Ada.IO_Exceptions;
with Fuzzy.Abstract_Edit.Intuitionistic;
with GLib.Object.Checked_Destroy;
with GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Intuitionistic;
with GLib.Values.Fuzzy.Logic;
with Gtk.Cell_Editable;
with Gtk.Main;

package body Gtk.Fuzzy_Set_Entry is
   use Fuzzy.Abstract_Edit.Handle;
   use Fuzzy.Logic;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Set_Entry." & Name;
   end Where;
--
-- Editing_Done -- Emits editing-done
--
   procedure Editing_Done (Editable : System.Address);
   pragma Import (C, Editing_Done, "gtk_cell_editable_editing_done");
--
-- Remove_Widget -- Emits remove-widget
--
   procedure Remove_Widget (Editable : System.Address);
   pragma Import (C, Remove_Widget, "gtk_cell_editable_remove_widget");
--
-- Gtk_Cell_Editable_Iface -- Interface of GtkCellEditable
--
   type Gtk_Cell_Editable_Iface is record
      Editing_Done  : System.Address;
      Remove_Widget : System.Address;
      Start_Editing : Start_Editing_Entry;
   end record;
   pragma Convention (C, Gtk_Cell_Editable_Iface);

   type Gtk_Cell_Editable_Iface_Ptr is
      access all Gtk_Cell_Editable_Iface;
   pragma Convention (C, Gtk_Cell_Editable_Iface_Ptr);
--
-- Interface_Peek -- Get the interface's "virtual" table
--
   function Interface_Peek
            (  Class : GObject_Class;
               IFace : GType := Gtk.Cell_Editable.Get_Type
            )  return Gtk_Cell_Editable_Iface_Ptr;
   pragma Import (C, Interface_Peek, "g_type_interface_peek");

   procedure Create_Popup
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class
             );

   procedure Init
             (  Widget  : not null access
                          Gtk_Fuzzy_Set_Entry_Record'Class;
                Initial : UTF8_String
             );

   procedure Activated_Entry
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             )  is
   begin
      Edit.Being_Changed := True;
      case Get_Content_Type (Edit) is
         when Plain_Set =>
            declare
               Update : Fuzzy.Set renames
                        Value (Edit.Domain.Ptr.all, Edit.Get_Text);
            begin
               Edit.Put (Update);
            end;
         when Intuitionistic_Set =>
            declare
               Update : Fuzzy.Intuitionistic.Set renames
                        Value (Edit.Domain.Ptr.all, Edit.Get_Text);
            begin
               Edit.Put (Update);
            end;
         when Intuitionistic_Classification =>
            declare
               Update : Fuzzy.Intuitionistic.Classification renames
                        Value (Edit.Domain.Ptr.all, Edit.Get_Text);
            begin
               Edit.Put (Update);
            end;
      end case;
      Edit.Canceled      := False;
      Edit.Edited        := True;
      Edit.Being_Changed := False;
   exception
      when others =>
         Edit.Canceled      := True;
         Edit.Being_Changed := False;
   end Activated_Entry;

   procedure Beep (Widget : not null access Gtk_Widget_Record'Class) is
   begin
      Widget.Get_Display.Beep;
   end Beep;

   function Get_Window (Widget : access Gtk_Widget_Record'Class)
      return Gtk_Window is
      Parent : constant Gtk_Widget := Get_Toplevel (Widget);
   begin
      if Parent /= null and then Parent.all in Gtk_Window_Record'Class
      then
         return Gtk_Window_Record'Class (Parent.all)'Access;
      else
         return null;
      end if;
   end Get_Window;

   function Button_Press
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean is
   begin
      if Edit.Being_Edited then
         --
         -- Key pressed while popup window is open
         --
         case Get_Event_Type (Event) is
            when Key_Press =>
               case Get_Key_Val (Event) is
                  when GDK_Return | GDK_KP_Enter | GDK_ISO_Enter =>
                     Edit.Canceled := False;
                     Changed_Entry (Edit.Edit, Edit);
                     if Edit.Canceled then
                        Beep (Widget);
                        return False;
                     end if;
                     Edit.Edited := True;
                     Edit.Set_Text (Edit.Edit.Get_Text);
                     Unset (Edit.Value);
                     Edit.Value := Edit.Selection.Get;
                     Edit.Done_Popup;
                     return True;
                  when GDK_Escape =>
                     Edit.Canceled := True;
                     Edit.Selection.Put (Edit.Value);
                     Edit.Done_Popup;
                     return True;
                  when others =>
                     return False;
               end case;
            when others =>
               return False;
         end case;
      else
         --
         -- Key pressed while popup window is closed
         --
         case Get_Event_Type (Event) is
            when Button_Press =>
               Edit.Create_Popup;
               Edit.Being_Edited := True;
               return True;
            when others =>
               return False;
         end case;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Button_Press")
         )  );
         return False;
   end Button_Press;

   function Button_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean is
      This  : constant Gtk_Widget := Widget.all'Access;
      Child : Gtk_Widget := Gtk.Main.Get_Event_Widget (Event);
   begin
      if Child /= This then
         while Child /= null loop
            Child := Child.Get_Parent;
            if Child = This then
               return False;
            end if;
         end loop;
      end if;
      --
      -- Mouse  button  press  happened  ouside  any  of  the   widget's
      -- children, that means outside the popup  window.  So  we  cancel
      -- editing
      --
      Edit.Canceled := True;
      Edit.Selection.Put (Edit.Value);
      Edit.Done_Popup;
      return True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Button_Press_Popup")
         )  );
         return True;
   end Button_Press_Popup;

   procedure Create_Popup
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class
             )  is
      Window : constant Gtk_Window := Get_Window (Widget);
      Frame  : Gtk_Frame;
      Event  : Gtk_Event_Box;
      Dummy  : Gdk_Grab_Status;
      Width  : constant GInt := 2;
   begin
      Widget.Canceled := True;
      if Widget.Popup = null then
         -- Creating the popup window
         Gtk_New (Widget.Popup, Window_Popup);
         --
         -- Focus out for the popup window will be detected by  catching
         -- the mouse button press events. The pointer will  be  grabbed
         -- later on. When a button get clicked Button_Press_Popup  will
         -- determine if it was outside the window.
         --
         Result_Handlers.Connect
         (  Widget.Popup,
            "button_press_event",
            Result_Handlers.To_Marshaller (Button_Press_Popup'Access),
            Widget.all'Access
         );
         Result_Handlers.Connect
         (  Get_Tree_View (Widget.Selection),
            "key_press_event",
            Result_Handlers.To_Marshaller (Key_Press_Popup'Access),
            Widget.all'Access
         );
         Widget.Popup.Set_Screen (Widget.Get_Screen);
         Widget.Popup.Set_Type_Hint (Window_Type_Hint_Combo);
         if Window /= null then
            Widget.Popup.Get_Group.Add_Window (Window);
            Widget.Popup.Set_Transient_For (Window);
         end if;
         Gtk_New (Event);
         Widget.Popup.Add (Event);
         Gtk_New (Frame);
         Event.Add (Frame);
         Frame.Set_Shadow_Type (Shadow_Out);
         -- Adding the selection widget
         Frame.Add (Widget.Box);
      end if;
      -- Frame around the popup window
      Widget.Being_Changed := True;
      Widget.Edit.Set_Text (Widget.Get_Text);
      Widget.Being_Changed := False;
      Widget.Edit.Set_Can_Default (True);
      Widget.Popup.Set_Default (Widget.Edit);
      Widget.Popup.Show_All;
      Widget.Popup.Grab_Focus;
      Widget.Popup.Set_Modal;
      Dummy :=
         Pointer_Grab
         (  Widget.Popup.Get_Window,
            True,
            Button_Press_Mask
         );
      Dummy := Keyboard_Grab (Get_Window (Widget.Popup));
      declare
         Dummy  : GInt;
         Height : GInt;
      begin
         Widget.Selection.Get_Tree_View.Get_Preferred_Height (Dummy, Height);
         Widget.Selection.Set_Size_Request
         (  GInt'Min (Widget.Get_Allocated_Width, 600),
            GInt'Min (Height, 500)
         );
      end;
      declare
         X : GInt;
         Y : GInt;
      begin
         Get_Screen_Position (Widget, X, Y);
         Widget.Popup.Move (X, Y);
      end;
      Widget.Popup.Present;
      Widget.Ref;
   end Create_Popup;

   procedure Changed_Entry
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             )  is
   begin
      if Edit.Being_Changed then
         return;
      end if;
      Edit.Being_Changed := True;
      case Get_Content_Type (Edit) is
         when Plain_Set =>
            declare
               Update : Fuzzy.Set renames
                           Value
                           (  Ptr (Edit.Domain).all,
                              Edit.Edit.Get_Text
                           );
            begin
               Edit.Selection.Put (Update);
            end;
         when Intuitionistic_Set =>
            declare
               Update : Fuzzy.Intuitionistic.Set renames
                           Value
                           (  Ptr (Edit.Domain).all,
                              Edit.Edit.Get_Text
                           );
            begin
               Edit.Selection.Put (Update);
            end;
         when Intuitionistic_Classification =>
            declare
               Update : Fuzzy.Intuitionistic.Classification renames
                           Value
                           (  Ptr (Edit.Domain).all,
                              Edit.Edit.Get_Text
                           );
            begin
               Edit.Selection.Put (Update);
            end;
      end case;
      Edit.Being_Changed := False;
   exception
      when others =>
         Edit.Being_Changed := False;
         Edit.Canceled      := True;
   end Changed_Entry;

   procedure Changed_Popup
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             )  is
   begin
      if Edit.Being_Changed then
         return;
      end if;
      Edit.Being_Changed := True;
      case Get_Content_Type (Edit) is
         when Plain_Set =>
            Set_Text
            (  Edit.Edit,
               Image
               (  Ptr (Edit.Domain).all,
                  Fuzzy.Set'(Edit.Selection.Get)
            )  );
         when Intuitionistic_Set =>
            Set_Text
            (  Edit.Edit,
               Image
               (  Ptr (Edit.Domain).all,
                  Fuzzy.Intuitionistic.Set'(Edit.Selection.Get)
            )  );
         when Intuitionistic_Classification =>
            Set_Text
            (  Edit.Edit,
               Image
               (  Ptr (Edit.Domain).all,
                  Classification'(Edit.Selection.Get)
            )  );
      end case;
      Edit.Being_Changed := False;
   exception
      when others =>
         Edit.Being_Changed := False;
   end Changed_Popup;

   function Create
            (  Data    : Fuzzy.Abstract_Edit.Handle.Handle;
               Default : Fuzzy_Boolean := Certain_True
             )  return Entry_Domain is
      Result : constant Entry_Domain := Ref (new Entry_Edit);
      This   : Entry_Edit renames Entry_Edit (Ptr (Result).all);
   begin
      This.Data    := Data;
      This.Default := Default;
      return Result;
   end Create;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             )  is
   begin
      if Edit.Being_Edited then
         Edit.Done_Popup;
      end if;
      if Edit.Popup /= null then
         GLib.Object.Checked_Destroy (Edit.Popup);
         Edit.Popup := null;
      end if;
      Unset (Edit.Value);
      Unref (Edit.Box);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy")
         )  );
   end Destroy;

   procedure Done_Popup
             (  Widget : access Gtk_Fuzzy_Set_Entry_Record'Class
             )  is
   begin
      Pointer_Ungrab;
      Keyboard_Ungrab;
      Set_Modal (Widget.Popup, False);
      Hide (Widget.Popup);
      Editing_Done  (Get_Object (Widget));
      Remove_Widget (Get_Object (Widget));
      Widget.Being_Edited := False;
      Unref (Widget);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Done_Popup")
         )  );
   end Done_Popup;

   function Edited
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean is
   begin
      return Widget.Edited;
   end Edited;

   function Editing_Canceled
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean is
   begin
      return Widget.Canceled;
   end Editing_Canceled;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy.Set is
      use GLib.Values.Fuzzy;
   begin
      return GLib.Values.Fuzzy.Get (Widget.Value);
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy.Intuitionistic.Set is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      return Get (Widget.Value);
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Classification is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      return Get (Widget.Value);
   end Get;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return GValue is
   begin
      return Copy (Widget.Value);
   end Get;

   function Get_Cardinality (Editor : Entry_Edit) return Natural is
      From, To : Integer;
   begin
      Fuzzy.Abstract_Edit.Get_Max_Range
      (  Editor.Data.Ptr.all,
         From,
         To
      );
      return To - From + 1;
   end Get_Cardinality;

   function Get_Cardinality
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Natural is
   begin
      return Widget.Selection.Get_Cardinality;
   end Get_Cardinality;

   function Get_Content_Type
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Content_Type is
   begin
      return Widget.Selection.Get_Content_Type;
   end Get_Content_Type;

   function Get_Default (Editor : Entry_Edit) return Fuzzy_Boolean is
   begin
      return Editor.Default;
   end Get_Default;

   function Get_Default
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Fuzzy_Boolean is
   begin
      return Widget.Domain.Ptr.Get_Default;
   end Get_Default;

   function Get_Domain (Editor : Entry_Edit)
      return Fuzzy.Abstract_Edit.Handle.Handle is
   begin
      return Editor.Data;
   end Get_Domain;

   function Get_Domain
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Entry_Domain is
   begin
      return Widget.Domain;
   end Get_Domain;

   function Get_Name
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Index  : Positive
             )  return String is
   begin
      return Widget.Selection.Get_Name (Index);
   end Get_Name;

   function Get_Type return Gtk_Type is
      Editable : Gtk_Cell_Editable_Iface_Ptr;
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.GEntry.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name & "Entry"
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boolean
            (  Name    => "has-header",
               Nick    => "Has header",
               Blurb   => "Allow columns header in selection window",
               Default => False
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_Boxed
            (  Name       => "default-truth-value",
               Boxed_Type => GLib.Values.Fuzzy.Logic.
                                GType_Fuzzy_Boolean,
               Nick       => "Default",
               Blurb      =>
                  (  "Default for undefined possibility and necessity "
                  &  "components while I/O of intuitionistic objects"
         )  )     );
      end if;
      Editable :=
         Interface_Peek (Class_Peek (Class_Record.The_Type));
      Editable.Start_Editing := Start_Editing_Ptr;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Entry_Record;
      begin
         Initialize (Widget, Domain, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Fuzzy.Set)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Entry_Record;
      begin
         Initialize (Widget, Domain, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Intuitionistic.Set)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Entry_Record;
      begin
         Initialize (Widget, Domain, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (Intuitionistic.Classification)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Set_Entry;
                Domain : Entry_Domain;
                Value  : GValue
             )  is
   begin
      Widget := new Gtk_Fuzzy_Set_Entry_Record;
      begin
         Initialize (Widget, Domain, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New (GValue)")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   function Get_Shape
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Shape is
   begin
      return Widget.Selection.Get_Shape;
   end Get_Shape;

   procedure Init
             (  Widget  : not null access
                          Gtk_Fuzzy_Set_Entry_Record'Class;
                Initial : UTF8_String
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.GEntry.Initialize (Widget);
      Widget.Set_Text (Initial);
      Gtk_New_VBox (Widget.Box);
      Gtk_New (Widget.Edit);
      Widget.Edit.Add_Events (Key_Press_Mask);
      Widget.Box.Pack_Start (Widget.Edit);
      Widget.Box.Pack_Start (Widget.Selection);
      Widget.Selection.Set_Editable (True);
      Widget.Box.Ref;
      Widget.Add_Events (Enter_Notify_Mask);
      Edit_Handlers.Connect
      (  Widget,
         "activate",
         Edit_Handlers.To_Marshaller (Activated_Entry'Access),
         Widget.all'Access
      );
      Result_Handlers.Connect
      (  Widget,
         "button_press_event",
         Result_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Result_Handlers.Connect
      (  Widget.Edit,
         "key_press_event",
         Result_Handlers.To_Marshaller (Button_Press'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect
      (  Widget,
         "style-updated",
         Edit_Handlers.To_Marshaller (Style_Updated'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect     -- Then ee'll be ready to grab focus
      (  Widget,
         "destroy",
         Edit_Handlers.To_Marshaller (Destroy'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect
      (  Widget.Edit,
         "changed",
         Edit_Handlers.To_Marshaller (Changed_Entry'Access),
         Widget.all'Access
      );
      Edit_Handlers.Connect
      (  Widget.Selection,
         "changed",
         Edit_Handlers.To_Marshaller (Changed_Popup'Access),
         Widget.all'Access
      );
      Style_Updated (Widget, Widget.all'Access);
   exception
      when others =>
         if Widget.Box /= null then
            Widget.Box.Unref;
         end if;
         raise;
   end Init;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             )  is
   begin
      Gtk_New
      (  Widget.Selection,
         Domain.Ptr.Get_Domain.Ptr.all,
         Value
      );
      Widget.Domain := Domain;
      Init (Widget, Image (Domain.Ptr.all, Value));
      Init (Widget.Value, GLib.Values.Fuzzy.GType_Set);
      GLib.Values.Fuzzy.Set (Widget.Value, Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      Gtk_New
      (  Widget.Selection,
         Domain.Ptr.Get_Domain.Ptr.all,
         Value
      );
      Widget.Domain := Domain;
      Init (Widget, Image (Domain.Ptr.all, Value));
      Init (Widget.Value, GLib.Values.Fuzzy.Intuitionistic.GType_Set);
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             )  is
   begin
      Gtk_New
      (  Widget.Selection,
         Domain.Ptr.Get_Domain.Ptr.all,
         Value
      );
      Widget.Domain := Domain;
      Init (Widget, Image (Domain.Ptr.all, Value));
      Init
      (  Widget.Value,
         GLib.Values.Fuzzy.Intuitionistic.GType_Classification
      );
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Set_Entry_Record'Class;
                Domain : Entry_Domain;
                Value  : GValue
             )  is
   begin
      Gtk_New
      (  Widget.Selection,
         Domain.Ptr.Get_Domain.Ptr.all,
         Value
      );
      Widget.Domain := Domain;
      case Get_Content_Type (Widget.Selection) is
         when Plain_Set =>
            Init
            (  Widget,
               Image
               (  Domain.Ptr.all,
                  Fuzzy.Set'
                  (  GLib.Values.Fuzzy.Get (Value)
            )  )  );
         when Intuitionistic_Set =>
            Init
            (  Widget,
               Image
               (  Domain.Ptr.all,
                  Fuzzy.Intuitionistic.Set'
                  (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  )  );
         when Intuitionistic_classification =>
            Init
            (  Widget,
               Image
               (  Domain.Ptr.all,
                  Classification'
                  (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  )  );
      end case;
      Widget.Value := Copy (Value);
   end Initialize;

   function Image
            (  Editor : Entry_Edit;
               Value  : Fuzzy.Set
            )  return UTF8_String is
   begin
      return
         Fuzzy.Abstract_Edit.Image
         (  Editor.Data.Ptr.all,
            Value
         );
   end Image;

   function Image
            (  Editor : Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Set
            )  return UTF8_String is
   begin
      return
         Fuzzy.Abstract_Edit.Intuitionistic.Image
         (  Editor.Data.Ptr.all,
            Value,
            Editor.Default
         );
   end Image;

   function Image
            (  Editor : Entry_Edit;
               Value  : Classification
            )  return UTF8_String is
   begin
      return
         Fuzzy.Abstract_Edit.Intuitionistic.Image
         (  Editor.Data.Ptr.all,
            Value,
            Editor.Default
         );
   end Image;

   function Is_Editable
            (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record
            )  return Boolean is
   begin
      return Widget.Get_Editable;
   end Is_Editable;

   function Key_Press_Popup
            (  Widget : access Gtk_Widget_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Edit   : Gtk_Fuzzy_Set_Entry
            )  return Boolean is
   begin
      if Get_Editable (Edit) then
         --
         -- Key pressed while popup window is open
         --
         case Get_Event_Type (Event) is
            when Key_Press =>
               case Get_Key_Val (Event) is
                  when GDK_Insert =>
                     Edit.Canceled := False;
                     Edit.Edited   := True;
                     Edit.Set_Text (Edit.Edit.Get_Text);
                     Unset (Edit.Value);
                     Edit.Value := Edit.Selection.Get;
                     Edit.Done_Popup;
                     return True;
                  when GDK_Delete | GDK_Backspace =>
                     Edit.Canceled := True;
                     Edit.Selection.Put (Edit.Value);
                     Edit.Done_Popup;
                     return True;
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end if;
      return False;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Key_Press_Popup")
         )  );
         return False;
   end Key_Press_Popup;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             )  is
      use GLib.Values.Fuzzy;
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put
      (  Domain.Ptr.Get_Domain.Ptr.all,
         Value
      );
      Widget.Set_Text (Image (Domain.Ptr.all, Value));
      if not Is_Set (Widget.Value) then
         Unset (Widget.Value);
         Init (Widget.Value, GType_Set);
      end if;
      GLib.Values.Fuzzy.Set (Widget.Value, Value);
      Widget.Domain := Domain;
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Fuzzy.Set
             )  is
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put (Value);
      Widget.Set_Text (Image (Widget.Domain.Ptr.all, Value));
      GLib.Values.Fuzzy.Set (Widget.Value, Value);
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put (Domain.Ptr.Get_Domain.Ptr.all, Value);
      Widget.Set_Text (Image (Domain.Ptr.all, Value));
      if not Is_Set (Widget.Value) then
         Unset (Widget.Value);
         Init (Widget.Value, GType_Set);
      end if;
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
      Widget.Domain := Domain;
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put (Value);
      Widget.Set_Text (Image (Widget.Domain.Ptr.all, Value));
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : Classification
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put (Domain.Ptr.Get_Domain.Ptr.all, Value);
      Widget.Set_Text (Image (Domain.Ptr.all, Value));
      if not Is_Classification (Widget.Value) then
         Unset (Widget.Value);
         Init (Widget.Value, GType_Classification);
      end if;
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
      Widget.Domain := Domain;
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : Classification
             )  is
      Changed : constant Boolean := Widget.Being_Changed;
   begin
      Widget.Being_Changed := True;
      Widget.Selection.Put (Value);
      Widget.Set_Text (Image (Widget.Domain.Ptr.all, Value));
      GLib.Values.Fuzzy.Intuitionistic.Set (Widget.Value, Value);
      Widget.Edited := False;
      Widget.Being_Changed := Changed;
   exception
      when others =>
         Widget.Being_Changed := Changed;
         raise;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Domain : Entry_Domain;
                Value  : GValue
             )  is
   begin
      if GLib.Values.Fuzzy.Intuitionistic.Is_Defined (Value) then
         if GLib.Values.Fuzzy.Is_Set (Value) then
            Widget.Put (Domain, GLib.Values.Fuzzy.Get (Value));
         elsif GLib.Values.Fuzzy.Intuitionistic.Is_Set (Value) then
            Widget.Put
            (  Domain,
               Fuzzy.Intuitionistic.Set'
               (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  );
         else
            Widget.Put
            (  Domain,
               Classification'
               (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  );
         end if;
      else
         raise Constraint_Error;
      end if;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Value  : GValue
             )  is
   begin
      if GLib.Values.Fuzzy.Intuitionistic.Is_Defined (Value) then
         if GLib.Values.Fuzzy.Is_Set (Value) then
            Widget.Put (GLib.Values.Fuzzy.Get (Value));
         elsif GLib.Values.Fuzzy.Intuitionistic.Is_Set (Value) then
            Widget.Put
            (  Fuzzy.Intuitionistic.Set'
               (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  );
         else
            Widget.Put
            (  Classification'
               (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
            )  );
         end if;
      else
         raise Constraint_Error;
      end if;
   end Put;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Fuzzy_Set_Entry_Record;
                Editable : Boolean
             )  is
   begin
      Gtk_Entry (Widget).Set_Editable (Editable);
      Widget.Edit.Set_Editable (Editable);
      Widget.Selection.Set_Editable (Editable);
   end Set_Editable;

   procedure Set_Default
             (  Editor  : in out Entry_Edit;
                Default : Fuzzy_Boolean
             )  is
   begin
      Editor.Default := Default;
   end Set_Default;

   procedure Set_Default
             (  Widget  : not null access Gtk_Fuzzy_Set_Entry_Record;
                Default : Fuzzy_Boolean
             )  is
   begin
      Set_Default (Widget.Domain.Ptr.all, Default);
      Widget.Default_Set := True;
      Changed_Entry (Widget, Widget.all'Access);
   end Set_Default;

   procedure Set_Shape
             (  Widget : not null access Gtk_Fuzzy_Set_Entry_Record;
                Form   : Shape
             )  is
   begin
      Widget.Selection.Set_Shape (Form);
   end Set_Shape;

   procedure Start_Editing
             (  Cell_Editable : System.Address;
     	        Event         : Gdk_Event
             )  is
      Ptr : constant GObject := Convert (Cell_Editable);
   begin
      if (  Ptr /= null
         and then
            Ptr.all in Gtk_Fuzzy_Set_Entry_Record'Class
         and then
            Button_Press
            (  Gtk_Fuzzy_Set_Entry_Record'Class (Ptr.all)'Access,
               Event,
               Gtk_Fuzzy_Set_Entry_Record'Class (Ptr.all)'Access
         )  )
      then
         null;
      end if;
   end Start_Editing;

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Edit   : Gtk_Fuzzy_Set_Entry
             )  is
      use GLib.Values.Fuzzy.Logic;
   begin
      if not Edit.Default_Set then
         declare
            Default : GValue;
         begin
            Init (Default, GType_Fuzzy_Boolean);
            Style_Get_Property
            (  Widget,
               "default-truth-value",
               Default
            );
            if Is_Defined (Default) then
               Edit.Set_Default (Get (Default));
            end if;
            Unset (Default);
            Changed_Entry (Widget, Edit);
         end;
      end if;
      Get_Tree_View (Edit.Selection).Set_Headers_Visible
      (  Style_Get (Edit, "has-header")
      );
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

   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Set is
   begin
      return
         Fuzzy.Abstract_Edit.Value
         (  Text,
            Editor.Data.Ptr.all
         );
   end Value;

   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         Fuzzy.Intuitionistic.Set'
         (  Fuzzy.Abstract_Edit.Intuitionistic.Value
            (  Text,
               Editor.Data.Ptr.all,
               Editor.Default
         )  );
   end Value;

   function Value
            (  Editor : Entry_Edit;
               Text   : UTF8_String
            )  return Classification is
   begin
      return
         Classification'
         (  Fuzzy.Abstract_Edit.Intuitionistic.Value
            (  Text,
               Editor.Data.Ptr.all,
               Editor.Default
         )  );
   end Value;

end Gtk.Fuzzy_Set_Entry;
