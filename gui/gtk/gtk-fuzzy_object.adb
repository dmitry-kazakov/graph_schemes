--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Object                            Luebeck            --
--  Implementation                                 Summer, 2008       --
--                                                                    --
--                                Last revision :  08:56 08 Apr 2022  --
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Missed;                use Gtk.Missed;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with Gtk.Main.Router;
with GtkAda.Handlers;
with GLib.Object.Checked_Destroy;
with Interfaces.C.Strings;
with Persistent.Native_ODBC;
with Persistent.SQLite;

package body Gtk.Fuzzy_Object is
   use Gtk.Widget;

   Button_Style_Updated : Boolean := False;
   Picker_Class   : aliased Ada_GObject_Class := Uninitialized_Class;
   Picker_Signals : constant Interfaces.C.Strings.Chars_Ptr_Array :=
      (0 => Interfaces.C.Strings.New_String ("picker-state-changed"));

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Object." & Name;
   end Where;

   procedure Browse
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
   begin
      Set (Box.Picker, Item_Path (Box.Edit.Get_Text));
   exception
      when others =>
         Box.Browse.Set_Sensitive (False);
         Set_Hint (Box.Hint, Box.Picker.Get_Parent, Erroneous, True);
   end Browse;

   function Check
            (  Constraint : Picker_Constraint;
               Storage    : Storage_Handle
            )  return Boolean is
   begin
      if Is_Valid (Storage) and then Constraint /= null then
         declare
            This : Item := Item (Constraint);
         begin
            loop
               if (  Is_Valid (This.Storage)
                  and then
                     This.Storage /= Storage
                  )
               then
                  if Trace_Checks then
                     Gtk.Main.Router.Trace
                     (  "Check [-] "
                     &  Image (Constraint)
                     );
                  end if;
                  return False;
               end if;
               This := Next (This);
               exit when This = Item (Constraint);
            end loop;
         end;
      end if;
      if Trace_Checks then
         Gtk.Main.Router.Trace ("Check [+] " & Image (Constraint));
      end if;
      return True;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Check")
         )  );
         return True;
   end Check;

   procedure Check
             (  Picker     : not null access Gtk_Null_Picker_Record;
                Path       : Item_Path;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      raise Picking_Error with "Null picker check";
   end Check;

   procedure Combine (Left, Right : Picker_Constraint) is
   begin
      if Left /= null and then Right /= null then
         if Is_In (Right) then
            declare
               Head : List := List (Right);
            begin
               Remove (Head, Right);
            end;
         end if;
         Insert (Left, Right);
      end if;
   end Combine;

   function Create (Name : String) return Picker_Constraint is
      Head : List;
      Item : constant Picker_Constraint :=
                      new Picker_Constraint_Object (Name'Length);
   begin
      Item.Name := Name;
      Append (Head, Item);
      return Item;
   end Create;

   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
      Self : List := List (Box.Constraint);
   begin
      Delete (Self, Item (Box.Constraint));
   end Destroy;

   procedure Free (Constraint : in out Picker_Constraint) is
      Self : List := List (Constraint);
   begin
      Delete (Self, Item (Constraint));
   end Free;

   function Get
            (  Constraint : Picker_Constraint
            )  return Storage_Handle is
      Result : Storage_Handle;
   begin
      if Constraint /= null then
         Result := Constraint.Storage;
      end if;
      return Result;
   end Get;

   procedure Get
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
   begin
      Box.Edit.Set_Text (String (Box.Picker.Get (Box.Constraint)));
   exception
      when others =>
         null;
   end Get;

   procedure Get
             (  Picker     : not null access Gtk_Null_Picker_Record;
                Constraint : Picker_Constraint;
                Store      : out Storage_Handle;
                Object     : out Deposit_Handle
             )  is
   begin
      raise Picking_Error with "Null picker get";
   end Get;

   function Get
            (  Picker     : not null access Gtk_Null_Picker_Record;
               Constraint : Picker_Constraint
            )  return Item_Path is
   begin
      return "";
   end Get;

   function Get_Constraint
            (  Widget : not null access Gtk_Picker_Box_Record'Class
            )  return Picker_Constraint is
   begin
      return Widget.Constraint;
   end Get_Constraint;

   function Get_Entry (Widget : not null access Gtk_Picker_Box_Record)
      return Gtk_Entry is
   begin
      return Widget.Edit;
   end Get_Entry;

   function Get_Name (Constraint : Picker_Constraint) return String is
   begin
      if Constraint = null then
         return "";
      else
         return Constraint.Name;
      end if;
   end Get_Name;

   function Get_Parent
            (  Picker : not null access Gtk_Null_Picker_Record
            )  return Gtk_Widget is
   begin
      return Picker.Parent;
   end Get_Parent;

   function Get_Path (Widget : not null access Gtk_Picker_Box_Record)
      return Item_Path is
   begin
      return Item_Path (Get_Text (Widget.Edit));
   end Get_Path;

   function Get_Path
            (  Picker : not null access Gtk_Null_Picker_Record;
               Object : Deposit_Handle
            )  return Item_Path is
   begin
      return "";
   end Get_Path;

   function Get_Picker (Widget : not null access Gtk_Picker_Box_Record)
      return Gtk_Object_Picker is
   begin
      return Widget.Picker;
   end Get_Picker;

   function Get_Type return Gtk_Type is
   begin
      Initialize_Class_Record
      (  Ancestor     => Gtk.Widget.Get_Type,
         Class_Record => Picker_Class,
         Type_Name    => Picker_Class_Name,
         Signals      => Picker_Signals
      );
      return Picker_Class.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Picker : out Gtk_Null_Picker;
                Parent : not null access Gtk_Widget_Record'Class
             )  is
   begin
      Picker := new Gtk_Null_Picker_Record;
      begin
         Initialize (Picker, Parent);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Picker);
            Picker := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget     : out Gtk_Picker_Box;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Storage_Handle;
                Editable   : Boolean := True;
                Initialize : Boolean := True
             )  is
   begin
      Widget := new Gtk_Picker_Box_Record;
      begin
         Gtk.Fuzzy_Object.Initialize
         (  Widget     => Widget,
            Name       => Name,
            Hint       => Hint,
            Picker     => Picker,
            Constraint => Constraint,
            Editable   => Editable,
            Initialize => Initialize
         );
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

   procedure Gtk_New
             (  Widget     : out Gtk_Picker_Box;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Editable   : Boolean := True;
                Initialize : Boolean := True
             )  is
      Constraint : Storage_Handle;
   begin
      Gtk_New
      (  Widget     => Widget,
         Name       => Name,
         Hint       => Hint,
         Picker     => Picker,
         Constraint => Constraint,
         Editable   => Editable,
         Initialize => Initialize
      );
   end Gtk_New;

   function Image (Constraint : Picker_Constraint) return String is
   begin
      if Constraint = null then
         return "none";
      else
         declare
            Result : Unbounded_String;
            This   : Item := Item (Constraint);
         begin
            loop
               if Length (Result) > 0 then
                  Append (Result, '|');
               end if;
               Append (Result, This.Name);
               Append (Result, '=');
               if Is_Valid (This.Storage) then
                  if Persistent.Native_ODBC.Is_ODBC (This.Storage) then
                     Append
                     (  Result,
                        Persistent.Native_ODBC.Get_Server_Name
                        (  This.Storage
                     )  );
                  elsif Persistent.SQLite.Is_SQLite (This.Storage) then
                     Append
                     (  Result,
                        Persistent.SQLite.Get_File_Name (This.Storage)
                     );
                  else
                     Append (Result, "unknown");
                  end if;
               else
                  Append (Result, '*');
               end if;
               This := Next (This);
               exit when This = Item (Constraint);
            end loop;
            return To_String (Result);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Image fault: "
            &  Exception_Information (Error)
            &  Where ("Image")
         )  );
         return "error";
   end Image;

   procedure Initialize
             (  Picker : not null access Gtk_Null_Picker_Record'Class;
                Parent : not null access Gtk_Widget_Record'Class
             )  is
   begin
      Gtk.Fuzzy_Object.Initialize
      (  Gtk_Object_Picker_Record (Picker.all)'Unchecked_Access
      );
      Picker.Parent := Parent.all'Unchecked_Access;
   end Initialize;

   procedure Initialize
             (  Widget     : not null access
                             Gtk_Picker_Box_Record'Class;
                Name       : String;
                Hint       : out Gtk_Box;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Storage_Handle;
                Editable   : Boolean;
                Initialize : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      Initialize_HBox (Widget);
      Widget.Picker := Picker.all'Access;
      Gtk_New (Widget.Browse);
      if not Button_Style_Updated then
         Set_Button_Style (Browse_Buttons.Class);
         Set_Button_Style (Get_Buttons.Class);
         Button_Style_Updated := True;
      end if;
      Set_Sensitive (Widget.Browse, False);
      Pack_Start (Widget, Widget.Browse, False, False);
      Gtk_New (Widget.Edit);
      Pack_Start (Widget, Widget.Edit);
      Gtk_New_HBox (Widget.Hint);
      Hint := Widget.Hint.all'Unchecked_Access;
      Widget.Constraint := Create (Name);
      Widget.Constraint.Storage := Constraint;
      Folder_Box_Handlers.Connect
      (  Widget.Browse,
         "clicked",
         Browse'Access,
         Widget.all'Unchecked_Access
      );
      Folder_Box_Handlers.Connect
      (  Widget.Edit,
         "changed",
         Set'Access,
         Widget.all'Unchecked_Access
      );
      Folder_Box_Handlers.Connect
      (  Widget,
         "destroy",
         Destroy'Access,
         Widget.all'Unchecked_Access
      );
      Set
      (  Widget.Style_Handler,
         Folder_Box_Handlers.Connect
         (  Picker.Get_Parent,
            "style-updated",
            Folder_Box_Handlers.To_Marshaller (Style_Updated'Access),
            Widget.all'Unchecked_Access
      )  );
      Set
      (  Widget.State_Handler,
         Folder_Box_Handlers.Connect
         (  Picker,
            "picker-state-changed",
            Folder_Box_Handlers.To_Marshaller (State_Changed'Access),
            Widget.all'Unchecked_Access
      )  );
      if Initialize then
         Widget.Edit.Set_Text (String (Picker.Get (Widget.Constraint)));
      end if;
      Widget.Set_Editable_Mode (Editable);
      if Initialize then
         State_Changed (Widget, Widget.all'Unchecked_Access);
      end if;
      Style_Updated (Picker.Get_Parent, Widget.all'Unchecked_Access);
   end Initialize;

   procedure Initialize
             (  Picker : not null access Gtk_Object_Picker_Record'Class
             )  is
   begin
      G_New (Picker, Get_Type);
      --Gtk.Widget.Initialize (Picker);
   end Initialize;

   procedure Install_Hints_Style_Properties
             (  Class : Ada_GObject_Class
             )  is
   begin
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "none-image",
            Nick    => "None image",
            Blurb   => (  "Stock item name used to mark fields "
                       &  "of the factory requiring editing"
                       ),
            Default => "gtk-edit"
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "conflicting-image",
            Nick    => "Conflicting image",
            Blurb   => (  "Stock item name used to mark conflicting "
                       &  "fields of the factory"
                       ),
            Default => Stock_Cancel
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "checked-image",
            Nick    => "Checked image",
            Blurb   => (  "Stock item name used to mark successfully "
                       &  "checked fields of the factory"
                       ),
            Default => Stock_Apply
      )  );
      Install_Style_Property
      (  Class_Ref (Class.The_Type),
         Gnew_String
         (  Name    => "erroneous-image",
            Nick    => "Erroneous image",
            Blurb   => (  "Stock item name used to mark erroneous "
                       &  "fields of the factory"
                       ),
            Default => Stock_Stop
      )  );
   end Install_Hints_Style_Properties;

   function Is_Editable
            (  Widget : not null access Gtk_Picker_Box_Record
            )  return Boolean is
   begin
      return Is_Sensitive (Widget.Edit);
   end Is_Editable;

   procedure Reset (Constraint : Picker_Constraint) is
   begin
      Invalidate (Constraint.Storage);
   end Reset;

   procedure Set
             (  Constraint : Picker_Constraint;
                Storage    : Storage_Handle
             )  is
   begin
      Constraint.Storage := Storage;
   end Set;

   procedure Set
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
      Saved  : constant Storage_Handle := Box.Constraint.Storage;
      Store  : Storage_Handle;
      Object : Deposit_Handle;
   begin
      Invalidate (Box.Constraint.Storage);
      Check
      (  Box.Picker,
         Item_Path (Box.Edit.Get_Text),
         Box.Constraint,
         Store,
         Object
      );
      if Is_Valid (Store) then
         Box.Constraint.Storage := Store;
         Set_Hint (Box.Hint, Box.Picker.Get_Parent, Checked, True);
         Box.Browse.Set_Sensitive (True);
      else
         Set_Hint (Box.Hint, Box.Picker.Get_Parent, Erroneous, True);
         Box.Browse.Set_Sensitive (False);
      end if;
   exception
      when Picking_Error =>
         Box.Constraint.Storage := Saved;
         Set_Hint
         (  Box.Hint,
            Box.Picker.Get_Parent,
            Conflicting,
            True
         );
         Box.Browse.Set_Sensitive (False);
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set")
         )  );
   end Set;

   procedure Set
             (  Picker : not null access Gtk_Null_Picker_Record;
                Store  : Storage_Handle;
                Object : Deposit_Handle
             )  is
   begin
      null;
   end Set;

   procedure Set
             (  Picker : not null access Gtk_Null_Picker_Record;
                Path   : Item_Path
             )  is
   begin
      null;
   end Set;

   procedure Set_Editable_Mode
             (  Widget   : not null access Gtk_Picker_Box_Record;
                Editable : Boolean
             )  is
   begin
      if Editable then
         Widget.Edit.Set_Sensitive (True);
         Gtk_New (Widget.Get);
         Widget.Pack_Start (Widget.Get, False, False);
         Folder_Box_Handlers.Connect
         (  Widget.Get,
            "clicked",
            Get'Access,
            Widget.all'Unchecked_Access
         );
      else
         Widget.Edit.Set_Sensitive (False);
         if Widget.Get /= null then
            Remove (Widget, Widget.Get);
            Widget.Get := null;
         end if;
      end if;
   end Set_Editable_Mode;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Picker_Box_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         Widget.Set_Editable_Mode (Editable);
      end if;
   end Set_Editable;

   procedure Set_Hint
             (  Box    : not null access Gtk_Box_Record'Class;
                Parent : not null access Gtk_Widget_Record'Class;
                Hint   : Hint_Type;
                Show   : Boolean
             )  is
      Image : Gtk_Image;
   begin
      Erase (Box);
      if Show then
         case Hint is
            when None =>
               Gtk_New
               (  Image,
                  Style_Get (Parent, "none-image"),
                  Icon_Size_Menu
               );
            when Checked =>
               Gtk_New
               (  Image,
                  Style_Get (Parent, "checked-image"),
                  Icon_Size_Menu
               );
            when Conflicting =>
               Gtk_New
               (  Image,
                  Style_Get (Parent, "conflicting-image"),
                  Icon_Size_Menu
               );
            when Erroneous =>
               Gtk_New
               (  Image,
                  Style_Get (Parent, "erroneous-image"),
                  Icon_Size_Menu
               );
         end case;
         Box.Pack_Start (Image, False, False);
      end if;
      Box.Show_All;
   end Set_Hint;

   procedure Set_Hint
             (  Widget : not null access Gtk_Picker_Box_Record;
                Parent : not null access Gtk_Widget_Record'Class;
                Hint   : Hint_Type;
                Show   : Boolean
             )  is
   begin
      if Widget.Hint /= null then
         Set_Hint (Widget.Hint, Parent, Hint, Show);
      end if;
   end Set_Hint;

   procedure Set_Path
             (  Widget : not null access Gtk_Picker_Box_Record;
                Path   : Item_Path
             )  is
   begin
      Widget.Edit.Set_Text (UTF8_String (Path));
   end;

   procedure State_Changed
             (  Picker : not null access Gtk_Object_Picker_Record
             )  is
   begin
      GtkAda.Handlers.Widget_Callback.Emit_By_Name
      (  Picker,
         "picker-state-changed"
      );
   end State_Changed;

   procedure State_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
   begin
      if Box.Get /= null then
         begin
            Box.Get.Set_Sensitive
            (  "" /= Box.Picker.Get (Box.Constraint)
            );
         exception
            when others =>
               Box.Get.Set_Sensitive (False);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("State_Changed")
         )  );
   end State_Changed;

   procedure Style_Updated
             (  Widget : access Gtk_Widget_Record'Class;
                Box    : Gtk_Picker_Box
             )  is
   begin
      Box.Set_Spacing (Style_Get (Widget, "button-spacing"));
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

end Gtk.Fuzzy_Object;
