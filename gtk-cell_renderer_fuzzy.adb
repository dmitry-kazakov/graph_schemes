--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy                     Luebeck            --
--  Implementation                                 Summer, 2007       --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Confidence_Factors;    use Confidence_Factors;
with Gdk.Color;             use Gdk.Color;
with GLib;                  use GLib;
with GLib.Messages;         use GLib.Messages;
with GLib.Properties;       use GLib.Properties;
with Glib.Values.Handling;  use Glib.Values.Handling;
with GtkAda.Types;          use GtkAda.Types;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Missed;            use Gtk.Missed;
with Gtk.Style;             use Gtk.Style;
with Gtk.Style_Context;     use Gtk.Style_Context;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Widget.Styles;     use Gtk.Widget.Styles;

with Fuzzy.Abstract_Edit.Intuitionistic;
use  Fuzzy.Abstract_Edit.Intuitionistic;

with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;
with Gdk.Window;
with GLib.Values.Fuzzy.Intuitionistic;
with GLib.Values.Fuzzy.Logic;
with Gtk.Fuzzy_Boolean_Drawing;

package body Gtk.Cell_Renderer_Fuzzy is
   use Entry_Edit_Handles;

   Renderer_Type : GType := GType_Invalid;

   type Value_Mode is
        (  Fuzzy_Set,
           Classification,
           Intuitionistic_Set,
           Text,
           Other
        );

   function Where (Name : String) return String is
   begin
      return " in Gtk.Cell_Renderer_Fuzzy." & Name;
   end Where;

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   procedure Class_Init (Class : GObject_Class) is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Base_Class_Init (Class);
      Class_Install_Property
      (  Class,
         Default_ID,
         Gnew_Boxed
         (  Name       => "default-truth-value",
            Boxed_Type => GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean,
            Nick       => "Default",
            Blurb      =>
               (  "Default for undefined possibility and necessity "
               &  "components while I/O of intuitionistic objects"
      )  )     );
      Class_Install_Property
      (  Class,
         Editable_Undefined_ID,
         Gnew_Boolean
         (  Name    => "editable-undefined",
            Nick    => "Editable undefined",
            Blurb   => "Undefined cells are editable",
            Default => False
      )  );
      Class_Install_Property
      (  Class,
         Set_ID,
         Gnew_Boxed
         (  Name       => "set-value",
            Boxed_Type => GLib.Values.Fuzzy.GType_Set,
            Nick       => "Fuzzy Set",
            Blurb      => "The fuzzy set"
      )  );
      Class_Install_Property
      (  Class,
         Set_ID,
         Gnew_Boxed
         (  Name       => "intuitionistic-set-value",
            Boxed_Type => GLib.Values.Fuzzy.Intuitionistic.GType_Set,
            Nick       => "Intuitionistic Fuzzy Set",
            Blurb      => "The intuitionistic fuzzy set"
      )  );
      Class_Install_Property
      (  Class,
         Set_ID,
         Gnew_Boxed
         (  Name       => "classification-value",
            Boxed_Type => GLib.Values.Fuzzy.Intuitionistic.
                          GType_Classification,
            Nick       => "Classification",
            Blurb      => "The intuitionistic fuzzy classification"
      )  );
      Class_Install_Property
      (  Class,
         Prefix_ID,
         Gnew_String
         (  Name    => "prefix-text",
            Nick    => "Prefix for output values",
            Default => "",
            Blurb   => "The text shown in front of a fuzzy value"
      )  );
   end Class_Init;

   procedure Editing_Done
             (  Editor : access Gtk_Fuzzy_Set_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fuzzy
             )  is
   begin
      if Cell.Focus_Out.Id /= Null_Handler_Id then
         Disconnect (Editor, Cell.Focus_Out);
         Cell.Focus_Out.Id := Null_Handler_Id;
      end if;
      if Editing_Canceled (Editor) then
         Stop_Editing (Cell, True);
      else
         declare
            Value : GValue := Get (Editor);
         begin
            Cell.Put (Get_Domain (Editor), Value);
            Unset (Value);
            Stop_Editing (Cell, False);
            Cell.Commit;
         exception
            when others =>
               Unset (Value);
               Stop_Editing (Cell, True);
         end;
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Editing_Done")
         )  );
   end Editing_Done;

   procedure Finalize
             (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
             )  is
      use Gtk.Cell_Renderer.Abstract_Renderer;
   begin
      if Cell.Text /= null then
         Unref (Cell.Text);
      end if;
      Free  (Cell.Prefix);
      Unset (Cell.Value);
      Finalize (Gtk_Abstract_Renderer_Record (Cell.all)'Access);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Finalize")
         )  );
   end Finalize;

   function Focus_Out
            (  Editor : access Gtk_Fuzzy_Set_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fuzzy
            )  return Boolean is
   begin
      Editing_Done (Editor, Cell);
      return False;
   end Focus_Out;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Set is
   begin
      return GLib.Values.Fuzzy.Get (Cell.Value);
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return GLib.Values.Fuzzy.Intuitionistic.Get (Cell.Value);
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return GLib.Values.Fuzzy.Intuitionistic.Get (Cell.Value);
   end Get;

   function Get
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return GValue is
   begin
      return Copy (Cell.Value);
   end Get;

   function Get_Aligned_Area
            (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Flags  : Gtk_Cell_Renderer_State;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
      Area   : constant Gdk_Rectangle :=
               Cell.Get_Size (Widget, Cell_Area);
      Result : Gdk_Rectangle;
   begin
      Result.X := Cell_Area.X + GInt (Get_X_Pad (Cell)) + Area.X;
      Result.Y := Cell_Area.Y + GInt (Get_Y_Pad (Cell)) + Area.Y;
      Result.Width :=
         GInt'Min
         (  Result.X - Cell_Area.X + Cell_Area.Width,
            Area.Width
         );
      Result.Height :=
         GInt'Min
         (  Result.Y - Cell_Area.Y + Cell_Area.Height,
            Area.Height
         );
      return Result;
   end Get_Aligned_Area;

   function Get_Cardinality
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Natural is
   begin
      return Get_Cardinality (Ptr (Cell.Domain).all);
   end Get_Cardinality;

   function Get_Domain
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Entry_Domain is
   begin
      return Cell.Domain;
   end Get_Domain;

   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Default_ID =>
            Init (Value, GLib.Values.Fuzzy.Logic.GType_Fuzzy_Boolean);
            GLib.Values.Fuzzy.Logic.Set (Value, Cell.Default);
         when Editable_Undefined_ID =>
            Init (Value, GType_Boolean);
            Set_Boolean (Value, Cell.Edit_Undefined);
         when Classification_ID =>
            if GLib.Values.Fuzzy.Intuitionistic.
               Is_Classification (Cell.Value) then
               Value := Copy (Cell.Value);
            else
               Init
               (  Value,
                  GLib.Values.Fuzzy.Intuitionistic.GType_Classification
               );
               GLib.Values.Fuzzy.Intuitionistic.Set_Undefined (Value);
            end if;
         when Intuitionistic_Set_ID =>
            if GLib.Values.Fuzzy.Intuitionistic.Is_Set (Cell.Value) then
               Value := Copy (Cell.Value);
            else
               Init
               (  Value,
                  GLib.Values.Fuzzy.Intuitionistic.GType_Set
               );
               GLib.Values.Fuzzy.Intuitionistic.Set_Undefined (Value);
            end if;
         when Set_ID =>
            if GLib.Values.Fuzzy.Is_Set (Cell.Value) then
               Value := Copy (Cell.Value);
            else
               Init (Value, GLib.Values.Fuzzy.GType_Set);
               GLib.Values.Fuzzy.Set_Undefined (Value);
            end if;
         when Prefix_ID =>
            Init (Value, GType_String);
            Set_String (Value, Cell.Prefix (1..Cell.Prefix_Length));
         when others =>
            Init (Value, GType_Invalid);
      end case;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Get_Property")
         )  );
   end Get_Property;

   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Rectangle is
      Area : Gdk_Rectangle;
   begin
      Update (Cell, Widget);
      Get_Pixel_Size (Cell.Text, Area.Width, Area.Height);
      Area.X := 0;
      Area.Y := 0;
      return Area;
   end Get_Size;

   function Get_Size
            (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
      Result : Gdk_Rectangle := Get_Size (Cell, Widget);
   begin
      Result.X :=
         GInt'Max
         (  GInt
            (  Get_X_Align (Cell)
            *  GFloat (Cell_Area.Width - Result.Width)
            ),
            0
         );
      Result.Y :=
         GInt'Max
         (  GInt
            (  Get_Y_Align (Cell)
            *  GFloat (Cell_Area.Height - Result.Height)
            ),
            0
         );
      return Result;
   end Get_Size;

   function Get_Type return Gtk_Type is
   begin
     if Renderer_Type = GType_Invalid then
        Renderer_Type :=
           Gtk.Cell_Renderer.Abstract_Renderer.Register
           (  Class_Name,
              Class_Init'Access
           );
     end if;
     return Renderer_Type;
   end Get_Type;

   procedure Gtk_New
             (  Cell       : out Gtk_Cell_Renderer_Fuzzy;
                Domain     : Entry_Domain;
                Value_Type : Gtk_Type := GLib.Values.Fuzzy.GType_Set
             )  is
   begin
      Cell := new Gtk_Cell_Renderer_Fuzzy_Record;
      begin
         Initialize (Cell, Domain, Get_Type, Value_Type);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Unref (Cell);
            Cell := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Cell       : not null access
                             Gtk_Cell_Renderer_Fuzzy_Record'Class;
                Domain     : Entry_Domain;
                Cell_Type  : Gtk_Type;
                Value_Type : Gtk_Type
             )  is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Initialize (Cell, Cell_Type);
      Init (Cell.Value, Value_Type);
      GLib.Values.Fuzzy.Intuitionistic.Set_Undefined (Cell.Value);
      Cell.Domain := Domain;
   end Initialize;

   function Is_Editable_Undefined
            (  Cell : not null access Gtk_Cell_Renderer_Fuzzy_Record
            )  return Boolean is
   begin
      return Cell.Edit_Undefined;
   end Is_Editable_Undefined;

   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : GValue
             )  is
      Value_Type : constant GType := Get_Type (Value);
   begin
      if GLib.Values.Fuzzy.Intuitionistic.Is_Classification (Value) then
         Put
         (  Cell,
            Fuzzy.Intuitionistic.Classification'
            (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
         )  );
         Cell.Updated := True;
         return;
      elsif GLib.Values.Fuzzy.Intuitionistic.Is_Set (Value) then
         Put
         (  Cell,
            Fuzzy.Intuitionistic.Set'
            (  GLib.Values.Fuzzy.Intuitionistic.Get (Value)
         )  );
         Cell.Updated := True;
         return;
      elsif GLib.Values.Fuzzy.Is_Set (Value) then
         Put (Cell, Fuzzy.Set'(GLib.Values.Fuzzy.Get (Value)));
         Cell.Updated := True;
         return;
      elsif GType_String = Value_Type then
         declare
            Mode : constant GType := Get_Type (Cell.Value);
         begin
            if Mode = GLib.Values.Fuzzy.Intuitionistic.
                      GType_Classification then
               Put
               (  Cell,
                  Fuzzy.Intuitionistic.Classification'
                  (  Gtk.Fuzzy_Set_Entry.Value
                     (  Ptr (Cell.Domain).all,
                        Get_String (Value)
               )  )  );
               Cell.Updated := True;
               return;
            elsif Mode = GLib.Values.Fuzzy.Intuitionistic.GType_Set then
               Put
               (  Cell,
                  Fuzzy.Intuitionistic.Set'
                  (  Gtk.Fuzzy_Set_Entry.Value
                     (  Ptr (Cell.Domain).all,
                        Get_String (Value)
               )  )  );
               Cell.Updated := True;
               return;
            else
               Put
               (  Cell,
                  Fuzzy.Set'
                  (  Gtk.Fuzzy_Set_Entry.Value
                     (  Ptr (Cell.Domain).all,
                        Get_String (Value)
               )  )  );
               Cell.Updated := True;
               return;
            end if;
         exception
            when others =>
               null;
         end;
      end if;
      Unset (Cell.Value);
      Init (Cell.Value, Value_Type);
      Cell.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Set
             )  is
      use GLib.Values.Fuzzy;
   begin
      if Get_Type (Cell.Value) /= GType_Set then
         Unset (Cell.Value);
         Init (Cell.Value, GType_Set);
      end if;
      if Get_Cardinality (Cell) = Value'Length then
         GLib.Values.Fuzzy.Set (Cell.Value, Value);
      else
         Set_Undefined (Cell.Value);
      end if;
      Cell.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Intuitionistic.Set
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      if Get_Type (Cell.Value) /= GType_Set then
         Unset (Cell.Value);
         Init (Cell.Value, GType_Set);
      end if;
      if Get_Cardinality (Cell) = Value.Cardinality then
         GLib.Values.Fuzzy.Intuitionistic.Set (Cell.Value, Value);
      else
         Set_Undefined (Cell.Value);
      end if;
      Cell.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Cell  : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Value : Fuzzy.Intuitionistic.Classification
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      if Get_Type (Cell.Value) /= GType_Classification then
         Unset (Cell.Value);
         Init (Cell.Value, GType_Classification);
      end if;
      if Get_Cardinality (Cell) = Value.Cardinality then
         GLib.Values.Fuzzy.Intuitionistic.Set (Cell.Value, Value);
      else
         Set_Undefined (Cell.Value);
      end if;
      Cell.Updated := True;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Put")
         )  );
   end Put;

   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : GValue
             )  is
   begin
      if not Is_Valid (Domain) then
         raise Constraint_Error;
      end if;
      Cell.Domain := Domain;
      Put (Cell, Value);
   end Put;

   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Set
             )  is
   begin
      if not Is_Valid (Domain) then
         raise Constraint_Error;
      end if;
      Cell.Domain := Domain;
      Put (Cell, Value);
   end Put;

   procedure Put
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Set
             )  is
   begin
      if not Is_Valid (Domain) then
         raise Constraint_Error;
      end if;
      Cell.Domain := Domain;
      Put (Cell, Value);
   end Put;

   procedure Put
             (  Cell   : access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain;
                Value  : Fuzzy.Intuitionistic.Classification
             )  is
   begin
      if not Is_Valid (Domain) then
         raise Constraint_Error;
      end if;
      Cell.Domain := Domain;
      Put (Cell, Value);
   end Put;

   procedure Set_Domain
             (  Cell   : not null access Gtk_Cell_Renderer_Fuzzy_Record;
                Domain : Entry_Domain
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      if not Is_Valid (Domain) then
         raise Constraint_Error;
      end if;
      Set_Undefined (Cell.Value);
      Cell.Domain  := Domain;
      Cell.Updated := True;
   end Set_Domain;

   procedure Render
             (  Cell    : not null access
                          Gtk_Cell_Renderer_Fuzzy_Record;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             )  is
      Area  : constant Gdk_Rectangle :=
              Cell.Get_Size (Widget, Cell_Area);
      Style : constant Gtk_Style_Context := Get_Style_Context (Widget);
   begin
      Cell.Update (Widget);
      Save (Context);
      Rectangle
      (  Context,
         GDouble (Cell_Area.X),
         GDouble (Cell_Area.Y),
         GDouble (Cell_Area.Width),
         GDouble (Cell_Area.Height)
      );
      Clip (Context);
      Render_Layout
      (  Style,
         Context,
         GDouble (Cell_Area.X + GInt (Get_X_Pad (Cell)) + Area.X),
         GDouble (Cell_Area.Y + GInt (Get_Y_Pad (Cell)) + Area.Y),
         Cell.Text
      );
      Restore (Context);
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Render")
         )  );
   end Render;

   procedure Set_Editable_Undefined
             (  Cell     : not null access
                           Gtk_Cell_Renderer_Fuzzy_Record;
                Editable : Boolean
             )  is
   begin
      Cell.Edit_Undefined := Editable;
   end Set_Editable_Undefined;

   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      case Param_ID is
         when Default_ID =>
            Cell.Default := GLib.Values.Fuzzy.Logic.Get (Value);
            Cell.Updated := True;
         when Editable_Undefined_ID =>
            Cell.Edit_Undefined := Get_Boolean (Value);
         when Classification_ID | Intuitionistic_Set_ID | Set_ID =>
            Put (Cell, Value);
         when Prefix_ID =>
            declare
               Text : String renames Get_String (Value);
            begin
               if Cell.Prefix'Length >= Text'Length then
                  Cell.Prefix (1..Text'Length) := Text;
               else
                  Free (Cell.Prefix);
                  Cell.Prefix := new String'(Text);
               end if;
               Cell.Prefix_Length := Text'Length;
            end;
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
            &  Where ("Set_Property")
         )  );
   end Set_Property;

   function Start_Editing
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Record;
               Event  : Gdk_Event;
               Widget : not null access Gtk_Widget_Record'Class;
               Path   : String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget is
      Editor : Gtk_Fuzzy_Set_Entry;
   begin
      Cell.Update (Widget);
      if GLib.Values.Fuzzy.Intuitionistic.Is_Defined (Cell.Value) then
         Gtk_New (Editor, Cell.Domain, Cell.Value);
      else
         --
         -- The value is undefined, let's use an empty value instead but
         -- only if editing empty values is alloved.
         --
         if not Cell.Edit_Undefined then
            return null;
         end if;
         declare
            Mode  : constant GType := Get_Type (Cell.Value);
            Empty : GValue;
         begin
            if Mode = GLib.Values.Fuzzy.Intuitionistic.
                      GType_Classification
            then
               Init (Empty, Mode);
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Empty,
                  Fuzzy.Intuitionistic.Classification'
                  (  Cardinality => Get_Cardinality (Cell),
                     Possibility => (others => Confidence'First),
                     Necessity   => (others => Confidence'First)
               )  );
            elsif Mode = GLib.Values.Fuzzy.Intuitionistic.GType_Set then
               Init (Empty, Mode);
               GLib.Values.Fuzzy.Intuitionistic.Set
               (  Empty,
                  Fuzzy.Intuitionistic.Set'
                  (  Cardinality => Get_Cardinality (Cell),
                     Possibility => (others => Confidence'First),
                     Necessity   => (others => Confidence'First)
               )  );
            elsif Mode = GLib.Values.Fuzzy.GType_Set then
               Init (Empty, Mode);
               GLib.Values.Fuzzy.Set
               (  Empty,
                  Fuzzy.Set'
                  (  1..Get_Cardinality (Cell) => Confidence'First
               )  );
            else
               return null;
            end if;
            Gtk_New (Editor, Cell.Domain, Empty);
            Unset (Empty);
         exception
            when others =>
               Unset (Empty);
               return null;
         end;
      end if;
      Editor.Set_Default (Cell.Default);
      Set_Property (Editor, Build ("xalign"), Get_X_Align (Cell));
      Set_Property (Editor, Build ("has-frame"), False);
      Editor.Select_Region (0, -1);
      Entry_Callbacks.Connect
      (  Editor,
         "editing_done",
         Entry_Callbacks.To_Marshaller (Editing_Done'Access),
         Cell.all'Access
      );
      Cell.Focus_Out :=
         Entry_Return_Callbacks.Connect
         (  Editor,
            "focus_out_event",
            Entry_Return_Callbacks.To_Marshaller (Focus_Out'Access),
            Cell.all'Access
         );
      Editor.Show;
      return Editor.all'Access;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Property")
         )  );
         return null;
   end Start_Editing;

   procedure Update
             (  Cell   : not null access
                         Gtk_Cell_Renderer_Fuzzy_Record'Class;
                Widget : not null access Gtk_Widget_Record'Class
             )  is
      use GLib.Values.Fuzzy.Intuitionistic;
   begin
      if not Cell.Updated then
         return;
      end if;
      Cell.Updated := False;
      if Cell.Text = null then
         Cell.Text := Create_Pango_Layout (Widget);
      end if;
      if Is_Classification (Cell.Value) then
         Cell.Text.Set_Text
         (  Cell.Prefix (1..Cell.Prefix_Length)
         &  Image
            (  Ptr (Cell.Domain).all,
               Fuzzy.Intuitionistic.Classification'(Get (Cell.Value))
         )  );
      elsif Is_Set (Cell.Value) then
         Cell.Text.Set_Text
         (  Cell.Prefix (1..Cell.Prefix_Length)
         &  Image
            (  Ptr (Cell.Domain).all,
               Fuzzy.Intuitionistic.Set'(Get (Cell.Value))
         )  );
      elsif GLib.Values.Fuzzy.Is_Set (Cell.Value) then
         Cell.Text.Set_Text
         (  Cell.Prefix (1..Cell.Prefix_Length)
         &  Image
            (  Ptr (Cell.Domain).all,
               GLib.Values.Fuzzy.Get (Cell.Value)
         )  );
      else
         Cell.Text.Set_Text (Cell.Prefix (1..Cell.Prefix_Length));
      end if;
   exception
      when Error : others =>
         Log
         (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Update")
         )  );
   end Update;

end Gtk.Cell_Renderer_Fuzzy;
