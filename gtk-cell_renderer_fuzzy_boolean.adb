--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Cell_Renderer_Fuzzy_Boolean             Luebeck            --
--  Implementation                                 Summer, 2006       --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Confidence_Factors.Edit;        use Confidence_Factors.Edit;
with Gdk.Color;                      use Gdk.Color;
with Gdk.Event;                      use Gdk.Event;
with Gdk.RGBA;                       use Gdk.RGBA;
with Glib.Messages;                  use Glib.Messages;
with GLib.Properties;                use GLib.Properties;
with GLib.Values.Confidence_Factors; use GLib.Values.Confidence_Factors;
with GLib.Values.Fuzzy.Logic;        use GLib.Values.Fuzzy.Logic;
with GtkAda.Types;                   use GtkAda.Types;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Handlers;                   use Gtk.Handlers;
with Gtk.Missed;                     use Gtk.Missed;
with Gtk.Style;                      use Gtk.Style;
with Gtk.Style_Context;              use Gtk.Style_Context;
with Gtk.Widget.Styles;              use Gtk.Widget.Styles;

with Ada.IO_Exceptions;

package body Gtk.Cell_Renderer_Fuzzy_Boolean is

   Renderer_Type : GType := GType_Invalid;

   Editable_Undefined_ID : constant Property_ID := Truth_Value_ID + 1;

   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

   function Where (Name : String) return String is
   begin
      return " in Gtk.Cell_Renderer_Fuzzy_Boolean." & Name;
   end Where;

   procedure Class_Init (Class : GObject_Class) is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Base_Class_Init (Class);
      Install_Value_Properties (Class);
      Class_Install_Property
      (  Class,
         Editable_Undefined_ID,
         Gnew_Boolean
         (  Name    => "editable-undefined",
            Nick    => "Editable undefined",
            Blurb   => "Undefined cells are editable",
            Default => False
      )  );
   end Class_Init;

   procedure Editing_Done
             (  Editor : access Gtk_Entry_Record'Class;
                Cell   : Gtk_Cell_Renderer_Fuzzy_Boolean
             )  is
   begin
      if Cell.Focus_Out.Id /= Null_Handler_Id then
         Disconnect (Editor, Cell.Focus_Out);
         Cell.Focus_Out.Id := Null_Handler_Id;
      end if;
      Parse (Get_Text (Editor), Cell.Data, False);
      Stop_Editing (Cell, False);
      Commit (Cell);
   exception
      when Ada.IO_Exceptions.End_Error | Constraint_Error |
           Data_Error =>
         Stop_Editing (Cell, True);
   end Editing_Done;

   function Focus_Out
            (  Editor : access Gtk_Entry_Record'Class;
               Event  : Gdk.Event.Gdk_Event;
               Cell   : Gtk_Cell_Renderer_Fuzzy_Boolean
            )  return Boolean is
   begin
      Editing_Done (Editor, Cell);
      return False;
   end Focus_Out;

   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return GValue is
   begin
      return Get (Cell.Data);
   end Get;

   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Confidence is
   begin
      if not Cell.Data.Invalid and then Cell.Data.Confidence then
         return Cell.Data.Value.Possibility;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Fuzzy_Boolean is
   begin
      if not (Cell.Data.Invalid or else Cell.Data.Confidence) then
         return Cell.Data.Value;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get_Aligned_Area
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
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

   procedure Get_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      if Param_ID = Editable_Undefined_ID then
         Init (Value, GType_Boolean);
         Set_Boolean (Value, Cell.Edit_Undefined);
      else
         Get_Property (Cell.Data, Param_ID, Value, Property_Spec);
      end if;
   end Get_Property;

   function Get_Shape
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Shape is
   begin
      return Get_Shape (Cell.Data'Access, Widget);
   end Get_Shape;

   function Get_Size
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget : not null access Gtk_Widget_Record'Class
            )  return Gdk_Rectangle is
   begin
      Update (Cell.Data, Widget);
      return
         Get_Size
         (  Cell.Data,
            Get_Shape (Cell.Data'Access, Widget),
            Get_X_Pad (Cell),
            Get_Y_Pad (Cell)
         );
   end Get_Size;

   function Get_Size
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Widget : not null access Gtk_Widget_Record'Class;
               Cell_Area : Gdk_Rectangle
            )  return Gdk_Rectangle is
   begin
      Update (Cell.Data, Widget);
      return
         Get_Size
         (  Cell.Data,
            Get_Shape (Cell.Data'Access, Widget),
            Cell_Area,
            Get_X_Align (Cell),
            Get_Y_Align (Cell),
            Get_X_Pad   (Cell),
            Get_Y_Pad   (Cell)
         );
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

   procedure Gtk_New (Cell : out Gtk_Cell_Renderer_Fuzzy_Boolean) is
   begin
      Cell := new Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
      begin
         Gtk.Cell_Renderer_Fuzzy_Boolean.Initialize (Cell);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Unref (Cell);
            Cell := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Boolean_Record'Class
             )  is
   begin
      Gtk.Cell_Renderer.Abstract_Renderer.Initialize (Cell, Get_Type);
   end Initialize;

   procedure Install_Style_Properties (Class : GObject_Class) is
   begin
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "possibility-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Pos-color",
            Blurb      => "The color of possible truth value"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "necessity-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Nec-color",
            Blurb      => "The color of necessary truth values"
      )  );
      Install_Style_Property
      (  Class,
         Gnew_Boxed
         (  Name       => "error-color",
            Boxed_Type => Gdk_Color_Type,
            Nick       => "Err-color",
            Blurb      => "The color of impossible truth values"
      )  );
      Shape_Style.Install_Style
      (  Class,
         Shape_Property.Gnew_Enum
         (  Name  => "shape",
            Nick  => "Shape",
            Blurb => "The shape of the graphical representation"
      )  );
      Look_And_Feel_Style.Install_Style
      (  Class,
         Look_And_Feel_Property.Gnew_Enum
         (  Name  => "look",
            Nick  => "Look",
            Blurb => "The look & feel of the graphical representation"
      )  );
   end Install_Style_Properties;

   procedure Install_Value_Properties (Class : GObject_Class) is
   begin
      Class_Install_Property
      (  Class,
         Intuitionistic_Value_ID,
         Gnew_Boxed
         (  Name       => "fuzzy-boolean-value",
            Boxed_Type => GType_Fuzzy_Boolean,
            Nick       => "Fuzzy Boolean",
            Blurb      => "The possibility-necessity pair"
      )  );
      Class_Install_Property
      (  Class,
         Truth_Value_ID,
         Gnew_Boxed
         (  Name       => "confidence-value",
            Boxed_Type => GType_Confidence,
            Nick       => "Truth value",
            Blurb      => "The confidence"
      )  );
   end Install_Value_Properties;

   function Is_Editable_Undefined
            (  Cell : not null access
                      Gtk_Cell_Renderer_Fuzzy_Boolean_Record
            )  return Boolean is
   begin
      return Cell.Edit_Undefined;
   end Is_Editable_Undefined;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : GValue
             )  is
   begin
      Put (Cell.Data, Value);
   end Put;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : Fuzzy_Boolean
             )  is
   begin
      Put (Cell.Data, Value);
   end Put;

   procedure Put
             (  Cell  : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Value : Confidence
             )  is
   begin
      Put (Cell.Data, Value);
   end Put;

   procedure Render
             (  Cell    : not null access
                          Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Context : Cairo_Context;
                Widget  : not null access Gtk_Widget_Record'Class;
                Background_Area : Gdk_Rectangle;
                Cell_Area       : Gdk_Rectangle;
                Flags           : Gtk_Cell_Renderer_State
             )  is
      BG    : Gdk_RGBA;
      Area  : Gdk_Rectangle;
      X_Pad : constant GInt := GInt (Get_X_Pad (Cell));
      Y_Pad : constant GInt := GInt (Get_Y_Pad (Cell));
   begin
      Update (Cell.Data, Widget);
      Area.X := Cell_Area.X + X_Pad;
      Area.Y := Cell_Area.Y + Y_Pad;
      Area.Width  := Cell_Area.Width  - X_Pad * 2;
      Area.Height := Cell_Area.Height - Y_Pad * 2;
      if Get_Property (Cell, Build ("cell-background-set")) then
         BG :=
            To_RGBA
            (  Get_Property (Cell, Build ("cell-background-gdk"))
            );
      else
         if 0 /= (Flags and Cell_Renderer_Selected) then
            Get_Style_Context (Widget).Get_Background_Color
            (  Gtk_State_Flag_Selected,
               BG
            );
         else
            Get_Style_Context (Widget).Get_Background_Color
            (  Gtk_State_Flag_Normal,
               BG
            );
         end if;
      end if;
      Draw (Cell.Data, Context, Widget, Area, BG, Flags);
   end Render;

   procedure Set_Editable_Undefined
             (  Cell     : not null access
                           Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Editable : Boolean
             )  is
   begin
      Cell.Edit_Undefined := Editable;
   end Set_Editable_Undefined;

   procedure Set_Property
             (  Cell          : not null access
                                Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      if Param_ID = Editable_Undefined_ID then
         Cell.Edit_Undefined := Get_Boolean (Value);
      else
         Set_Property (Cell.Data, Param_ID, Value, Property_Spec);
      end if;
   end Set_Property;

   procedure Set_Shape
             (  Cell : not null access
                       Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
                Form : Shape
             )  is
   begin
      Cell.Data.Shape_Fixed := True;
      if Cell.Data.Form /= Form then
         Cell.Data.Form    := Form;
         Cell.Data.Updated := True;
      end if;
   end Set_Shape;

   function Start_Editing
            (  Cell   : not null access
                        Gtk_Cell_Renderer_Fuzzy_Boolean_Record;
               Event  : Gdk_Event;
               Widget : not null access Gtk_Widget_Record'Class;
               Path   : String;
               Background_Area : Gdk_Rectangle;
               Cell_Area       : Gdk_Rectangle;
               Flags           : Gtk_Cell_Renderer_State
            )  return Gtk_Widget is
      Editor : Gtk_Entry;
   begin
      if Cell.Data.Invalid and then not Cell.Edit_Undefined then
         return null;
      end if;
      Gtk_New (Editor);
      Set_Property (Editor, Build ("xalign"), Get_X_Align (Cell));
      Set_Property (Editor, Build ("has-frame"), False);
      if not Cell.Data.Invalid then
         if Cell.Data.Confidence or else
            Cell.Data.Value.Possibility = Cell.Data.Value.Necessity then
            Set_Text (Editor, Image (Cell.Data.Value.Possibility));
         else
            Set_Text
            (  Editor,
               (  Image (Cell.Data.Value.Possibility)
               &  ':'
               &  Image (Cell.Data.Value.Necessity)
            )  );
         end if;
      end if;
      Select_Region (Editor, 0, -1);
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
      Show (Editor);
      return Editor.all'Access;
   end Start_Editing;

end Gtk.Cell_Renderer_Fuzzy_Boolean;
