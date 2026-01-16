--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Boolean                           Luebeck            --
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

with Ada.Exceptions;     use Ada.Exceptions;
with Cairo;              use Cairo;
with GLib.Messages;      use GLib.Messages;
with GLib.Types;         use GLib.Types;
with Gdk.Event;          use Gdk.Event;
with Gdk.Rectangle;      use Gdk.Rectangle;
with Gdk.RGBA;           use Gdk.RGBA;
with Gdk.Window;         use Gdk.Window;
with GtkAda.Types;       use GtkAda.Types;
with Gtk.Enums;          use Gtk.Enums;
with Gtk.Handlers;       use Gtk.Handlers;
with Gtk.Missed;         use Gtk.Missed;
with Gtk.Style_Context;  use Gtk.Style_Context;

with GNAT.Traceback.Symbolic;
with Gtk.Cell_Renderer_Fuzzy_Boolean;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Boolean is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Boolean." & Name;
   end Where;

   procedure Class_Init (Class : GObject_Class);
   pragma Convention (C, Class_Init);

   procedure Class_Init (Class : GObject_Class) is
      use Gtk.Cell_Renderer_Fuzzy_Boolean;
   begin
      Set_Properties_Handlers
      (  Class,
         Set_Property'Access,
         Get_Property'Access
      );
      Install_Value_Properties (Class);
      Install_Style_Properties (Class);
   end Class_Init;

   function Draw
            (  Widget  : access Gtk_Fuzzy_Boolean_Record'Class;
               Context : Cairo_Context
            )  return Boolean is
      BG : Gdk_RGBA;
   begin
      Get_Style_Context (Widget).Get_Background_Color
      (  Widget.Get_State_Flags,
         BG
      );
      Widget.Data.Update (Widget);
      Draw
      (  Data       => Widget.Data,
         Context    => Context,
         Widget     => Widget,
         Background => BG,
         Area       => (  X      => 0,
                          Y      => 0,
                          Width  => Widget.Get_Allocated_Width,
                          Height => Widget.Get_Allocated_Height
      )               );
      return True;
   end Draw;

   function Get (Widget : not null access Gtk_Fuzzy_Boolean_Record)
      return Confidence is
   begin
      if not Widget.Data.Invalid and then Widget.Data.Confidence then
         return Widget.Data.Value.Possibility;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get (Widget : not null access Gtk_Fuzzy_Boolean_Record)
      return Fuzzy.Logic.Fuzzy_Boolean is
   begin
      if not (Widget.Data.Invalid or else Widget.Data.Confidence) then
         return Widget.Data.Value;
      else
         raise Constraint_Error;
      end if;
   end Get;

   function Get (Widget : not null access Gtk_Fuzzy_Boolean_Record)
      return GValue is
   begin
      return Widget.Data.Get;
   end Get;

   procedure Get_Property
             (  Widget        : access GObject_Record'Class;
                Param_ID      : Property_ID;
                Value         : out GValue;
                Property_Spec : Param_Spec
             )  is
   begin
      Get_Property
      (  Gtk_Fuzzy_Boolean_Record'Class (Widget.all).Data,
         Param_ID,
         Value,
         Property_Spec
      );
   end Get_Property;

   function Get_Shape
            (  Widget : not null access Gtk_Fuzzy_Boolean_Record
            )  return Shape is
   begin
      return Widget.Data.Get_Shape (Widget);
   end Get_Shape;

   function Get_Type return Gtk_Type is
   begin
      Initialize_Class_Record
      (  Ancestor     => Gtk.Drawing_Area.Get_Type,
         Class_Record => Class_Record,
         Type_Name    => Class_Name,
         Class_Init   => Class_Init'Access
      );
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Boolean;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             )  is
   begin
      Widget := new Gtk_Fuzzy_Boolean_Record;
      begin
         Initialize (Widget, Value);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record'Class;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Drawing_Area.Initialize (Widget);
      Return_Boolean_Callback.Connect
      (  Widget,
         "draw",
         Return_Boolean_Callback.To_Marshaller (Draw'Access)
      );
      Allocation_Callback.Connect
      (  Widget,
         "size_allocate",
         Allocation_Marshaller.To_Marshaller (Size_Allocate'Access)
      );
      Widget.Data.Put (Value);
      declare
         Area : Gdk_Rectangle renames
                   Widget.Data.Get_Size
                   (  Widget.Data.Get_Shape (Widget),
                      0,
                      0
                   );
      begin
         Widget.Set_Size_Request (Area.Width, Area.Height);
      end;
   end Initialize;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : Fuzzy.Logic.Fuzzy_Boolean
             )  is
   begin
      Widget.Data.Put (Value);
      if Widget.Data.Updated then
         Widget.Queue_Draw;
      end if;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : Confidence
             )  is
   begin
      Widget.Data.Put (Value);
      if Widget.Data.Updated then
         Widget.Queue_Draw;
      end if;
   end Put;

   procedure Put
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Value  : GValue
             )  is
   begin
      Widget.Data.Put (Value);
      if Widget.Data.Updated then
         Widget.Queue_Draw;
      end if;
   end Put;

   procedure Set_Property
             (  Widget        : access GObject_Record'Class;
                Param_ID      : Property_ID;
                Value         : GValue;
                Property_Spec : Param_Spec
             )  is
      This : Gtk_Fuzzy_Boolean_Record'Class renames
                Gtk_Fuzzy_Boolean_Record'Class (Widget.all);
   begin
      Set_Property
      (  This.Data,
         Param_ID,
         Value,
         Property_Spec
      );
      if This.Data.Updated then
         Queue_Draw (This'Access);
      end if;
   end Set_Property;

   procedure Set_Shape
             (  Widget : not null access Gtk_Fuzzy_Boolean_Record;
                Form   : Shape
             )  is
   begin
      Widget.Data.Shape_Fixed := True;
      if Get_Shape (Widget) /= Form then
         Widget.Data.Form    := Form;
         Widget.Data.Updated := True;
         Widget.Queue_Draw;
      end if;
   end Set_Shape;

   procedure Size_Allocate
             (  Widget     : access Gtk_Fuzzy_Boolean_Record'Class;
                Allocation : Gtk_Allocation_Access
             )  is
   begin
      if Widget.Get_Realized then
         Move_Resize
         (  Get_Window (Widget),
            Allocation.X,
            Allocation.Y,
            Allocation.Width,
            Allocation.Height
         );
         Widget.Data.Updated := True;
      end if;
      Emit_Stop_By_Name (Widget, "size_allocate");
   end Size_Allocate;

end Gtk.Fuzzy_Boolean;
