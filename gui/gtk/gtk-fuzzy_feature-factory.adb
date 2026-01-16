--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Factory                   Luebeck            --
--  Implementation                                 Winter, 2007       --
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
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Factory is
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Factory." & Name;
   end Where;

   function Get_Constraint
            (  Widget : not null access Gtk_Fuzzy_Feature_Factory_Record
            )  return Picker_Constraint is
   begin
      if Widget.Selection = null then
         return Unconstrained;
      else
         return Get_Constraint (Widget.Selection);
      end if;
   end Get_Constraint;

   function Get_Factory
            (  Widget : access Gtk_Fuzzy_Feature_Factory_Record'Class
            )  return Gtk_Fuzzy_Feature_Abstract_Factory is
      Factory : constant Gtk_Widget := Get_Child2 (Widget);
   begin
      if Factory = null then
         return null;
      end if;
      if Factory.all not in
            Gtk_Fuzzy_Feature_Abstract_Factory_Record'Class
      then
         return null;
      end if;
      return
         Gtk_Fuzzy_Feature_Abstract_Factory_Record'Class
         (  Factory.all
         ) 'Unchecked_Access;
   end Get_Factory;

   procedure Change_Spacing
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Factory_Record;
                Button  : GUInt;
                Column  : GUInt;
                Row     : GUInt
             )  is
      Factory : constant Gtk_Fuzzy_Feature_Abstract_Factory :=
                         Get_Factory (Widget);
   begin
      if Factory /= null then
         Set_Button_Spacing (Factory, Button);
         Set_Col_Spacings   (Factory, Column);
         Set_Row_Spacings   (Factory, Row);
      end if;
   end Change_Spacing;

   function Create
            (  Widget : not null access
                        Gtk_Fuzzy_Feature_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Factory : constant Gtk_Fuzzy_Feature_Abstract_Factory :=
                         Get_Factory (Widget);
   begin
      if Factory = null then
         raise Use_Error;
      else
         return Create (Factory, Name);
      end if;
   end Create;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Paned.Get_Type_HPaned,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "button-spacing",
               Nick    => "Button spacing",
               Blurb   => "Spacing in the feature factory button boxes",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "column-spacing",
               Nick    => "Column spacing",
               Blurb   => "Column spacing in the feature factory pane",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_UInt
            (  Name    => "row-spacing",
               Nick    => "Row spacing",
               Blurb   => "Row spacing in the feature factory pane",
               Minimum => 0,
               Maximum => GUInt'Last,
               Default => 3
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget            : out Gtk_Fuzzy_Feature_Factory;
                Feature_Picker    : Gtk_Object_Picker := null;
                Classifier_Picker : Gtk_Object_Picker := null;
                Feature           : Feature_Handle    := No_Feature;
                Editable          : Boolean           := True
             )  is
   begin
      Widget := new Gtk_Fuzzy_Feature_Factory_Record;
      begin
         Initialize
         (  Widget            => Widget,
            Feature_Picker    => Feature_Picker,
            Classifier_Picker => Classifier_Picker,
            Feature           => Feature,
            Editable          => Editable
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

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Feature_Factory_Record'Class;
                Feature_Picker    : Gtk_Object_Picker;
                Classifier_Picker : Gtk_Object_Picker;
                Feature           : Feature_Handle;
                Editable          : Boolean
             )  is
      Scroller : Gtk_Scrolled_Window;
      Frame    : Gtk_Frame;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize_HPaned (Widget);
      Gtk_New (Frame);
      Frame.Set_Shadow_Type (Shadow_In);
      Widget.Pack1 (Frame, Resize => False);

      Gtk_New (Scroller);
      Scroller.Set_Policy (Policy_Automatic, Policy_Automatic);
      Frame.Add (Scroller);

      Widget.Selection := new Selection_Record (Widget.all'Access);
      begin
         Initialize
         (  Widget            => Widget.Selection,
            Feature_Picker    => Feature_Picker,
            Classifier_Picker => Classifier_Picker,
            Feature           => Feature,
            Editable          => Editable
         );
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Initialize")
            )  );
            GLib.Object.Checked_Destroy (Widget.Selection);
            raise;
      end;
      Scroller.Add (Widget.Selection);
      Widget.Selection.Ref;
      Widget_Handlers.Connect
      (  Widget,
         "destroy",
         On_Destroy'Access,
         Widget.all'Unchecked_Access
      );
      Widget_Handlers.Connect
      (  Widget,
         "style-updated",
         Style_Updated'Access,
         Widget.all'Unchecked_Access
      );
      Style_Updated (Widget, Widget.all'Unchecked_Access);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Fuzzy_Feature_Factory_Record
            )  return Boolean is
   begin
      return Widget.Editable;
   end Is_Editable;

   procedure On_Destroy
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Fuzzy_Feature_Factory
             )  is
   begin
      if Factory.Selection /= null then
         Factory.Selection.Unref;
         Factory.Selection := null;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Destroy")
         )  );
   end On_Destroy;

   procedure On_Selection_Change
             (  Widget     : not null access Selection_Record;
                Selected   : Gtk_Fuzzy_Feature_Abstract_Factory;
                Deselected : Gtk_Fuzzy_Feature_Abstract_Factory
             )  is
   begin
      if Deselected /= null then
         Remove (Widget.Parent, Deselected);
      end if;
      Widget.Parent.Pack2 (Selected, Resize => True);
      if Widget.Parent.Spacing_Set then
         Widget.Parent.Change_Spacing
         (  Button => Widget.Parent.Button_Spacing,
            Column => Widget.Parent.Column_Spacing,
            Row    => Widget.Parent.Row_Spacing
         );
      else
         if Widget.Get_Realized then
            Widget.Parent.Change_Spacing
            (  Button => Style_Get (Widget, "button-spacing"),
               Column => Style_Get (Widget, "column-spacing"),
               Row    => Style_Get (Widget, "row-spacing")
            );
            Widget_Handlers.Connect
            (  Widget,
               "style-updated",
               Style_Updated'Access,
               Widget.Parent.all'Unchecked_Access
            );
         end if;
      end if;
      Selected.Show_All;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Selection_Change")
         )  );
   end On_Selection_Change;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Feature_Factory_Record;
                Editable : Boolean
             )  is
      Factory : constant Gtk_Fuzzy_Feature_Abstract_Factory :=
                         Get_Factory (Widget);
   begin
      Widget.Editable := Editable;
      if Factory /= null then
         Set_Editable (Factory, Editable);
      end if;
--      if Widget.Selection /= null then
--         Set_Editable (Widget.Selection, Editable);
--      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Editable")
         )  );
   end Set_Editable;

   procedure Set_Spacing
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Factory_Record;
                Button  : GUInt;
                Column  : GUInt;
                Row     : GUInt
             )  is
      Factory : Gtk_Fuzzy_Feature_Abstract_Factory :=
                   Get_Factory (Widget);
   begin
      Widget.Spacing_Set    := True;
      Widget.Button_Spacing := Button;
      Widget.Column_Spacing := Column;
      Widget.Row_Spacing    := Row;
      Change_Spacing (Widget, Button, Column, Row);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Spacing")
         )  );
   end Set_Spacing;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Fuzzy_Feature_Factory
             )  is
   begin
      if Get_Factory (Factory) /= null then
         if Factory.Spacing_Set then
            Change_Spacing
            (  Factory,
               Factory.Button_Spacing,
               Factory.Column_Spacing,
               Factory.Row_Spacing
            );
         else
            Change_Spacing
            (  Factory,
               Button => Style_Get (Factory, "button-spacing"),
               Column => Style_Get (Factory, "column-spacing"),
               Row    => Style_Get (Factory, "row-spacing")
            );
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

   procedure Verify
             (  Widget : not null access
                         Gtk_Fuzzy_Feature_Factory_Record
             )  is
      Factory : constant Gtk_Fuzzy_Feature_Abstract_Factory :=
                         Get_Factory (Widget);
   begin
      if Factory = null then
         raise Use_Error;
      else
         Verify (Factory);
      end if;
   exception
      when Constraint_Error =>
         raise;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Verify")
         )  );
   end Verify;

end Gtk.Fuzzy_Feature.Factory;
