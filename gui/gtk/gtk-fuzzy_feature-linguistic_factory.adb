--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Linguistic_Factory                       Autumn, 2007       --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Fuzzy.Abstract_Edit.Named;    use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Gtk_Icon_Factory;       use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;                use GLib.Messages;
with GLib.Properties;              use GLib.Properties;
with GLib.Properties.Creation;     use GLib.Properties.Creation;
with GLib.Types;                   use GLib.Types;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Missed;                   use Gtk.Missed;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Units;                        use Units;

with Fuzzy.Feature.Linguistic_Handle;
use  Fuzzy.Feature.Linguistic_Handle;

with Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Float_Handle;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Linguistic_Factory is
   use Fuzzy.Feature.Domain_Floats.Float_Measures;
   use Variable_Measures;
   use Variable_Measure_Sets;
   use Variable_Sets;
   use Fuzzy.Feature.Domain_Float_Handle;
   use Fuzzy.Feature.Linguistic_Handle;
   use Gtk.Widget;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Linguistic_Factory." & Name;
   end Where;

   procedure Changed
             (  Control : access Gtk_Widget_Record'Class;
                Widget  : Gtk_Linguistic_Factory
             )  is
   begin
      Widget.Update;
   end Changed;

   function Create
            (  Widget : not null access Gtk_Linguistic_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Domain : Linguistic_Set;
      Scale  : Measure;
   begin
      Widget.Domain.Get_Tree_View.Get (Domain, Scale);
      return
         Create
         (  Name   => Name,
            Domain => Domain,
            Scale  => Widget.Scale_Entry.Get_Text
         );
   end Create;

   procedure Destroy
             (  Widget : access Gtk_Linguistic_Factory_Record'Class
             )  is
   begin
      null;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Destroy Gtk_Linguistic_Factory_Record")
         )  );
   end Destroy;

   function Edited
            (  Widget : not null access Gtk_Linguistic_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   procedure Get
             (  Widget : not null access Gtk_Linguistic_Factory_Record;
                Value  : out Linguistic_Set;
                Scale  : out Measure
             )  is
   begin
      -- Get scale
      Scale := Get (Widget.Scale_Entry);
      Set_Hint
      (  Widget.Scale_Hint,
         Widget,
         Checked,
         Widget.Is_Editable
      );
      Widget.Domain.Get (Value, Scale);
   exception
      when Ada.IO_Exceptions.End_Error =>
         Set_Hint
         (  Widget.Scale_Hint,
            Widget,
            Checked,
            Widget.Is_Editable
         );
         Widget.Domain.Get (Value, Scale);
      when Unit_Error | Constraint_Error |
         Ada.IO_Exceptions.Data_Error =>
         Set_Hint
         (  Widget.Scale_Hint,
            Widget,
            Erroneous,
            Widget.Is_Editable
         );
         raise Constraint_Error with
               Style_Get (Widget, "unit-scale-error");
   end Get;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Linguistic_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-label",
               Nick    => "Unit",
               Blurb   => "The unit label",
               Default => "Unit"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-scale-error",
               Nick    => "Illegal scale unit",
               Blurb   => "Illegal unit of the scale field message",
               Default => "Illegal unit of the scale field"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Linguistic_Factory;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True
             )  is
   begin
      if Feature.Is_Valid and then not Is_Linguistic (Feature) then
         raise Constraint_Error with "Not a linguistic feature";
      end if;
      Widget := new Gtk_Linguistic_Factory_Record;
      begin
         Initialize (Widget, Feature, Editable);
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
             (  Widget   : not null access
                           Gtk_Linguistic_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             )  is
      Domain : Set_Measure;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 2, 3, False);
      Widget.Set_Homogeneous (False);
      -- Scale label
      Gtk_New (Widget.Scale_Label);
      Widget.Scale_Label.Set_Halign (Align_End);
      Widget.Scale_Label.Set_Valign (Align_Center);
--    Widget.Scale_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.Scale_Label,
         0, 1, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- Scale entry
      Gtk_New (Widget.Scale_Entry);
      Widget.Attach
      (  Widget.Scale_Entry,
         1, 2, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Scale_Entry.Set_Width_Chars (10);
      if Find_Property (Widget.Scale_Entry, "max-width-chars") /= null
      then
         Set_Property
         (  Widget.Scale_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      -- Scale hint
      Gtk_New_HBox (Widget.Scale_Hint);
      Widget.Attach
      (  Widget.Scale_Hint,
         2, 3, 0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      if Feature.Is_Valid then
         Domain := Empty (Get_Scale (Feature));
         declare
            Cardinality : constant Positive :=
                                   Get_Cardinality (Feature);
            Names       : Domain_Description;
         begin
            Get_Domain (Feature, Names);
            for Index in 1..Cardinality loop
               Insert
               (  Domain,
                  Index,
                  Get_Name (Names, Index),
                  Get_Variable (Feature, Index)
               );
            end loop;
         end;
         Widget.Scale_Entry.Set_Text (Get_Scale_Text (Feature));
         Gtk_New
         (  Widget   => Widget.Domain,
            Value    => Domain,
            Scale    => Get_Scale_Text (Feature),
            Layout   => Orientation_Horizontal,
            Editable => False
         );
      else
         Gtk_New
         (  Widget   => Widget.Domain,
            Value    => Domain,
            Layout   => Orientation_Horizontal,
            Editable => False
         );
      end if;
      Widget.Attach (Widget.Domain, 0, 3, 1, 2);
      Handlers.Connect (Widget, "destroy", Destroy'Access);
      Handlers.Connect (Widget, "style-updated", Style_Updated'Access);
      Action_Handlers.Connect
      (  Widget.Scale_Entry,
         "changed",
         Changed'Access,
         Widget.all'Access
      );
      Widget.Scale_Entry.Set_Sensitive (Editable);
      Widget.Domain.Set_Editable (Editable);
      Style_Updated (Widget);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Linguistic_Factory_Record
            )  return Boolean is
   begin
      return Is_Sensitive (Widget.Scale_Entry);
   end Is_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Linguistic_Factory_Record
             )  is
      Domain : Linguistic_Set;
      Scale  : Measure;
   begin
      Get (Widget, Domain, Scale);
   end Verify;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Linguistic_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         Widget.Scale_Entry.Set_Sensitive (Editable);
         Widget.Domain.Set_Editable (Editable);
      end if;
   end Set_Editable;

   procedure Style_Updated
             (  Widget : access Gtk_Linguistic_Factory_Record'Class
             )  is
   begin
      Widget.Scale_Label.Set_Text (Style_Get (Widget, "unit-label"));
      Set_Hint
      (  Widget.Scale_Hint,
         Widget,
         None,
         Widget.Is_Editable
      );
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

   procedure Update
             (  Widget : not null access Gtk_Linguistic_Factory_Record
             )  is
      New_Scale : Measure;
      Old_Scale : Measure;
      Domain    : Linguistic_Set;
   begin
      Widget.Modified := True;
      New_Scale := Get (Widget.Scale_Entry);
      Widget.Domain.Get (Domain, Old_Scale);
      Widget.Domain.Put
      (  (New_Scale.SI, Domain, New_Scale.Offset),
         Widget.Scale_Entry.Get_Text
      );
      Set_Hint
      (  Widget.Scale_Hint,
         Widget,
         Checked,
         Widget.Is_Editable
      );
   exception
      when others =>
         Set_Hint
         (  Widget.Scale_Hint,
            Widget,
            Erroneous,
            Widget.Is_Editable
         );
   end Update;

end Gtk.Fuzzy_Feature.Linguistic_Factory;
