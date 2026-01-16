--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Float_Factory.Isosceles_Trapezoids       Spring, 2007       --
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

with Ada.Exceptions;              use Ada.Exceptions;
with Confidence_Factors;          use Confidence_Factors;
with Fuzzy.Gtk_Icon_Factory;      use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties;             use GLib.Properties;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with GLib.Types;                  use GLib.Types;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Missed;                  use Gtk.Missed;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Units;                       use Units;

with Fuzzy.Feature.Isosceles_Trapezoids_Handle;
use  Fuzzy.Feature.Isosceles_Trapezoids_Handle;

with Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Float_Handle;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Float_Factory.Isosceles_Trapezoids is
   use Variables;
   use Variable_Measures;
   use Variable_Measure_Sets;
   use Fuzzy.Feature.Domain_Floats.Float_Edit;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return
      (  " in Gtk.Fuzzy_Feature.Float_Factory.Isosceles_Trapezoids."
      &  Name
      );
   end Where;

   function Create
            (  Widget : not null access
                        Gtk_Isosceles_Trapezoids_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Data       : aliased Float_Data;
      Shoulder   : aliased Domain_Float;
      Scale_Text : constant UTF8_String :=
                      Widget.Get (Data'Access, Shoulder'Access);
   begin
      return
         Create
         (  Name,
            Data.Cardinality,
            Data.From,
            Data.To,
            Shoulder,
            Scale_Text
         );
   end Create;

   function Get
            (  Widget   : not null access
                          Gtk_Isosceles_Trapezoids_Factory_Record;
               Data     : not null access Float_Data;
               Shoulder : not null access Domain_Float
            )  return UTF8_String is
      Result : constant UTF8_String := Widget.Get (Data);
   begin
      if (  Data.Cardinality < 2
         or else
            (Data.From < Data.To and then Data.Cardinality < 3)
         )
      then
         Set_Hint
         (  Widget.Intervals_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise Constraint_Error with
               Style_Get (Widget, "cardinality-error");
      end if;
      -- Shoulder field
      begin
         begin
            Shoulder.all :=
               Get_Scaled
               (  Get_Text (Widget.Shoulder_Entry),
                  Data.Scale
               );
         exception
            when others =>
               Set_Hint
               (  Widget.Shoulder_Hint,
                  Widget,
                  Erroneous,
                  Is_Editable (Widget)
               );
               raise;
         end;
      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "missing-shoulder-error");
         when Constraint_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "overflow-shoulder-error");
         when Ada.IO_Exceptions.Data_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "non-numeric-shoulder-error");
         when Unit_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "unit-shoulder-error");
         when Ada.IO_Exceptions.Use_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "incompatible-shoulder-error");
      end;
      if Shoulder.all < 0.0 then
         Set_Hint
         (  Widget.Shoulder_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise Constraint_Error with
               Style_Get (Widget, "negative-shoulder-error");
      end if;
      Set_Hint
      (  Widget.Shoulder_Hint,
         Widget,
         Checked,
         Is_Editable (Widget)
      );
      return Result;
   end Get;

   function Get_Name
            (  Widget      : not null access
                             Gtk_Isosceles_Trapezoids_Factory_Record;
               Cardinality : Positive;
               Value       : Positive
            )  return String is
   begin
      if Value = 1 then
         return "L";
      elsif Value >= Cardinality then
         return "R";
      else
         return "T" & Image (Value - 1);
      end if;
   end Get_Name;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Fuzzy_Feature.Float_Factory.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Isosceles_Trapezoids_Factory_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "cardinality-error",
               Nick    => "Wrong cardinality",
               Blurb   => "Too small cardinality message",
               Default =>
                  (  "The cardinality must be at least 2 when "
                  &  "the interval is empty. Otherwise, it must be"
                  &  "greater than 2"
         )  )     );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "incompatible-shoulder-error",
               Nick    => "Incompatible shoulder",
               Blurb   => "Incompatible unit in shoulder",
               Default => (  "The field shoulder has a unit "
                          &  "incompatible with the scale"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-shoulder-error",
               Nick    => "Missing shoulder",
               Blurb   => "No number in the shoulder field message",
               Default => "The field shoulder must contain a number"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-shoulder-error",
               Nick    => "Overlowed shoulder",
               Blurb   => "The field shoulder is out of range message",
               Default => (  "The field shoulder contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "negative-shoulder-error",
               Nick    => "Negative shoulder",
               Blurb   => "Negative field shoulder message",
               Default => "The field shoulder must be non-negative"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-shoulder-error",
               Nick    => "Wrong shoulder",
               Blurb   => "No number in the field shoulder message",
               Default => "The field shoulder must contain a number"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "shoulder-label",
               Nick    => "Shoulder",
               Blurb   => "The shoulder label",
               Default => "Shoulder"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-shoulder-error",
               Nick    => "Illegal shoulder unit",
               Blurb   => "Illegal unit of the shoulder field message",
               Default => "Illegal unit of the shoulder field"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Isosceles_Trapezoids_Factory;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True;
                Preview_On : Boolean        := True
             )  is
   begin
      if (  Feature.Is_Valid
         and then
            not Is_Isosceles_Trapezoid (Feature)
         )
      then
         raise Constraint_Error with
               "Not an isosceles trapezoids feature";
      end if;
      Widget := new Gtk_Isosceles_Trapezoids_Factory_Record;
      begin
         Isosceles_Trapezoids.Initialize
         (  Widget,
            Feature,
            Editable,
            Preview_On
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
                         Gtk_Isosceles_Trapezoids_Factory_Record'Class;
                Feature    : Feature_Handle;
                Editable   : Boolean;
                Preview_On : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Initialize
      (  Widget,
         Feature,
         Editable,
         Preview_On,
         Columns => 3
      );
      -- Shoulder label
      Gtk_New (Widget.Shoulder_Label);
      Widget.Shoulder_Label.Set_Halign (Align_End);
      Widget.Shoulder_Label.Set_Valign (Align_Center);
--    Widget.Shoulder_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.Shoulder_Label,
         6, 7, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- Shoulder entry
      Gtk_New (Widget.Shoulder_Entry);
      Widget.Attach
      (  Widget.Shoulder_Entry,
         7, 8, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Shoulder_Entry.Set_Width_Chars (10);
      if (  Find_Property (Widget.Shoulder_Entry, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Widget.Shoulder_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      -- Shoulder hint
      Gtk_New_HBox (Widget.Shoulder_Hint);
      Widget.Attach
      (  Widget.Shoulder_Hint,
         8, 9, 0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      if Feature.Is_Valid then
         Widget.Shoulder_Entry.Set_Text
         (  Image
            (  Get_Value_As
               (  Get_Shoulder (Feature),
                  Fuzzy.Feature.Domain_Float_Handle.Get_Scale (Feature)
         )  )  );
      end if;
      Action_Handlers.Connect
      (  Widget.Shoulder_Entry,
         "changed",
         Action_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Widget.Shoulder_Entry.Set_Sensitive (Editable);
      Style_Updated (Widget);
   end Initialize;

   procedure Set_Editable
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         Set_Editable
         (  Gtk_Float_Factory_Record (Widget.all)'Access,
            Editable
         );
         Widget.Shoulder_Entry.Set_Sensitive (Editable);
      end if;
   end Set_Editable;

   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             )  is
   begin
      if Widget.Shoulder_Hint /= null then
         Set_Hint
         (  Widget.Shoulder_Hint,
            Widget,
            None,
            Is_Editable (Widget)
         );
      end if;
      if Widget.Shoulder_Label /= null then
         Set_Text
         (  Widget.Shoulder_Label,
            Style_Get (Widget, "shoulder-label")
         );
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Warning,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Style_Changed")
         )  );
   end Style_Changed;

   procedure Verify
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             )  is
      Data       : aliased Float_Data;
      Shoulder   : aliased Domain_Float;
      Scale_Text : UTF8_String :=
                      Widget.Get (Data'Access, Shoulder'Access);
   begin
      null;
   end Verify;

   procedure Update
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             )  is
   begin
      Widget.Modified := True;
      --
      -- Update the preview
      --
      declare
         Data       : aliased Float_Data;
         Shoulder   : aliased Domain_Float;
         Step       : Domain_Float;
         Scale_Text : constant UTF8_String :=
                         Widget.Get (Data'Access, Shoulder'Access);
         Domain     : Set_Measure := Empty (Data.Scale);
      begin
         Step :=
            (Data.To - Data.From) / Domain_Float (Data.Cardinality - 2);
         Insert
         (  Domain,
            1,
            "L",
            (  Variable'
               (  (Data.From,            Confidence'Last )
               &  (Data.From + Shoulder, Confidence'First)
               )
            *  Data.Scale
         )  );
         for Index in 2..Data.Cardinality - 1 loop
            Data.To := Data.From + Step;
            Insert
            (  Domain,
               Index,
               "T" & Image (Index - 1),
               (  Variable'
                  (  (Data.From - Shoulder, Confidence'First)
                  &  (Data.From,            Confidence'Last )
                  &  (Data.To,              Confidence'Last )
                  &  (Data.To   + Shoulder, Confidence'First)
                  )
               *  Data.Scale
            )  );
            Data.From := Data.To;
         end loop;
         Insert
         (  Domain,
            Data.Cardinality,
            "R",
            (  Variable'
               (  (Data.From - Shoulder, Confidence'First)
               &  (Data.From,            Confidence'Last )
               )
            *  Data.Scale
         )  );
         Put (Widget.Domain, Domain, Scale_Text);
      end;
   exception
      when Error : others =>
         null;
   end Update;

end Gtk.Fuzzy_Feature.Float_Factory.Isosceles_Trapezoids;
