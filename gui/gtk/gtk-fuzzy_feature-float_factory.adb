--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Float_Factory                            Spring, 2007       --
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
with Fuzzy.Feature.Float_Handle;  use Fuzzy.Feature.Float_Handle;
with Fuzzy.Gtk_Icon_Factory;      use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;               use GLib.Messages;
with GLib.Properties;             use GLib.Properties;
with GLib.Properties.Creation;    use GLib.Properties.Creation;
with GLib.Types;                  use GLib.Types;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Missed;                  use Gtk.Missed;
with Gtk.Widget.Styles;           use Gtk.Widget.Styles;
with Strings_Edit;                use Strings_Edit;
with Strings_Edit.Integers;       use Strings_Edit.Integers;
with Strings_Edit.UTF8.Maps;      use Strings_Edit.UTF8.Maps;
with Units;                       use Units;

with Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Float_Handle;
with GLib.Object.Checked_Destroy;
with Name_Tables;

package body Gtk.Fuzzy_Feature.Float_Factory is
   use Variables;
   use Variable_Measures;
   use Variable_Measure_Sets;
   use Fuzzy.Feature.Domain_Float_Handle;
   use Fuzzy.Feature.Domain_Floats.Float_Edit;
   use Gtk.Widget;
   use Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Editor.
       Editor_Of.Domain_Of;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Float_Factory." & Name;
   end Where;

   procedure Changed
             (  Control : access Gtk_Widget_Record'Class;
                Widget  : Gtk_Float_Factory
             )  is
   begin
      Widget.Update;
   end Changed;

   function Create
            (  Widget : not null access Gtk_Float_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Data       : aliased Float_Data;
      Scale_Text : constant UTF8_String := Get (Widget, Data'Access);
   begin
      return
         Create
         (  Name,
            Data.Cardinality,
            Data.From,
            Data.To,
            Scale_Text
         );
   end Create;

   function Edited
            (  Widget : not null access Gtk_Float_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   function Get
            (  Widget : not null access Gtk_Float_Factory_Record;
               Data   : not null access Float_Data
            )  return String is
      No_Scale : Boolean := False;
   begin
      -- Get cardinality
      declare
         Source  : constant String := Get_Text (Widget.Intervals_Entry);
         Pointer : Integer := Source'First;
         Count   : Integer;
      begin
         Get (Source, Pointer, Name_Tables.Blanks);
         begin
            Get (Source, Pointer, Count);
         exception
            when Ada.IO_Exceptions.End_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "missing-intervals-error");
            when Ada.IO_Exceptions.Data_Error | Constraint_Error =>
               raise Constraint_Error with
                     Style_Get (Widget, "wrong-intervals-error");
         end;
         Get (Source, Pointer, Name_Tables.Blanks);
         if Pointer <= Source'Last then
            raise Constraint_Error with
                  Style_Get (Widget, "wrong-intervals-error");
         end if;
         if Count < 1 then
            raise Constraint_Error with
                  Style_Get (Widget, "wrong-intervals-error");
         end if;
         Data.Cardinality := Count;
         Set_Hint
         (  Widget.Intervals_Hint,
            Widget,
            Checked,
            Is_Editable (Widget)
         );
      exception
         when others =>
            Set_Hint
            (  Widget.From_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.To_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.Intervals_Hint,
               Widget,
               Erroneous,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.Scale_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            raise;
      end;
      -- Get scale
      begin
         Data.Scale := Get (Widget.Scale_Entry);
         Set_Hint
         (  Widget.Scale_Hint,
            Widget,
            Checked,
            Is_Editable (Widget)
         );
      exception
         when Ada.IO_Exceptions.End_Error =>
            Data.Scale := (0, 1.0, 0.0);	-- Dimensionless
            No_Scale   := True;
         when Unit_Error | Constraint_Error |
              Ada.IO_Exceptions.Data_Error =>
            Set_Hint
            (  Widget.From_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.To_Hint,
               Widget,
               None,
               Is_Editable (Widget)
            );
            Set_Hint
            (  Widget.Scale_Hint,
               Widget,
               Erroneous,
               Is_Editable (Widget)
            );
            raise Constraint_Error with
                  Style_Get (Widget, "unit-scale-error");
      end;
      -- From field
      begin
         begin
            Data.From :=
               Get_Scaled (Get_Text (Widget.From_Entry), Data.Scale);
         exception
            when others =>
               Set_Hint
               (  Widget.From_Hint,
                  Widget,
                  Erroneous,
                  Is_Editable (Widget)
               );
               Set_Hint
               (  Widget.To_Hint,
                  Widget,
                  None,
                  Is_Editable (Widget)
               );
               raise;
         end;
      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "missing-from-error");
         when Constraint_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "overflow-from-error");
         when Ada.IO_Exceptions.Data_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "non-numeric-from-error");
         when Unit_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "unit-from-error");
         when Ada.IO_Exceptions.Use_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "incompatible-from-error");
      end;
      -- To field
      begin
         begin
            Data.To :=
               Get_Scaled (Get_Text (Widget.To_Entry), Data.Scale);
         exception
            when others =>
               Set_Hint
               (  Widget.From_Hint,
                  Widget,
                  Checked,
                  Is_Editable (Widget)
               );
               Set_Hint
               (  Widget.To_Hint,
                  Widget,
                  Erroneous,
                  Is_Editable (Widget)
               );
               raise;
         end;
      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "missing-to-error");
         when Constraint_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "overflow-to-error");
         when Ada.IO_Exceptions.Data_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "non-numeric-to-error");
         when Unit_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "unit-to-error");
         when Ada.IO_Exceptions.Use_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "incompatible-to-error");
      end;
      if (  Data.From > Data.To
         or else
            (  (Data.To - Data.From) / Domain_Float (Data.Cardinality)
            =  0.0
         )  )
      then
         Set_Hint
         (  Widget.From_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         Set_Hint
         (  Widget.To_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise Constraint_Error with Style_Get (Widget, "bounds-error");
      end if;
      Set_Hint
      (  Widget.From_Hint,
         Widget,
         Checked,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.To_Hint,
         Widget,
         Checked,
         Is_Editable (Widget)
      );
      if No_Scale then
         return "";
      else
         return Get_Text (Widget.Scale_Entry);
      end if;
   end Get;

   function Get_Name
            (  Widget      : not null access Gtk_Float_Factory_Record;
               Cardinality : Positive;
               Value       : Positive
            )  return String is
   begin
      return "I" & Image (Value);
   end Get_Name;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Float_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "bounds-error",
               Nick    => "Wrong interval",
               Blurb   => "From is greater than to message",
               Default => (  "The field from cannot exceed "
                          &  "to"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "incompatible-from-error",
               Nick    => "Incompatible from",
               Blurb   => "Incompatible unit in from message",
               Default => (  "The field from has a unit incompatible "
                          &  "with the scale"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "incompatible-to-error",
               Nick    => "Incompatible unit in to message",
               Blurb   => "Unit error in to",
               Default => (  "The field to has a unit incompatible "
                          &  "with the scale"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-from-error",
               Nick    => "Missing from",
               Blurb   => "No number in the from field message",
               Default => (  "The field from must contain "
                          &  "a number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-intervals-error",
               Nick    => "Missing intervals",
               Blurb   => "No intervals number message",
               Default => (  "The field intervals must contain "
                          &  "an integer number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-to-error",
               Nick    => "Missing to",
               Blurb   => "No number in the to field message",
               Default => (  "The field to must contain "
                          &  "a number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-from-error",
               Nick    => "Overlowed from",
               Blurb   => "The field to is out of range message",
               Default => (  "The field from contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-to-error",
               Nick    => "Overlowed to",
               Blurb   => "The field to is out of range message",
               Default => (  "The field from contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-from-error",
               Nick    => "Wrong from",
               Blurb   => "No number in the field to message",
               Default => "The field from must contain a number"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-to-error",
               Nick    => "Wrong to",
               Blurb   => "No number in the field to message",
               Default => "The field to must contain a number"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "from-label",
               Nick    => "From",
               Blurb   => "The from label",
               Default => "From"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "intervals-label",
               Nick    => "Intervals",
               Blurb   => "The intervals label",
               Default => "Intervals"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "preview-label",
               Nick    => "Feature domain",
               Blurb   => "The preview title label",
               Default => "Feature domain"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "to-label",
               Nick    => "To",
               Blurb   => "The to label",
               Default => "To"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-from-error",
               Nick    => "Illegal from unit",
               Blurb   => "Illegal unit of the from field message",
               Default => "Illegal unit of the from field"
         )  );
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
            (  Name    => "unit-to-error",
               Nick    => "Illegal to unit",
               Blurb   => "Illegal unit of the to field message",
               Default => "Illegal unit of the to field"
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
             (  Widget     : out Gtk_Float_Factory;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True;
                Preview_On : Boolean        := True
             )  is
   begin
      if Feature.Is_Valid and then not Is_Float (Feature) then
         raise Constraint_Error with
               "Not a floating-point feature";
      end if;
      Widget := new Gtk_Float_Factory_Record;
      begin
         Initialize (Widget, Feature, Editable, Preview_On);
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
             (  Widget     : not null access
                             Gtk_Float_Factory_Record'Class;
                Feature    : Feature_Handle;
                Editable   : Boolean;
                Preview_On : Boolean;
                Rows       : Natural := 0;
                Columns    : Natural := 0
             )  is
      Last_Options : Gtk_Attach_Options := Fill;
      Domain       : Set_Measure;
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      if Columns = 0 then
         Last_Options := Last_Options or Expand;
      end if;
      Widget.Rows    := GUInt (Rows);
      Widget.Columns := GUInt (Columns);
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize
      (  Widget,
         3 + Widget.Rows,
         6 + Widget.Columns,
         False
      );
      Widget.Set_Homogeneous (False);
      -- From label
      Gtk_New (Widget.From_Label);
      Widget.From_Label.Set_Halign (Align_End);
      Widget.From_Label.Set_Valign (Align_Center);
--    Widget.From_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.From_Label,
         0, 1, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- To label
      Gtk_New (Widget.To_Label);
      Widget.To_Label.Set_Halign (Align_End);
      Widget.To_Label.Set_Valign (Align_Center);
--    Widget.To_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.To_Label,
         0, 1, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- Intervals label
      Gtk_New (Widget.Intervals_Label);
      Widget.Intervals_Label.Set_Halign (Align_End);
      Widget.Intervals_Label.Set_Valign (Align_Center);
--    Widget.Intervals_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.Intervals_Label,
         3, 4, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- Scale label
      Gtk_New (Widget.Scale_Label);
      Widget.Scale_Label.Set_Halign (Align_End);
      Widget.Scale_Label.Set_Valign (Align_Center);
--    Widget.Scale_Label.Set_Alignment (1.0, 0.5);
      Widget.Attach
      (  Widget.Scale_Label,
         3, 4, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- From entry
      Gtk_New (Widget.From_Entry);
      Attach
      (  Widget,
         Widget.From_Entry,
         1, 2, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.From_Entry.Set_Width_Chars (10);
      if Find_Property (Widget.From_Entry, "max-width-chars") /= null
      then
         Set_Property
         (  Widget.From_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      -- To entry
      Gtk_New (Widget.To_Entry);
      Widget.Attach
      (  Widget.To_Entry,
         1, 2, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.To_Entry.Set_Width_Chars (10);
      if Find_Property (Widget.To_Entry, "max-width-chars") /= null then
         Set_Property
         (  Widget.To_Entry,
            Build ("max-width-chars"),
            GInt'(10)
         );
      end if;
      -- From hint
      Gtk_New_HBox (Widget.From_Hint);
      Widget.Attach
      (  Widget.From_Hint,
         2, 3, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- To hint
      Gtk_New_HBox (Widget.To_Hint);
      Widget.Attach
      (  Widget.To_Hint,
         2, 3, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      -- Intervals entry
      Gtk_New (Widget.Intervals_Entry);
      Widget.Attach
      (  Widget.Intervals_Entry,
         4, 5, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Intervals_Entry.Set_Width_Chars (5);
      if (  Find_Property (Widget.Intervals_Entry, "max-width-chars")
         /= null
         )
      then
         Set_Property
         (  Widget.Intervals_Entry,
            Build ("max-width-chars"),
            GInt'(5)
         );
      end if;
      -- Scale entry
      Gtk_New (Widget.Scale_Entry);
      Widget.Attach
      (  Widget.Scale_Entry,
         4, 5, 1, 2,
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
      -- Intervals hint
      Gtk_New_HBox (Widget.Intervals_Hint);
      Widget.Attach
      (  Widget.Intervals_Hint,
         5, 6, 0, 1,
         Xoptions => Last_Options,
         Yoptions => Shrink
      );
      -- Scale hint
      Gtk_New_HBox (Widget.Scale_Hint);
      Widget.Attach
      (  Widget.Scale_Hint,
         5, 6, 1, 2,
         Xoptions => Last_Options,
         Yoptions => Shrink
      );
      -- Preview button
      Gtk_New (Widget.Preview, "Preview");
      Widget.Attach
      (  Widget.Preview,
         0, 6 + Widget.Columns,
         2 + Widget.Rows, 3 + Widget.Rows,
         Xoptions => Expand or Fill,
         Yoptions => Expand or Fill
      );
      if Feature.Is_Valid then
         Domain := Empty (Get_Scale (Feature));
         declare
            Cardinality : constant Positive :=
                                   Get_Cardinality (Feature);
         begin
            for Index in 1..Cardinality loop
               Insert
               (  Domain,
                  Index,
                  Get_Name (Widget, Cardinality, Index),
                  Get_Variable (Feature, Index)
               );
            end loop;
         end;
         Widget.From_Entry.Set_Text
         (  Image
            (  Get_Value_As
               (  Get_From (Feature),
                  Get_Scale (Feature)
         )  )  );
         Widget.To_Entry.Set_Text
         (  Image
            (  Get_Value_As
               (  Get_To (Feature),
                  Get_Scale (Feature)
         )  )  );
         Widget.Intervals_Entry.Set_Text
         (  Image (Get_Cardinality (Feature))
         );
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
      Add (Widget.Preview, Widget.Domain);
      Set_Expanded (Widget.Preview, Preview_On);
      Handlers.Connect
      (  Widget,
         "style-updated",
         Handlers.To_Marshaller (Style_Updated'Access)
      );
      Action_Handlers.Connect
      (  Widget.From_Entry,
         "changed",
         Action_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Action_Handlers.Connect
      (  Widget.To_Entry,
         "changed",
         Action_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Action_Handlers.Connect
      (  Widget.Intervals_Entry,
         "changed",
         Action_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Action_Handlers.Connect
      (  Widget.Scale_Entry,
         "changed",
         Action_Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Set_Sensitive (Widget.From_Entry,      Editable);
      Set_Sensitive (Widget.To_Entry,        Editable);
      Set_Sensitive (Widget.Scale_Entry,     Editable);
      Set_Sensitive (Widget.Intervals_Entry, Editable);
      Style_Updated (Widget);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Float_Factory_Record
            )  return Boolean is
   begin
      return Is_Sensitive (Widget.From_Entry);
   end Is_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Float_Factory_Record
             )  is
      Data       : aliased Float_Data;
      Scale_Text : UTF8_String := Get (Widget, Data'Access);
   begin
      null;
   end Verify;

   procedure Set_Col_Spacings
             (  Widget  : not null access Gtk_Float_Factory_Record;
                Spacing : Guint
             )  is
   begin
      Gtk.Table.Set_Col_Spacings
      (  Gtk_Table_Record (Widget.all)'Access,
         Spacing
      );
      Set_Spacing (Widget.Preview, GInt (Spacing));
   end Set_Col_Spacings;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Float_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Is_Editable (Widget) xor Editable then
         Set_Sensitive (Widget.From_Entry,      Editable);
         Set_Sensitive (Widget.To_Entry,        Editable);
         Set_Sensitive (Widget.Scale_Entry,     Editable);
         Set_Sensitive (Widget.Intervals_Entry, Editable);
      end if;
   end Set_Editable;

   procedure Style_Changed
             (  Widget : not null access Gtk_Float_Factory_Record
             )  is
   begin
      null;
   end Style_Changed;

   procedure Style_Updated
             (  Widget : access Gtk_Float_Factory_Record'Class
             )  is
   begin
      Set_Text (Widget.From_Label, Style_Get (Widget, "from-label"));
      Set_Text
      (  Widget.Intervals_Label,
         Style_Get (Widget, "intervals-label")
      );
      Set_Label
      (  Widget.Preview,
         Style_Get (Widget, "preview-label")
      );
      Set_Text (Widget.To_Label, Style_Get (Widget, "to-label"));
      Set_Text (Widget.Scale_Label, Style_Get (Widget, "unit-label"));
      Set_Hint
      (  Widget.From_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.To_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.Intervals_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
      Set_Hint
      (  Widget.Scale_Hint,
         Widget,
         None,
         Is_Editable (Widget)
      );
      Widget.Style_Changed;
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

   procedure Update
             (  Widget : not null access Gtk_Float_Factory_Record
             )  is
   begin
      Widget.Modified := True;
      --
      -- Update the preview
      --
      declare
         Data       : aliased Float_Data;
         Step       : Domain_Float;
         Scale_Text : constant UTF8_String := Get (Widget, Data'Access);
         Domain     : Set_Measure := Empty (Data.Scale);
      begin
         Step :=
            (Data.To - Data.From) / Domain_Float (Data.Cardinality);
         for Index in 1..Data.Cardinality loop
            Data.To := Data.From + Step;
            Insert
            (  Domain,
               Index,
               "I" & Image (Index),
               (  Variable'
                  (  (Data.From, Confidence'First)
                  &  (Data.From, Confidence'Last )
                  &  (Data.To,   Confidence'Last )
                  &  (Data.To,   Confidence'First)
                  )
               *  Data.Scale
            )  );
            Data.From := Data.To;
         end loop;
         Put (Widget.Domain, Domain, Scale_Text);
      end;
   exception
      when others =>
         null;
   end Update;

end Gtk.Fuzzy_Feature.Float_Factory;
