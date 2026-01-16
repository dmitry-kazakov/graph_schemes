--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Classificatory_Factory                   Summer 2008        --
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

with Ada.Exceptions;                use Ada.Exceptions;
with Confidence_Factors;            use Confidence_Factors;
with Confidence_Factors.Edit;       use Confidence_Factors.Edit;
with Deposit_Handles;               use Deposit_Handles;
with Fuzzy.Classifier;              use Fuzzy.Classifier;
with Fuzzy.Feature.Classificatory;  use Fuzzy.Feature.Classificatory;
with Fuzzy.Gtk_Icon_Factory;        use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;                 use GLib.Messages;
with GLib.Properties.Creation;      use GLib.Properties.Creation;
with GLib.Types;                    use GLib.Types;
with Gtk.Abstract_Browser;          use Gtk.Abstract_Browser;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Widget.Styles;             use Gtk.Widget.Styles;
with Persistent;                    use Persistent;
with Persistent.Handle;             use Persistent.Handle;

with Ada.IO_Exceptions;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Classificatory_Factory is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Classificatory_Factory." & Name;
   end Where;

   procedure Get
             (  Widget     : not null access
                             Gtk_Classificatory_Factory_Record;
                Classifier : out Classifier_Handle;
                Method     : out Generalization_Mode;
                Threshold  : out Confidence
             )  is
   begin
      -- Get classifier
      Classifier := Get_Classifier (Widget);
      -- Get method
      Method := Get (Widget.Method_Combo);
      -- Get threshold
      begin
         begin
            Threshold := Value (Get_Text (Widget.Threshold_Entry));
         exception
            when others =>
               Set_Hint
               (  Widget.Threshold_Hint,
                  Widget,
                  Erroneous,
                  Is_Editable (Widget)
               );
               raise;
         end;
      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "missing-threshold-error");
         when Constraint_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "range-threshold-error");
         when Ada.IO_Exceptions.Data_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "non-numeric-threshold-error");
      end;
   end Get;

   function Get_Classifier
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Classifier_Handle is
      Path   : constant Item_Path := Get_Path (Widget.Classifier_Entry);
      Store  : Storage_Handle;
      Object : Deposit_Handle;
   begin
      if Path'Length = 0 then
         raise Constraint_Error with
               Style_Get (Widget, "missing-classifier-error");
      end if;
      Check
      (  Widget.Classifier_Entry.Get_Picker,
         Path,
         Get_Constraint (Widget.Classifier_Entry),
         Store,
         Object
      );
      if (  not Store.Is_Valid
         or else
            not Object.Is_Valid
         or else
            Object.Ptr.all not in Classifier_Object'Class
         )
      then
         raise Constraint_Error with
               Style_Get (Widget, "wrong-classifier-error");
      end if;
      return To_Classifier_Handle (Object);
   exception
      when Picking_Error =>
         Set_Hint
         (  Widget.Classifier_Hint,
            Widget,
            Conflicting,
            Is_Editable (Widget)
         );
         raise;
      when others =>
         Set_Hint
         (  Widget.Classifier_Hint,
            Widget,
            Erroneous,
            Is_Editable (Widget)
         );
         raise;
   end Get_Classifier;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             )  is
   begin
      Factory.Modified := True;
   end Changed;

   function Create
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Method     : Generalization_Mode;
      Classifier : Classifier_Handle;
      Threshold  : Confidence;
   begin
      Get (Widget, Classifier, Method, Threshold);
      return
         Create
         (  Name,
            Classifier,
            Method,
            Threshold
         );
   end Create;

   function Edited
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Classificatory_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "classifier-label",
               Nick    => "Classifier",
               Blurb   => "The classifier label",
               Default => "Classifier"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "generalization-label",
               Nick    => "Generalization",
               Blurb   => "The generalization mode label",
               Default => "Generalization method"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-classifier-error",
               Nick    => "No classifier",
               Blurb   => "No classifier message",
               Default => (  "A classifier must be specified. The "
                          &  "feature value is to be determined by "
                          &  "classifier's outcome on the training "
                          &  "example in question"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-threshold-error",
               Nick    => "Missing threshold",
               Blurb   => "No threshold message",
               Default => (  "The threshold must contain "
                          &  "a numeric truth value 0..1. The value "
                          &  "defines the truncation level. All values "
                          &  "less than this threshold are treated as "
                          &  "0. Higher values of the threshold may "
                          &  "speed up classification, though make it "
                          &  "less accurate"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-threshold-error",
               Nick    => "Wrong threshold",
               Blurb   => "Wrong threshold field message",
               Default => (  "The threshold must contain "
                          &  "a numeric truth value 0..1"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "range-threshold-error",
               Nick    => "Threshold out of range",
               Blurb   => "Threshold field out of range message",
               Default => (  "The threshold must be "
                          &  "in range 0..1"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "threshold-label",
               Nick    => "Threshold",
               Blurb   => "The threshold value label",
               Default => "Threshold"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "wrong-classifier-error",
               Nick    => "Wrong classifier",
               Blurb   => "Wrong classifier message",
               Default => "The path does not specify a classifier"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Classificatory_Factory;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle    := No_Feature;
                Editable   : Boolean           := True
             )  is
   begin
      if (  Feature.Is_Valid
         and then
            not Is_Classificatory (Feature)
         )
      then
         raise Constraint_Error with "Not an output feature";
      end if;
      Widget := new Gtk_Classificatory_Factory_Record;
      begin
         Initialize
         (  Widget,
            Picker,
            Constraint,
            Feature,
            Editable
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
             (  Widget     : not null access
                             Gtk_Classificatory_Factory_Record'Class;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle;
                Editable   : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 2, 6, False);
      Widget.Set_Homogeneous (False);
         -- Labels
      Gtk_New (Widget.Method_Label);
      Widget.Method_Label.Set_Halign (Align_End);
      Widget.Method_Label.Set_Valign (Align_Center);
--    Widget.Method_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Classifier_Label);
      Widget.Classifier_Label.Set_Halign (Align_End);
      Widget.Classifier_Label.Set_Valign (Align_Center);
--    Widget.Classifier_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Threshold_Label);
      Widget.Threshold_Label.Set_Halign (Align_End);
      Widget.Threshold_Label.Set_Valign (Align_Center);
--    Widget.Threshold_Label.Set_Alignment (1.0, 0.5);
         -- Hints
      Gtk_New_HBox (Widget.Threshold_Hint);
         -- Entries
      Gtk_New (Widget.Method_Combo, Linear);
      Gtk_New (Widget.Threshold_Entry);
      Gtk_New
      (  Widget   => Widget.Classifier_Entry,
         Name     => "source classifier",
         Hint     => Widget.Classifier_Hint,
         Picker   => Picker,
         Editable => Editable
      );
      if Is_Classificatory (Feature) then
         Set_Feature (Widget, Feature);
      end if;
         -- Row 1
      Widget.Attach
      (  Widget.Classifier_Label,
         0, 1, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Classifier_Entry,
         1, 5, 0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Classifier_Hint,
         5, 6, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 2
      Widget.Attach
      (  Widget.Threshold_Label,
         0, 1, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Threshold_Entry,
         1, 2, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Threshold_Hint,
         2, 3, 1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Method_Label,
         3, 4, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Method_Combo,
         4, 5, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );

      Handlers.Connect
      (  Widget,
         "style-updated",
         Handlers.To_Marshaller (Style_Updated'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Method_Combo,
         "changed",
         Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Get_Entry (Widget.Classifier_Entry),
         "changed",
         Handlers.To_Marshaller (Set_Classifier'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Threshold_Entry,
         "changed",
         Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Combine (Constraint, Get_Constraint (Widget.Classifier_Entry));
      Widget.Classifier_Entry.Set_Editable (Editable);
      Widget.Method_Combo.Set_Sensitive (Editable);
      Widget.Threshold_Entry.Set_Sensitive (Editable);
      Style_Updated (Widget, Widget.all'Access);
   end Initialize;

   function Is_Editable
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Boolean is
   begin
      return Widget.Classifier_Entry.Is_Editable;
   end Is_Editable;

   procedure Verify
             (  Widget : not null access
                         Gtk_Classificatory_Factory_Record
             )  is
      Classifier : Classifier_Handle;
      Method     : Generalization_Mode;
      Threshold  : Confidence;
   begin
      Get (Widget, Classifier, Method, Threshold);
   end Verify;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Classificatory_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         Widget.Method_Combo.Set_Sensitive (Editable);
         Widget.Classifier_Entry.Set_Sensitive (Editable);
         Widget.Threshold_Entry.Set_Editable (Editable);
      end if;
   end Set_Editable;

   procedure Set_Classifier
             (  Factory    : not null access
                             Gtk_Classificatory_Factory_Record;
                Classifier : Classifier_Handle
             )  is
   begin
      Set_Path
      (  Factory.Classifier_Entry,
         Factory.Classifier_Entry.Get_Picker.Get_Path
         (  To_Deposit_Handle (Classifier)
      )  );
   end Set_Classifier;

   procedure Set_Classifier
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             )  is
   begin
      Factory.Set_Classifier (Factory.Get_Classifier);
      Factory.Modified := True;
   exception
      when others =>
         null;
   end Set_Classifier;

   procedure Set_Feature
             (  Factory : not null access
                          Gtk_Classificatory_Factory_Record;
                Feature : Feature_Handle
             )  is
   begin
      Factory.Set_Classifier (Get_Classifier (Feature));
      Factory.Method_Combo.Set (Get_Generalization (Feature));
      Factory.Threshold_Entry.Set_Text
      (  Image (Get_Threshold (Feature))
      );
   end Set_Feature;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             )  is
   begin
      Set_Text
      (  Factory.Method_Label,
         Style_Get (Factory, "generalization-label")
      );
      Set_Text
      (  Factory.Classifier_Label,
         Style_Get (Factory, "classifier-label")
      );
      Set_Text
      (  Factory.Threshold_Label,
         Style_Get (Factory, "threshold-label")
      );
      Set_Hint
      (  Factory.Threshold_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
      Set_Hint
      (  Factory.Classifier_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
   end Style_Updated;

end Gtk.Fuzzy_Feature.Classificatory_Factory;
