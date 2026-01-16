--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Output_Factory                           Spring, 2008       --
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
with Deposit_Handles;              use Deposit_Handles;
with Fuzzy.Feature.Output_Handle;  use Fuzzy.Feature.Output_Handle;
with Fuzzy.Gtk_Icon_Factory;       use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;                use GLib.Messages;
with GLib.Properties.Creation;     use Glib.Properties.Creation;
with GLib.Types;                   use GLib.Types;
with Gtk.Abstract_Browser;         use Gtk.Abstract_Browser;
with Gtk.Cell_Layout;              use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;       use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                    use Gtk.Enums;
with Gtk.Tree_Model;               use Gtk.Tree_Model;
with Gtk.Widget.Styles;            use Gtk.Widget.Styles;
with Persistent;                   use Persistent;
with Persistent.Handle;            use Persistent.Handle;
with Units;                        use Units;

with Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Float_Handle;
with Fuzzy.Feature.Output_Handle.Center_Of_Area;
with Fuzzy.Feature.Output_Handle.Center_Of_Gravity;
with Fuzzy.Feature.Output_Handle.Discrete_Center_Of_Gravity;
with Fuzzy.Feature.Output_Handle.Leftmost_Max;
with Fuzzy.Feature.Output_Handle.Rightmost_Max;
with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Feature.Output_Factory is
   use Fuzzy.Feature.Domain_Floats;
   use Fuzzy.Feature.Domain_Floats.Float_Measures;
   use Fuzzy.Feature.Domain_Float_Handle;
   use Measure_UTF8_Edit;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   COA  : constant GInt := 0;
   COG  : constant GInt := 1;
   LM   : constant GInt := 2;
   RM   : constant GInt := 3;
   DCOG : constant GInt := 4;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Output_Factory." & Name;
   end Where;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             )  is
   begin
      Factory.Modified := True;
   end Changed;

   function Create
            (  Widget : not null access Gtk_Output_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is
      Method  : Defuzzifier_Handle;
      Feature : Feature_Handle;
      Default : Domain_Float;
   begin
      Get (Widget, Method, Feature, Default);
      return
         Fuzzy.Feature.Output_Handle.Create
         (  Name,
            Feature,
            Method,
            Default * Get_Scale (Feature)
         );
   end Create;

   function Edited
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Boolean is
   begin
      return Widget.Modified;
   end Edited;

   procedure Get
             (  Widget  : not null access Gtk_Output_Factory_Record;
                Method  : out Defuzzifier_Handle;
                Feature : out Feature_Handle;
                Default : out Domain_Float
             )  is
   begin
      -- Get feature
      Feature := Get_Feature (Widget);
      -- Get method
      case Get_Active (Widget.Method_Combo) is
         when COA =>
            Method :=
               Fuzzy.Feature.Output_Handle.Center_Of_Area.Create;
         when COG =>
            Method :=
               Fuzzy.Feature.Output_Handle.Center_Of_Gravity.Create;
         when DCOG =>
            Method :=
               Fuzzy.Feature.Output_Handle.
                  Discrete_Center_Of_Gravity.Create;
         when LM =>
            Method :=
               Fuzzy.Feature.Output_Handle.Leftmost_Max.Create;
         when RM =>
            Method :=
               Fuzzy.Feature.Output_Handle.Rightmost_Max.Create;
         when -1 =>
            Set_Hint
            (  Widget.Method_Hint,
               Widget,
               Erroneous,
               Widget.Is_Editable
            );
            raise Constraint_Error with
                  Style_Get (Widget, "no-deffuzifier-error");
         when others =>
            Set_Hint
            (  Widget.Method_Hint,
               Widget,
               Erroneous,
               Widget.Is_Editable
            );
            raise Constraint_Error with
                  Style_Get (Widget, "unknown-deffuzifier-error");
      end case;
      -- Get default
      begin
         begin
            Default :=
               Get_Scaled
               (  Widget.Default_Entry.Get_Text,
                  Get_Scale (Feature)
               );
         exception
            when others =>
               Set_Hint
               (  Widget.Default_Hint,
                  Widget,
                  Erroneous,
                  Widget.Is_Editable
               );
               raise;
         end;
      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "missing-default-error");
         when Constraint_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "overflow-default-error");
         when Ada.IO_Exceptions.Data_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "non-numeric-default-error");
         when Unit_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "unit-default-error");
         when Ada.IO_Exceptions.Use_Error =>
            raise Constraint_Error with
                  Style_Get (Widget, "incompatible-default-error");
      end;
   end Get;

   function Get_Feature
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Feature_Handle is
      Path   : constant Item_Path := Get_Path (Widget.Feature_Entry);
      Store  : Storage_Handle;
      Object : Deposit_Handle;
   begin
      if Path'Length = 0 then
         raise Constraint_Error with
               Style_Get (Widget, "missing-feature-error");
      end if;
      Check
      (  Widget.Feature_Entry.Get_Picker,
         Path,
         Get_Constraint (Widget.Feature_Entry),
         Store,
         Object
      );
      if (  not Store.Is_Valid
         or else
            not Object.Is_Valid
         or else
            Object.Ptr.all not in Domain_Feature_Object'Class
         )
      then
         raise Constraint_Error with
               Style_Get (Widget, "wrong-feature-error");
      end if;
      return To_Feature_Handle (Object);
   exception
      when Picking_Error =>
         Set_Hint
         (  Widget.Feature_Hint,
            Widget,
            Conflicting,
            Widget.Is_Editable
         );
         raise;
      when others =>
         Set_Hint
         (  Widget.Feature_Hint,
            Widget,
            Erroneous,
            Widget.Is_Editable
         );
         raise;
   end Get_Feature;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Table.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Output_Factory_Class_Name
         )
      then
         Install_Hints_Style_Properties (Class_Record);
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "default-label",
               Nick    => "Default",
               Blurb   => "The default value label",
               Default => "Default"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "feature-label",
               Nick    => "Feature",
               Blurb   => "The feature label",
               Default => "Reference feature"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "incompatible-default-error",
               Nick    => "Incompatible default",
               Blurb   => "Incompatible unit in default message",
               Default => (  "The field default has a unit "
                          &  "incompatible with the selected feature"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-label",
               Nick    => "Defuzzifier",
               Blurb   => "The defuzzifier label",
               Default => "Defuzzifier"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-center-of-area",
               Nick    => "COA",
               Blurb   => "The COA's name",
               Default => "Center of area"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-center-of-gravity",
               Nick    => "COG",
               Blurb   => "The COG's name",
               Default => "Center of gravity"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-discrete-center-of-gravity",
               Nick    => "DCOG",
               Blurb   => "The DCOG's name",
               Default => "Discrete center of gravity"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-leftmost-maximum",
               Nick    => "LM",
               Blurb   => "The LM's name",
               Default => "Leftmost maximum"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "method-rightmost-maximum",
               Nick    => "RM",
               Blurb   => "The RM's name",
               Default => "Rightmost maximum"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-default-error",
               Nick    => "Missing default",
               Blurb   => "No number in the default field message",
               Default => (  "The field default must contain "
                          &  "a number used as the default when "
                          &  "defuzzification is impossible by"
                          &  "the method specified"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "missing-feature-error",
               Nick    => "No feature",
               Blurb   => "No feature message",
               Default => "Missing feature to defuzzy"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "no-deffuzifier-error",
               Nick    => "No defuzzifier",
               Blurb   => "No defuzzifier message",
               Default => "A defuzzifier method has to be selected"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "non-numeric-default-error",
               Nick    => "Wrong default",
               Blurb   => "No number in the field default message",
               Default => "The field default must contain a number"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "overflow-default-error",
               Nick    => "Overlowed default",
               Blurb   => "The field default is out of range message",
               Default => (  "The field default contains "
                          &  "a too large number"
         )  )             );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unknown-deffuzifier-error",
               Nick    => "Unknown defuzzifier",
               Blurb   => "Unknown defuzzifier message",
               Default => "The defuzzifier method is unknown"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "unit-default-error",
               Nick    => "Illegal default unit",
               Blurb   => "Illegal unit of the default field message",
               Default => "Illegal unit of the default field"
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
            (  Name    => "wrong-feature-error",
               Nick    => "Wrong feature",
               Blurb   => "Wrong feature message",
               Default => (  "The path does not specify a "
                          &  "floating-point feature object (capable "
                          &  "of being defuzzified)"
         )  )             );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget     : out Gtk_Output_Factory;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True
             )  is
   begin
      if Feature.Is_Valid and then not Is_Output (Feature) then
         raise Constraint_Error with "Not an output feature";
      end if;
      Widget := new Gtk_Output_Factory_Record;
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
                             Gtk_Output_Factory_Record'Class;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle;
                Editable   : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Table.Initialize (Widget, 2, 9, False);
      Widget.Set_Homogeneous (False);
      -- Labels
      Gtk_New (Widget.Method_Label);
      Widget.Method_Label.Set_Halign (Align_End);
      Widget.Method_Label.Set_Valign (Align_Center);
--    Widget.Method_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Feature_Label);
      Widget.Feature_Label.Set_Halign (Align_End);
      Widget.Feature_Label.Set_Valign (Align_Center);
--    Widget.Feature_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Unit_Label);
      Widget.Unit_Label.Set_Halign (Align_End);
      Widget.Unit_Label.Set_Valign (Align_Center);
--    Widget.Unit_Label.Set_Alignment (1.0, 0.5);
      Gtk_New (Widget.Default_Label);
      Widget.Default_Label.Set_Halign (Align_End);
      Widget.Default_Label.Set_Valign (Align_Center);
--    Widget.Default_Label.Set_Alignment (1.0, 0.5);
      -- Hints
      Gtk_New_HBox (Widget.Method_Hint);
      Gtk_New_HBox (Widget.Default_Hint);
      -- Entries
      Gtk_New (Widget.Method_List, (GType_String, GType_Int));
      Gtk_New (Widget.Default_Entry);
      declare
         Text : Gtk_Cell_Renderer_Text;
         Row  : Gtk_Tree_Iter;
      begin
         Widget.Method_List.Append (Row);
         Widget.Method_List.Set (Row, 1, COA);
         Widget.Method_List.Append (Row);
         Widget.Method_List.Set (Row, 1, COG);
         Widget.Method_List.Append (Row);
         Widget.Method_List.Set (Row, 1, DCOG);
         Widget.Method_List.Append (Row);
         Widget.Method_List.Set (Row, 1, LM);
         Widget.Method_List.Append (Row);
         Widget.Method_List.Set (Row, 1, RM);
         Gtk_New_With_Model
         (  Widget.Method_Combo,
            To_Interface (Widget.Method_List)
         );
         Widget.Method_List.Unref;
         Gtk_New (Text);
         Pack_Start (+Widget.Method_Combo, Text, False);
         Add_Attribute (+Widget.Method_Combo, Text, "text", 0);
      end;
      Gtk_New
      (  Widget   => Widget.Feature_Entry,
         Name     => "source feature",
         Hint     => Widget.Feature_Hint,
         Picker   => Picker,
         Editable => Editable
      );
      Gtk_New (Widget.Unit_Entry);
      Widget.Unit_Entry.Set_Sensitive (False);

      if Is_Output (Feature) then
         Set_Feature (Widget, Feature);
      end if;
         -- Row 1
      Widget.Attach
      (  Widget.Feature_Label,
         0, 1, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Feature_Entry,
         1, 8, 0, 1,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Feature_Hint,
         8, 9, 0, 1,
         Xoptions => Fill,
         Yoptions => Shrink
      );
         -- Row 2
      Widget.Attach
      (  Widget.Method_Label,
         0, 1, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Method_Combo,
         1, 2, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Method_Hint,
         2, 3, 1, 2,
         Xoptions => Fill or Expand,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Default_Label,
         3, 4, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Default_Entry,
         4, 5, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Default_Hint,
         5, 6, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Unit_Label,
         6, 7, 1, 2,
         Xoptions => Fill,
         Yoptions => Shrink
      );
      Widget.Attach
      (  Widget.Unit_Entry,
         7, 8, 1, 2,
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
      (  Get_Entry (Widget.Feature_Entry),
         "changed",
         Handlers.To_Marshaller (Set_Feature'Access),
         Widget.all'Access
      );
      Handlers.Connect
      (  Widget.Default_Entry,
         "changed",
         Handlers.To_Marshaller (Changed'Access),
         Widget.all'Access
      );
      Combine (Constraint, Get_Constraint (Widget.Feature_Entry));
      begin
         Set_Text
         (  Widget.Unit_Entry,
            Get_Scale_Text (Get_Feature (Widget))
         );
      exception
         when others =>
            null;
      end;
      Widget.Method_Combo.Set_Sensitive (Editable);
      Widget.Feature_Entry.Set_Editable (Editable);
      Widget.Default_Entry.Set_Sensitive (Editable);
      Style_Updated (Widget, Widget.all'Access);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Boolean is
   begin
      return Widget.Feature_Entry.Is_Editable;
   end Is_Editable;

   procedure Verify
             (  Widget : not null access Gtk_Output_Factory_Record
             )  is
      Method  : Defuzzifier_Handle;
      Feature : Feature_Handle;
      Default : Domain_Float;
   begin
      Get (Widget, Method, Feature, Default);
   end Verify;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Output_Factory_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         Widget.Method_Combo.Set_Sensitive (Editable);
         Widget.Feature_Entry.Set_Sensitive (Editable);
         Widget.Default_Entry.Set_Editable (Editable);
      end if;
   end Set_Editable;

   procedure Set_Feature
             (  Factory : not null access Gtk_Output_Factory_Record;
                Feature : Feature_Handle
             )  is
   begin
      Factory.Default_Entry.Set_Text
      (  Image (Get_Default (Feature))
      );
      Factory.Unit_Entry.Set_Text (Get_Scale_Text (Feature));
      declare
         Source : Deposit_Handle;
         Name   : constant String :=
                     Get_Method_Name (Get_Defuzzifier (Feature));
      begin
         Set
         (  Source,
            To_Deposit_Ptr (Float_Handles.Ptr (Get_Source (Feature)))
         );
         Set_Path
         (  Factory.Feature_Entry,
            Get_Path (Get_Picker (Factory.Feature_Entry), Source)
         );
         if Name = Center_Of_Area.Name_Of then
            Factory.Method_Combo.Set_Active (COA);
         elsif Name = Center_Of_Gravity.Name_Of then
            Factory.Method_Combo.Set_Active (COG);
         elsif Name = Discrete_Center_Of_Gravity.Name_Of then
            Factory.Method_Combo.Set_Active (DCOG);
         elsif Name = Leftmost_Max.Name_Of then
            Factory.Method_Combo.Set_Active (LM);
         elsif Name = Rightmost_Max.Name_Of then
            Factory.Method_Combo.Set_Active (RM);
         end if;
      end;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Feature (change viewed feature)")
         )  );
   end Set_Feature;

   procedure Set_Feature
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             )  is
      Feature : Feature_Handle;
   begin
      Feature := Get_Feature (Factory);
      if Feature.Is_Valid then
         Factory.Unit_Entry.Set_Text (Get_Scale_Text (Feature));
      end if;
      Factory.Modified := True;
   exception
      when Picking_Error | Constraint_Error =>
         null;
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("Set_Feature (handler)")
         )  );
   end Set_Feature;

   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             )  is
   begin
      Factory.Method_Label.Set_Text
      (  Style_Get (Factory, "method-label")
      );
      Factory.Feature_Label.Set_Text
      (  Style_Get (Factory, "feature-label")
      );
      Factory.Unit_Label.Set_Text
      (  Style_Get (Factory, "unit-label")
      );
      Factory.Default_Label.Set_Text
      (  Style_Get (Factory, "default-label")
      );
      Factory.Method_List.Set
      (  Nth_Child (Factory.Method_List, Null_Iter, COA),
         0,
         String'(Style_Get (Factory, "method-center-of-area"))
      );
      Factory.Method_List.Set
      (  Nth_Child (Factory.Method_List, Null_Iter, COG),
         0,
         String'(Style_Get (Factory, "method-center-of-gravity"))
      );
      Factory.Method_List.Set
      (  Nth_Child (Factory.Method_List, Null_Iter, DCOG),
         0,
         String'
         (  Style_Get (Factory, "method-discrete-center-of-gravity")
      )  );
      Factory.Method_List.Set
      (  Nth_Child (Factory.Method_List, Null_Iter, LM),
         0,
         String'(Style_Get (Factory, "method-leftmost-maximum"))
      );
      Factory.Method_List.Set
      (  Nth_Child (Factory.Method_List, Null_Iter, RM),
         0,
         String'(Style_Get (Factory, "method-rightmost-maximum"))
      );
      Set_Hint
      (  Factory.Default_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
      Set_Hint
      (  Factory.Feature_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
      Set_Hint
      (  Factory.Method_Hint,
         Factory,
         None,
         Is_Editable (Factory)
      );
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

end Gtk.Fuzzy_Feature.Output_Factory;
