--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Selection                 Luebeck            --
--  Implementation                                 Winter, 2007       --
--                                                                    --
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
with Fuzzy.Feature.Binary;          use Fuzzy.Feature.Binary;
with Fuzzy.Feature.Classificatory;  use Fuzzy.Feature.Classificatory;
with Fuzzy.Feature.Discrete;        use Fuzzy.Feature.Discrete;
with Fuzzy.Feature.Output_Handle;   use Fuzzy.Feature.Output_Handle;
with Fuzzy.Gtk_Icon_Factory;        use Fuzzy.Gtk_Icon_Factory;
with GLib;                          use GLib;
with GLib.Messages;                 use GLib.Messages;
with GLib.Object;                   use GLib.Object;
with GLib.Properties.Creation;      use GLib.Properties.Creation;
with GLib.Types;                    use GLib.Types;
with GLib.Values.Feature_Factory;   use GLib.Values.Feature_Factory;
with GLib.Values;                   use GLib.Values;
with Gtk.Cell_Renderer_Text;        use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;      use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Missed;                    use Gtk.Missed;
with Gtk.Stock;                     use Gtk.Stock;
with Gtk.Tree_View_Column;          use Gtk.Tree_View_Column;
with Gtk.Widget;                    use Gtk.Widget;
with Gtk.Widget.Styles;             use Gtk.Widget.Styles;

with Fuzzy.Feature.Binary.Mutually_Independent;
with Fuzzy.Feature.Domain_Integer_Handle;
with Fuzzy.Feature.Float_Handle;
with Fuzzy.Feature.Isosceles_Trapezoids_Handle;
with Fuzzy.Feature.Linguistic_Handle;
with Gtk.Fuzzy_Feature.Binary_Factory;
with Gtk.Fuzzy_Feature.Classificatory_Factory;
with Gtk.Fuzzy_Feature.Discrete_Factory;
with Gtk.Fuzzy_Feature.Float_Factory;
with Gtk.Fuzzy_Feature.Integer_Factory;
with Gtk.Fuzzy_Feature.Isosceles_Trapezoids_Factory;
with Gtk.Fuzzy_Feature.Linguistic_Factory;
with Gtk.Fuzzy_Feature.Output_Factory;
with GLib.Object.Checked_Destroy;

use Fuzzy.Feature.Binary.Mutually_Independent;
use Fuzzy.Feature.Domain_Integer_Handle;
use Fuzzy.Feature.Float_Handle;
use Fuzzy.Feature.Isosceles_Trapezoids_Handle;
use Fuzzy.Feature.Linguistic_Handle;
use Gtk.Fuzzy_Feature.Binary_Factory;
use Gtk.Fuzzy_Feature.Classificatory_Factory;
use Gtk.Fuzzy_Feature.Discrete_Factory;
use Gtk.Fuzzy_Feature.Float_Factory;
use Gtk.Fuzzy_Feature.Integer_Factory;
use Gtk.Fuzzy_Feature.Isosceles_Trapezoids_Factory;
use Gtk.Fuzzy_Feature.Linguistic_Factory;
use Gtk.Fuzzy_Feature.Output_Factory;

package body Gtk.Fuzzy_Feature.Selection is

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   Integer_Feature            : constant GInt := 1;
   Nominal_Feature            : constant GInt := 2;
   Interval_Feature           : constant GInt := 3;
   Trapezoid_Feature          : constant GInt := 4;
   Linguistic_Feature         : constant GInt := 5;
   Output_Feature             : constant GInt := 6;
   Dependent_Binary_Feature   : constant GInt := 7;
   Independent_Binary_Feature : constant GInt := 8;
   Classificatory_Feature     : constant GInt := 9;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Feature.Selection." & Name;
   end Where;

   function Get_Constraint
            (  Widget : not null access Gtk_Feature_Selection_Record
            )  return Picker_Constraint is
   begin
      return Widget.Constraint;
   end Get_Constraint;

   function Get_Type return GType is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Tree_View.Get_Type,
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

         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "feature-column-title",
               Nick    => "Feature type",
               Blurb   => "The feature type column, name of",
               Default => "Feature type"
         )  );
         -- Names of the feature types classes
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "binary",
               Nick    => "Binary",
               Blurb   => "Binary features, type name",
               Default => "Binary"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "classificatory",
               Nick    => "Classificatory",
               Blurb   => "Classificatory features, type name",
               Default => "Classificatory"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "computed",
               Nick    => "Computed",
               Blurb   => "Computed features, type name",
               Default => "Computed"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "dependent",
               Nick    => "Dependent",
               Blurb   => "Dependent binary features, name",
               Default => "Mutually dependent"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "discrete",
               Nick    => "Discrete",
               Blurb   => "Discrete domain features, type name",
               Default => "Discrete"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "independent",
               Nick    => "Independent",
               Blurb   => "Independent features, name",
               Default => "Independent"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "real",
               Nick    => "Real",
               Blurb   => "Real-valued features, type name",
               Default => "Real"
         )  );
         -- Names of the feature types
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "integer",
               Nick    => "Integer",
               Blurb   => "Integer-valued features, name",
               Default => "Integer"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "interval",
               Nick    => "Interval",
               Blurb   => "Interval-valued features, name",
               Default => "Intervals (rectangular)"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "linguistic",
               Nick    => "linguistic",
               Blurb   => "Linguistic features, name",
               Default => "Piecewise linear"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "nominal",
               Nick    => "Nominal",
               Blurb   => "Nominal features, name",
               Default => "Nominal"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "output",
               Nick    => "Output",
               Blurb   => "Defuzzified features, name",
               Default => "Defuzzified"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "trapezoids",
               Nick    => "trapezoids",
               Blurb   => "Trapezoids-valued features, name",
               Default => "Isosceles trapezoids"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget            : out Gtk_Feature_Selection;
                Feature_Picker    : Gtk_Object_Picker := null;
                Classifier_Picker : Gtk_Object_Picker := null;
                Feature           : Feature_Handle    := No_Feature;
                Editable          : Boolean           := True
             )  is
   begin
      Widget := new Gtk_Feature_Selection_Record;
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
             (  Widget            : not null access
                                    Gtk_Feature_Selection_Record'Class;
                Feature_Picker    : Gtk_Object_Picker;
                Classifier_Picker : Gtk_Object_Picker;
                Feature           : Feature_Handle;
                Editable          : Boolean
             )  is
   begin
      Fuzzy.Gtk_Icon_Factory.Init; -- Make sure icons loaded
      G_New (Widget, Get_Type);
      Gtk.Tree_View.Initialize (Widget);
      Widget.Editable := Editable;
      Set_Rules_Hint (Widget, True);
      declare
         Column    : Gtk_Tree_View_Column;
         Text      : Gtk_Cell_Renderer_Text;
         Icon      : Gtk_Cell_Renderer_Pixbuf;
         Column_No : Gint;
      begin
         Gtk_New (Column);

         Gtk_New (Icon);
         Column.Pack_Start (Icon, False);
         Add_Stock_Attribute (Column, Icon, 3);

         Gtk_New (Text);
         Column.Pack_Start (Text, True);
         Column.Add_Attribute (Text, "text", 0);

         Column_No := Widget.Append_Column (Column);
         Column.Set_Resizable (True);
         Column.Set_Sort_Column_Id (0);
      end;
      Widget.Get_Selection.Set_Mode (Selection_Single);
      Widget.Feature    := Feature;
      Widget.Constraint := Create ("feature selection");
      if Feature_Picker = null then
         declare
            Picker : Gtk_Null_Picker;
         begin
            Gtk_New (Picker, Widget);
            Widget.Feature_Picker := Picker.all'Unchecked_Access;
         end;
      else
         Widget.Feature_Picker := Feature_Picker;
      end if;
      Widget.Feature_Picker.Ref;
      if Classifier_Picker = null then
         declare
            Picker : Gtk_Null_Picker;
         begin
            Gtk_New (Picker, Widget);
            Widget.Classifier_Picker := Picker.all'Unchecked_Access;
         end;
      else
         Widget.Classifier_Picker := Classifier_Picker;
      end if;
      Widget.Classifier_Picker.Ref;
      Feature_Selection_Handlers.Connect
      (  Widget,
         "style-updated",
         Feature_Selection_Handlers.To_Marshaller (Style_Updated'Access)
      );
      Feature_Selection_Handlers.Connect
      (  Widget,
         "destroy",
         Feature_Selection_Handlers.To_Marshaller (On_Destroy'Access)
      );
      Style_Updated (Widget);
   end Initialize;

   function Is_Editable
            (  Widget : not null access Gtk_Feature_Selection_Record
            )  return Boolean is
   begin
      return Widget.Editable;
   end Is_Editable;

   procedure On_Destroy
             (  Widget : access Gtk_Feature_Selection_Record'Class
             )  is
   begin
      if Widget.Feature_Picker /= null then
         Widget.Feature_Picker.Unref;
         Widget.Feature_Picker := null;
      end if;
      if Widget.Classifier_Picker /= null then
         Widget.Classifier_Picker.Unref;
         Widget.Classifier_Picker := null;
      end if;
      Free (Widget.Constraint);
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

   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Widget    : Gtk_Feature_Selection
             )  is
      Model    : Gtk_Tree_Model;
      Iter     : Gtk_Tree_Iter;
      Value    : GValue;
      Index    : GInt;
      Previous : constant Gtk_Fuzzy_Feature_Abstract_Factory :=
                          Widget.Selected;
      Selected : Gtk_Fuzzy_Feature_Abstract_Factory;
   begin
      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter and then not Has_Child (Model, Iter) then
         Get_Value (Model, Iter, 1, Value);
         Index := Get_Int (Value);
         Unset (Value);
         Get_Value (Model, Iter, 2, Value);
         Selected := Get_Factory (Value);
         Unset (Value);
         if (  Selected = null
            and then
               (   Widget.Editable
               or else
                   Widget.Selected = null
            )  )
         then
            case Index is
               when Classificatory_Feature =>
                  if Is_Classificatory (Widget.Feature) then
                     Gtk.Fuzzy_Feature.Classificatory_Factory.Gtk_New
                     (  Widget =>
                           Gtk_Classificatory_Factory (Selected),
                        Feature    => Widget.Feature,
                        Picker     => Widget.Classifier_Picker,
                        Constraint => Widget.Constraint,
                        Editable   => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget =>
                           Gtk_Classificatory_Factory (Selected),
                        Picker     => Widget.Classifier_Picker,
                        Constraint => Widget.Constraint,
                        Editable   => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Integer_Feature =>
                  if Is_Domain_Integer (Widget.Feature) then
                     Gtk_New
                     (  Widget   => Gtk_Integer_Factory (Selected),
                        Feature  => Widget.Feature,
                        Editable => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget   => Gtk_Integer_Factory (Selected),
                        Editable => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Nominal_Feature =>
                  if Is_Discrete (Widget.Feature) then
                     Gtk_New
                     (  Widget   => Gtk_Discrete_Factory (Selected),
                        Feature  => Widget.Feature,
                        Editable => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget   => Gtk_Discrete_Factory (Selected),
                        Editable => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Interval_Feature =>
                  if Is_Float (Widget.Feature) then
                     Gtk_New
                     (  Widget   => Gtk_Float_Factory (Selected),
                        Feature  => Widget.Feature,
                        Editable => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget   => Gtk_Float_Factory (Selected),
                        Editable => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Trapezoid_Feature =>
                  if Is_Isosceles_Trapezoid (Widget.Feature) then
                     Gtk_New
                     (  Gtk_Isosceles_Trapezoids_Factory (Selected),
                        Feature  => Widget.Feature,
                        Editable => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Gtk_Isosceles_Trapezoids_Factory (Selected),
                        Editable => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Linguistic_Feature =>
                  if Is_Linguistic (Widget.Feature) then
                     Gtk_New
                     (  Widget   => Gtk_Linguistic_Factory (Selected),
                        Feature  => Widget.Feature,
                        Editable => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget   => Gtk_Linguistic_Factory (Selected),
                        Editable => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Output_Feature =>
                  if Is_Output (Widget.Feature) then
                     Gtk_New
                     (  Widget     => Gtk_Output_Factory (Selected),
                        Feature    => Widget.Feature,
                        Picker     => Widget.Feature_Picker,
                        Constraint => Widget.Constraint,
                        Editable   => Widget.Editable
                     );
                  else
                     Gtk_New
                     (  Widget     => Gtk_Output_Factory (Selected),
                        Picker     => Widget.Feature_Picker,
                        Constraint => Widget.Constraint,
                        Editable   => Widget.Editable
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Dependent_Binary_Feature =>
                  if Is_Binary (Widget.Feature) then
                     Gtk.Fuzzy_Feature.Binary_Factory.Gtk_New
                     (  Widget      => Gtk_Binary_Factory (Selected),
                        Feature     => Widget.Feature,
                        Picker      => Widget.Feature_Picker,
                        Constraint  => Widget.Constraint,
                        Editable    => Widget.Editable,
                        Independent => False
                     );
                  else
                     Gtk_New
                     (  Widget      => Gtk_Binary_Factory (Selected),
                        Picker      => Widget.Feature_Picker,
                        Constraint  => Widget.Constraint,
                        Editable    => Widget.Editable,
                        Independent => False
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when Independent_Binary_Feature =>
                  if Is_Bit (Widget.Feature) then
                     Gtk.Fuzzy_Feature.Binary_Factory.Gtk_New
                     (  Widget      => Gtk_Binary_Factory (Selected),
                        Feature     => Widget.Feature,
                        Picker      => Widget.Feature_Picker,
                        Constraint  => Widget.Constraint,
                        Editable    => Widget.Editable,
                        Independent => True
                     );
                  else
                     Gtk_New
                     (  Widget      => Gtk_Binary_Factory (Selected),
                        Picker      => Widget.Feature_Picker,
                        Constraint  => Widget.Constraint,
                        Editable    => Widget.Editable,
                        Independent => True
                     );
                  end if;
                  Init (Value, GLib.Values.Feature_Factory.Get_Type);
                  Set_Factory (Value, Selected);
                  Set_Value (Widget.List, Iter, 2, Value);
                  Unset (Value);
               when others =>
                  null;
            end case;
         end if;
         if Selected /= null and then Selected /= Previous then
            if Previous = null or else Widget.Editable then
               Widget.Selected := Selected;
               Widget.Row      := Iter;
               Widget.On_Selection_Change (Selected, Previous);
            else
               Select_Iter (Selection, Widget.Row);
            end if;
            return;
         end if;
      end if;
      if Widget.Editable then
         Widget.Row := Iter;
         On_Category_Change (Widget);
      else
         if Iter /= Null_Iter then
            Select_Iter (Selection, Widget.Row);
         end if;
      end if;
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: "
            &  Exception_Information (Error)
            &  Where ("On_Selection")
         )  );
   end On_Selection;

   procedure Set_Editable
             (  Widget   : not null access Gtk_Feature_Selection_Record;
                Editable : Boolean
             )  is
   begin
      Widget.Editable := Editable;
   end Set_Editable;

   procedure Style_Updated
             (  Widget : access Gtk_Feature_Selection_Record'Class
             )  is
      type Tree_Level is range 1..3;
      Path     : array (Tree_Level) of Gtk_Tree_Iter :=
                    (others => Null_Iter);
      Level    : Tree_Level := Path'First;
      Row      : Gtk_Tree_Iter;
      Text     : GValue;
      Index    : GValue;

      procedure Add_Feature (Name : String; Kind_Of : GInt) is
      begin
         Append (Widget.List, Row, Path (Level));
         Set_String (Text, Style_Get (Widget, Name));
         Set_Value (Widget.List, Row, 0, Text);
         Set_Int (Index, Kind_Of);
         Set_Value (Widget.List, Row, 1, Index);
         case Kind_Of is
            when Integer_Feature =>
               Set_String (Text, Integer_Feature_Icon);
            when Nominal_Feature =>
               Set_String (Text, Nominal_Feature_Icon);
            when Interval_Feature =>
               Set_String (Text, Float_Feature_Icon);
            when Trapezoid_Feature =>
               Set_String (Text, Isosceles_Feature_Icon);
            when Linguistic_Feature =>
               Set_String (Text, Linear_Feature_Icon);
            when Output_Feature =>
               Set_String (Text, Output_Feature_Icon);
            when Dependent_Binary_Feature =>
               Set_String (Text, Dependent_Binary_Feature_Icon);
            when Independent_Binary_Feature =>
               Set_String (Text, Independent_Binary_Feature_Icon);
            when Classificatory_Feature =>
               Set_String (Text, Classificatory_Feature_Icon);
            when others =>
               Set_String (Text, Stock_File);
         end case;
         Set_Value (Widget.List, Row, 3, Text);
      end Add_Feature;

      procedure Add_Section (Name : String) is
      begin
         Level := Level + 1;
         Append (Widget.List, Path (Level), Path (Level - 1));
         Set_String (Text, Style_Get (Widget, Name));
         Set_Value (Widget.List, Path (Level), 0, Text);
         Set_Int (Index, 0);
         Set_Value (Widget.List, Path (Level), 1, Index);
         Set_String (Text, Stock_Directory);
         Set_Value (Widget.List, Path (Level), 3, Text);
      end Add_Section;

      procedure End_Section is
      begin
         Level := Level - 1;
      end End_Section;

   begin
      Init;
      Widget.Row := Null_Iter;
      Init (Text,  GType_String);
      Init (Index, GType_Int);
      Set_Title
      (  Get_Column (Widget, 0),
         Style_Get (Widget, "feature-column-title")
      );
      Set_Model (Widget, Null_Gtk_Tree_Model);
      Gtk_New
      (  Widget.List,
         (  GType_String,
            GType_Int,
            GLib.Values.Feature_Factory.Get_Type,
            GType_String
      )  );
      Add_Section ("independent");
         Add_Section ("discrete");
            Add_Feature ("integer", Integer_Feature);
            if Is_Domain_Integer (Widget.Feature) then
               Widget.Row := Row;
            end if;
            Add_Feature ("nominal", Nominal_Feature);
            if Is_Discrete (Widget.Feature) then
               Widget.Row := Row;
            end if;
         End_Section;
         Add_Section ("real");
            Add_Feature ("interval", Interval_Feature);
            if Is_Float (Widget.Feature) then
               Widget.Row := Row;
            end if;
            Add_Feature ("trapezoids", Trapezoid_Feature);
            if Is_Isosceles_Trapezoid (Widget.Feature) then
               Widget.Row := Row;
            end if;
            Add_Feature ("linguistic", Linguistic_Feature);
            if Is_Linguistic (Widget.Feature) then
               Widget.Row := Row;
            end if;
            Add_Feature ("output", Output_Feature);
            if Is_Output (Widget.Feature) then
               Widget.Row := Row;
            end if;
         End_Section;
      End_Section;
      Add_Section ("computed");
         Add_Section ("binary");
            Add_Feature ("dependent", Dependent_Binary_Feature);
            if Is_Binary (Widget.Feature) then
               Widget.Row := Row;
            end if;
            Add_Feature ("independent", Independent_Binary_Feature);
            if Is_Bit (Widget.Feature) then
               Widget.Row := Row;
            end if;
         End_Section;
         Add_Feature ("classificatory", Classificatory_Feature);
         if Is_Classificatory (Widget.Feature) then
            Widget.Row := Row;
         end if;
      End_Section;
      Unset (Text);
      Unset (Index);
      Set_Model (Widget, To_Interface (Widget.List));
      Unref (Widget.List);
      Expand_All (Widget);
      if not Widget.Handler then
         Widget.Handler := True;
         Feature_Type_Selection_Handlers.Connect
         (  Get_Selection (Widget),
            "changed",
            Feature_Type_Selection_Handlers.To_Marshaller
            (  On_Selection'Access
            ),
            Widget.all'Access
         );
      end if;
      if Widget.Row /= Null_Iter then
         Select_Iter (Get_Selection (Widget), Widget.Row);
      end if;
      declare
         Dummy  : GInt;
         Height : GInt;
         Width  : GInt;
      begin
         Widget.Columns_Autosize;                     -- Size columns
         Widget.Get_Preferred_Width  (Dummy, Width);  -- Query tree view
         Widget.Get_Preferred_Height (Dummy, Height); -- size
         Widget.Set_Size_Request                      -- Set new size
         (  GInt'Min (Width,  250),
            GInt'Min (Height, 300)
         );
      end;
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

end Gtk.Fuzzy_Feature.Selection;
