--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Output_Factory                           Spring, 2008       --
--  Interface                                                         --
--                                Last revision :  14:48 30 May 2014  --
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
--
--  This package provides a GTK+ widget for viewing/editing output fuzzy
--  features. It has the look:
--      ._____________________.___________________.________.
--      |                     |                   |        |
--      |             Method  |  Defuzzification  | Status |
--      |                     |  selection box    |        |
--      |_____________________|___________________|________|
--      |                     |                   |        |
--      |  Reference feature  |  Feature path     | Status |
--      |_____________________|___________________|________|
--      |                     |                   |        |
--      |            Default  |  Default value    | Status |
--      |_____________________|___________________|________|
--      |                     |                   |        |
--      |               Unit  |  Measurement unit |        |
--      |_____________________|___________________|________|
--
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Combo_Box;                use Gtk.Combo_Box;
with Gtk.Fuzzy_Object;             use Gtk.Fuzzy_Object;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.List_Store;               use Gtk.List_Store;
with Gtk.Widget;                   use Gtk.Widget;

with Gtk.Handlers;

with Fuzzy.Feature.Handle.Defuzzifiers;
use Fuzzy.Feature.Handle.Defuzzifiers;

package Gtk.Fuzzy_Feature.Output_Factory is

   Output_Factory_Class_Name : constant UTF8_String :=
                                  Prefix & "OutputFeatureFactory";
--
-- Gtk_Output_Factory_Record -- The widget type
--
-- Style properties :
--
--    default-label              - The label of the default value field.
--                                 String, the default is "Default".
--    feature-label              - The label of the feature path  field.
--                                 String,  the  default  is  "Reference
--                                 feature".
--    incompatible-default-error - The error text shown when the default
--                                 has  incompatible  units. String, the
--                                 default  is  "The field default has a
--                                 unit  incompatible  with the selected
--                                 feature".
--    method-label               - The label  of  the  method  selection
--                                 combo box.  String,  the  default  is
--                                 "Defuzzifier".
--    method-center-of-area      - The  name  of  COA  combo  box  item.
--                                 String, the  default  is  "Center  of
--                                 area".
--    method-center-of-gravity   - The  name  of  COG  combo  box  item.
--                                 String, the  default  is  "Center  of
--                                 gravity".
--    method-leftmost-maximum    - The  name  of  LM  combo  box   item.
--                                 String,  the  default  is   "Leftmost
--                                 maximum".
--    method-rightmost-maximum   - The  name  of  RM  combo  box   item.
--                                 String,  the  default  is  "Rightmost
--                                 maximum".
--    missing-default-error      - The error text shown when the default
--                                 value is missing. String, the default
--                                 is "The field default must contain  a
--                                 number  used  as   a   default   when
--                                 defuzzification is impossible by  the
--                                 method specified".
--    missing-feature-error      - The  error  text   shown   when   the
--                                 reference feature is missing. String,
--                                 the  default  is  "Missing feature to
--                                 defuzzy".
--    no-deffuzifier-error       - The error text shown when nothing  is
--                                 selected  in  the  combo box. String,
--                                 the  default is "A defuzzifier method
--                                 has to be selected".
--    non-numeric-default-error  - The eror text shown when the  default
--                                 value field does not contain a  valid
--                                 number. String, the default  is  "The
--                                 field default must contain a number".
--    overflow-default-error     - The error text shown when the default
--                                 field is too big. String, the default
--                                 is "The field default contains a  too
--                                 large number".
--    unknown-deffuzifier-error  - The  error  text   shown   when   the
--                                 defuzzifier    method   is   unknown.
--                                 String,    the    default   is   "The
--                                 defuzzifier method is unknown".
--    unit-default-error         - The error text shown when the default
--                                 field  has  an  illegal unit. String,
--                                 the  default  is "Illegal unit of the
--                                 default field".
--    unit-label                 - The label of the feature unit  field.
--                                 String, the default is "Unit".
--    wrong-feature-error        - The  error  text   shown   when   the
--                                 reference  feature  is wrong. String,
--                                 the  default  is  "The  path does not
--                                 specify   a   floating-point  feature
--                                 object      (capable     of     being
--                                 defuzzified)".
--
--  + Styles from Install_Hints_Style_Properties
--
   type Gtk_Output_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Output_Factory is
      access all Gtk_Output_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Output_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Boolean;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Factory
--
--    Widget     - The result
--    Picker     - Object picker for reference feature selection
--    Constraint - Picking constraint to combine with
--    Feature    - To initialize the fields of
--    Editable   - Initial state
--
-- The  parameter  Feature  can  be  an invalid handle. In that case the
-- fields  left  uninitialized.  When  Feature is valid it must refer an
-- integer feature, otherwise Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Gtk_New
             (  Widget     : out Gtk_Output_Factory;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget     - The widget to initialize
--    Picker     - Object picker for reference feature selection
--    Constraint - Picking constraint to combine with
--    Feature    - To initialize the fields of
--    Editable   - Initial state
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Initialize
             (  Widget     : not null access
                             Gtk_Output_Factory_Record'Class;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle;
                Editable   : Boolean
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access Gtk_Output_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Output_Factory_Record
             );
private
   type Gtk_Output_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      Method_List   : Gtk_List_Store;
      Method_Label  : Gtk_Label;
      Method_Combo  : Gtk_Combo_Box;
      Method_Hint   : Gtk_Box;
      Feature_Label : Gtk_Label;
      Feature_Entry : Gtk_Picker_Box;
      Feature_Hint  : Gtk_Box;
      Unit_Label    : Gtk_Label;
      Unit_Entry    : Gtk_Entry;
      Default_Label : Gtk_Label;
      Default_Entry : Gtk_Entry;
      Default_Hint  : Gtk_Box;
      Modified      : Boolean := False;
   end record;

   procedure Get
             (  Widget  : not null access Gtk_Output_Factory_Record;
                Method  : out Defuzzifier_Handle;
                Feature : out Feature_Handle;
                Default : out Domain_Float
             );
   function Get_Feature
            (  Widget : not null access Gtk_Output_Factory_Record
            )  return Feature_Handle;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             );
   procedure Set_Feature
             (  Factory : not null access Gtk_Output_Factory_Record;
                Feature : Feature_Handle
             );
   procedure Set_Feature
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             );
   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Output_Factory
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Output_Factory
          );

end Gtk.Fuzzy_Feature.Output_Factory;
