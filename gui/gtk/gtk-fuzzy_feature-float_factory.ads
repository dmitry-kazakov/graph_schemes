--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Float_Factory                            Spting, 2007       --
--  Interface                                                         --
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
--
--  This package provides a  GTK+  widget  for  viewing/editing  integer
--  fuzzy features. It has the look:
--  .______._______.________.___________._______.________.
--  |      |       |        |           |       |        |
--  | From | Value | Status | Intervals | Value | Status |
--  |______|_______|________|___________|_______|________|
--  |      |       |        |           |       |        |
--  |   To | Value | Status |     Scale | Value | Status |
--  |______|_______|________|___________|_______|________|
--  |                                                    |
--  |  Domain preview                                    |
--  |____________________________________________________|
--
with Fuzzy.Feature.Domain_Floats;  use Fuzzy.Feature.Domain_Floats;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Expander;                 use Gtk.Expander;
with Gtk.Fuzzy_Object;             use Gtk.Fuzzy_Object;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Widget;                   use Gtk.Widget;

with Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Editor;
with Gtk.Fuzzy_Feature.Unit_Entry;
with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Float_Factory is

   Float_Factory_Class_Name : constant UTF8_String :=
                                   Prefix & "FloatFeatureFactory";
--
-- Gtk_Float_Factory_Record -- The widget type
--
-- Style properties :
--
--    bounds-error            - The error  text  shown  when  the  lower
--                              range bound is greater  than  the  upper
--                              one.  String,  the default is "The field
--                              to must be greater than from";
--    from-label              - The label of the  lower  bound.  String,
--                              the default is "From";
--    incompatible-from-error - The  error  text  shown when the unit of
--                              the from field is incompatible with  the
--                              scale. String, the default is "The field
--                              to has  a  unit  incompatible  with  the
--                              scale";
--    incompatible-to-error   - The  error  text  shown when the unit of
--                              the to field is  incompatible  with  the
--                              scale. String, the default is "The field
--                              to has  a  unit  incompatible  with  the
--                              scale";
--    intervals-label         - The  label   of  the  intervals  number.
--                              String, the default is "Intervals".
--    missing-from-error      - The error  text  shown  when  the  lower
--                              bound is missing. String, the default is
--                              "The field from must contain a number";
--    missing-intervals-error - The  error text shown when the number of
--                              intervals  is not specified. String, the
--                              default is  "The  field  intervals  must
--                              contain an integer number";
--    missing-to-error        - The error  text  shown  when  the  upper
--                              bound is missing. String, the default is
--                              "The field to must contain a number";
--    non-numeric-from-error  - The eror text shown when the upper bound
--                              field  does  not contain a valid number.
--                              String,  the  default is "The field from
--                              must contain a number";
--    non-numeric-to-error    - The eror text shown when the lower bound
--                              field  does  not contain a valid number.
--                              String,  the  default is "The field from
--                              must contain a number";
--    overflow-from-error     - The error  text  shown  when  the  lower
--                              bound is too big. String, the default is
--                              "The  field  from  contains  a too large
--                              number";
--    overflow-to-error       - The error  text  shown  when  the  upper
--                              bound is too big. String, the default is
--                              "The  field  from  contains  a too large
--                              number";
--    preview-label           - The title text shown above the  preview.
--                              String, the default is "Feature domain";
--    to-label                - The label of the  upper  bound.  String,
--                              the default is "To";
--    unit-from-error         - The  error  text  shown when the unit of
--                              the  from  field is illegal. String, the
--                              default  is  "Illegal  unit  of  the  to
--                              field";
--    unit-label              - The  label  of  the  scale  edit  field.
--                              String, the default is "Unit";
--    unit-scale-error        - The  error  text  shown when the unit of
--                              the scale field is illegal. String,  the
--                              default is "Illegal unit  of  the  scale
--                              field";
--    unit-to-error           - The  error  text  shown when the unit of
--                              the  to  field  is  illegal. String, the
--                              default  is  "Illegal  unit  of  the  to
--                              field".
--
--  + Styles from Install_Factory_Style_Properties
--
   type Gtk_Float_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Float_Factory is
      access all Gtk_Float_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Float_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Float_Factory_Record
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
--    Feature    - To initialize the fields of
--    Editable   - Initial state
--    Preview_On - Initial state of preview
--
-- The  parameter  Feature  can  be  an invalid handle. In that case the
-- fields left uninitialized. When Feature is  valid  it  must  refer  a
-- float feature, otherwise Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Gtk_New
             (  Widget     : out Gtk_Float_Factory;
                Feature    : Feature_Handle := No_Feature;
                Editable   : Boolean        := True;
                Preview_On : Boolean        := True
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget     - The widget to initialize
--    Feature    - To initialize the fields of
--    Editable   - Initial state
--    Preview_On - Initial state of preview
--    Rows       - The number of additional rows above the preview
--    Columns    - The number of additional columns on the right
--
-- Exceptions :
--
--    Constraint_Error - Feature is not a float feature
--
   procedure Initialize
             (  Widget     : not null access
                             Gtk_Float_Factory_Record'Class;
                Feature    : Feature_Handle;
                Editable   : Boolean;
                Preview_On : Boolean;
                Rows       : Natural := 0;
                Columns    : Natural := 0
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget :not null  access Gtk_Float_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access Gtk_Float_Factory_Record;
                Editable : Boolean
             );
--
-- Style_Changed -- Notification upon style change
--
--    Widget - The widget
--
   procedure Style_Changed
             (  Widget : not null access Gtk_Float_Factory_Record
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Float_Factory_Record
             );
--
-- Update -- Notification upon fields changes
--
--    Widget - The widget
--
   procedure Update
             (  Widget : not null access Gtk_Float_Factory_Record
             );
private
   use Fuzzy.Feature.Domain_Floats.Float_Measures;
   use Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Editor;
   use Gtk.Fuzzy_Feature.Unit_Entry;

   type Gtk_Float_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      From_Label      : Gtk_Label;
      From_Entry      : Gtk_Entry;
      From_Hint       : Gtk_HBox;
      To_Label        : Gtk_Label;
      To_Entry        : Gtk_Entry;
      To_Hint         : Gtk_HBox;
      Intervals_Label : Gtk_Label;
      Intervals_Entry : Gtk_Entry;
      Intervals_Hint  : Gtk_HBox;
      Scale_Label     : Gtk_Label;
      Scale_Entry     : Gtk_Unit_Entry;
      Scale_Hint      : Gtk_HBox;
      Domain          : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
      Preview         : Gtk_Expander;
      Rows            : GUInt;
      Columns         : GUInt;
      Modified        : Boolean := False;
   end record;
--
-- Float_Data -- Feature data
--
   type Float_Data is record
      Cardinality : Positive;
      From        : Domain_Float;
      To          : Domain_Float;
      Scale       : Measure;
   end record;
--
-- Get -- Current widget's data
--
   function Get
            (  Widget : not null access Gtk_Float_Factory_Record;
               Data   : not null access Float_Data
            )  return String;
--
-- Get_Name -- Of a variable in the preview
--
--    Widget      - The widget
--    Cardinality - Of the feature
--    Value       - Of the variable 1..Cardinality
--
   function Get_Name
            (  Widget      : not null access Gtk_Float_Factory_Record;
               Cardinality : Positive;
               Value       : Positive
            )  return String;

   procedure Changed
             (  Control : access Gtk_Widget_Record'Class;
                Widget  : Gtk_Float_Factory
             );
--
-- Set_Col_Spacings -- Overrides Gtk.Table
--
   overriding
   procedure Set_Col_Spacings
             (  Widget  : not null access Gtk_Float_Factory_Record;
                Spacing : Guint
             );

   procedure Style_Updated
             (  Widget : access Gtk_Float_Factory_Record'Class
             );

   package Handlers is
      new Gtk.Handlers.Callback
          (  Gtk_Float_Factory_Record
          );

   package Action_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Float_Factory
          );
end Gtk.Fuzzy_Feature.Float_Factory;
