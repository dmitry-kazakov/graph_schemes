--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Classificatory_Factory                   Summer, 2008       --
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
--  This   package   provides   a   GTK+   widget   for  viewing/editing
--  classificatory fuzzy features. It has the look:
--      ._____________________.___________________.________.
--      |                     |                   |        |
--      |         Classifier  |  Classifier path  | Status |
--      |_____________________|___________________|________|
--      |                     |                   |        |
--      |     Generalization  |  Method of,       | Status |
--      |                     |  selection box    |        |
--      |_____________________|___________________|________|
--      |                     |                   |        |
--      |          Threshold  |  Confidence       | Status |
--      |_____________________|___________________|________|
--
with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Fuzzy_Object;         use Gtk.Fuzzy_Object;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Handlers;
with Gtk.Fuzzy_Generalization_Combo;

package Gtk.Fuzzy_Feature.Classificatory_Factory is
   use Gtk.Fuzzy_Generalization_Combo;

   Classificatory_Factory_Class_Name : constant UTF8_String :=
      Prefix & "ClassificatoryFeatureFactory";
--
-- Gtk_Classificatory_Factory_Record -- The widget type
--
-- Style properties :
--
--    classifier-label             - The  label  of  the classifier path
--                                   field.   String,   the  default  is
--                                   "Classifier".
--    generalization-label         - The  label  of  the  generalization
--                                   selection combo  box.  String,  the
--                                   default is "Generalization method".
--    missing-classifier-error     - The  error  text  shown  when   the
--                                   classifier is missing. String,  the
--                                   default  is  "A  classifier must be
--                                   specified.  The feature value is to
--                                   be   determined   by   classifier's
--                                   outcome  on the training example in
--                                   question".
--    missing-threshold-error      - The  error  text  shown  when   the
--                                   threshold value is missing. String,
--                                   the  default is "The threshold must
--                                   contain a numeric truth value 0..1.
--                                   The value  defines  the  truncation
--                                   level.  All  values  less than this
--                                   threshold  are treated as 0. Higher
--                                   values of the threshold  may  speed
--                                   up  classification,  though make it
--                                   less accurate".
--    non-numeric-threshold-error  - The   eror   text  shown  when  the
--                                   threshold  value  field  does   not
--                                   contain a valid number. String, the
--                                   default  is  "The  threshold   must
--                                   contain   a   numeric  truth  value
--                                   0..1.".
--    range-threshold-error        - The  error  text  shown  when   the
--                                   threshold  is not in range. String,
--                                   the  default is "The threshold must
--                                   be in range 0..1.".
--    threshold-label              - The label of the  threshold  field.
--                                   String, the default is "Threshold".
--    wrong-classifier-error       - The  error  text  shown  when   the
--                                   classifier is  wrong.  String,  the
--                                   default  is  "The  path  does   not
--                                   specify a classifier".
--
--  + Styles from Install_Hints_Style_Properties
--
   type Gtk_Classificatory_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Classificatory_Factory is
      access all Gtk_Classificatory_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Boolean;
--
-- Get_Type -- Get the type of the widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Type return Gtk_Type;
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
             (  Widget     : out Gtk_Classificatory_Factory;
                Picker     : not null access
                             Gtk_Object_Picker_Record'Class;
                Constraint : Picker_Constraint;
                Feature    : Feature_Handle    := No_Feature;
                Editable   : Boolean           := True
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
                             Gtk_Classificatory_Factory_Record'Class;
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
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Classificatory_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access
                         Gtk_Classificatory_Factory_Record
             );

private
   type Gtk_Classificatory_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      Method_Label     : Gtk_Label;
      Method_Combo     : Gtk_Fuzzy_Generalization_Combo;
      Classifier_Label : Gtk_Label;
      Classifier_Entry : Gtk_Picker_Box;
      Classifier_Hint  : Gtk_Box;
      Threshold_Label  : Gtk_Label;
      Threshold_Entry  : Gtk_Entry;
      Threshold_Hint   : Gtk_Box;
      Modified         : Boolean := False;
   end record;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             );
   function Get_Classifier
            (  Widget : not null access
                        Gtk_Classificatory_Factory_Record
            )  return Classifier_Handle;
   procedure Set_Classifier
             (  Factory    : not null access
                             Gtk_Classificatory_Factory_Record;
                Classifier : Classifier_Handle
             );
   procedure Set_Classifier
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             );
   procedure Set_Feature
             (  Factory : not null access
                          Gtk_Classificatory_Factory_Record;
                Feature : Feature_Handle
             );
   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Classificatory_Factory
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Classificatory_Factory
          );

end Gtk.Fuzzy_Feature.Classificatory_Factory;
