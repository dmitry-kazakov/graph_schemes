--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Linguistic_Factory                       Autumn, 2007       --
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
--  This  package  provides a GTK+ widget for viewing/editing linguistic
--  fuzzy features. It has the look:
--  ._______._______.___________________________.
--  |       |       |                           |
--  | Unit  | Edit  |                           |
--  |_______|_______|___________________________|
--  |                                           |
--  |                                           |
--  |  Linguistic set editor                    |
--  |                                           |
--  |___________________________________________|
--
with Gtk.Fuzzy_Object;  use Gtk.Fuzzy_Object;
with Gtk.Box;           use Gtk.Box;
with Gtk.GEntry;        use Gtk.GEntry;
with Gtk.Label;         use Gtk.Label;
with Gtk.Widget;        use Gtk.Widget;

with Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Editor;
with Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Tree_View;
with Gtk.Fuzzy_Feature.Unit_Entry;
with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Linguistic_Factory is

   Linguistic_Factory_Class_Name : constant UTF8_String :=
                                   Prefix & "LinguisticFeatureFactory";
--
-- Gtk_Linguistic_Factory_Record -- The widget type
--
-- Style properties :
--
--    unit-label       - The label of the scale edit field. String,  the
--                       default is "Unit";
--    unit-scale-error - The error text shown when the unit of the scale
--                       field  is  illegal.  String,  the  default   is
--                       "Illegal unit of the scale field".
--
--  + Styles from Install_Factory_Style_Properties
--
   type Gtk_Linguistic_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Linguistic_Factory is
      access all Gtk_Linguistic_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Linguistic_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Linguistic_Factory_Record
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
--    Widget   - The result
--    Feature  - To initialize the fields of
--    Editable - Initial state
--
-- The  parameter  Feature  can  be  an invalid handle. In that case the
-- fields left uninitialized. When Feature is  valid  it  must  refer  a
-- linguistic feature, otherwise Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Gtk_New
             (  Widget   : out Gtk_Linguistic_Factory;
                Feature  : Feature_Handle := No_Feature;
                Editable : Boolean        := True
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - The widget to initialize
--    Feature  - To initialize the fields of
--    Editable - Initial state
--
-- Exceptions :
--
--    Constraint_Error - Feature is not a linguistic feature
--
   procedure Initialize
             (  Widget   : not null access
                           Gtk_Linguistic_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget : not null access Gtk_Linguistic_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Linguistic_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Linguistic_Factory_Record
             );
--
-- Update -- Notification upon fields changes
--
--    Widget - The widget
--
   procedure Update
             (  Widget : not null access Gtk_Linguistic_Factory_Record
             );

private
   use Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Editor;
   use Gtk.Fuzzy_Feature.Linguistic_Set_Measure_Tree_View;
   use Gtk.Fuzzy_Feature.Unit_Entry;

   type Gtk_Linguistic_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      Scale_Label : Gtk_Label;
      Scale_Entry : Gtk_Unit_Entry;
      Scale_Hint  : Gtk_HBox;
      Domain      : Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
      Modified    : Boolean := False;
   end record;

   procedure Changed
             (  Control : access Gtk_Widget_Record'Class;
                Widget  : Gtk_Linguistic_Factory
             );

   procedure Destroy
             (  Widget : access Gtk_Linguistic_Factory_Record'Class
             );

   procedure Style_Updated
             (  Widget : access Gtk_Linguistic_Factory_Record'Class
             );

   package Handlers is
      new Gtk.Handlers.Callback
          (  Gtk_Linguistic_Factory_Record
          );

   package Action_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Linguistic_Factory
          );
   package Entry_Boolean_Handlers is
      new Gtk.Handlers.User_Return_Callback
          (  Gtk_Entry_Record,
             Boolean,
             Gtk_Linguistic_Factory
          );
end Gtk.Fuzzy_Feature.Linguistic_Factory;
