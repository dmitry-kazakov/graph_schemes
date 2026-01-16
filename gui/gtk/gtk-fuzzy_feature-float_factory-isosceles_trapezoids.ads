--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Float_Factory.Isosceles_Trapezoids       Summer, 2007       --
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
--  This package provides a  GTK+  widget  for  viewing/editing  integer
--  fuzzy features. It has the look:
--     .______._______.__.___________._______.__.__________._______.__.
--     |      |       |  |           |       |  |          |       |  |
--     | From | Value |  | Intervals | Value |  | Shoulder | Value |  |
--     |______|_______|__|___________|_______|__|__________|_______|__|
--     |      |       |  |           |       |  |          |       |  |
--     |   To | Value |  |     Scale | Value |  |          |       |  |
--     |______|_______|__|___________|_______|__|__________|_______|__|
--     |                                                              |
--     |  Domain preview                                              |
--     |______________________________________________________________|
--
with Gtk.Fuzzy_Feature.Float_Factory;

package Gtk.Fuzzy_Feature.Float_Factory.Isosceles_Trapezoids is

   Isosceles_Trapezoids_Factory_Class_Name : constant UTF8_String :=
      Prefix & "IsoscelesTrapezoidsFeatureFactory";
--
-- Gtk_Isosceles_Trapezoids_Factory_Record -- The type
--
-- Style properties :
--
--    cardinality-error           - The   error   text  shown  when  the
--                                  cardinality is  roo  small.  String,
--                                  the default is "The cardinality must
--                                  be at least 2 when the  interval  is
--                                  empty. Otherwise, it must be greater
--                                  than 2";
--    incompatible-shoulder-error - The  error  text shown when the unit
--                                  of    the    shoulder    field    is
--                                  incompatible with the scale. String,
--                                  the default is "The  field  shoulder
--                                  has a  unit  incompatible  with  the
--                                  scale";
--    missing-shoulder-error      - The   error   text  shown  when  the
--                                  shoulder  is  missing.  String,  the
--                                  default  is "The field shoulder must
--                                  contain a number";
--    non-numeric-shoulder-error  - The  eror  text   shown   when   the
--                                  shoulder  field  does  not contain a
--                                  valid number. String, the default is
--                                  "The field shoulder contains  a  too
--                                  large number";
--    negative-shoulder-error     - The  eror  text   shown   when   the
--                                  shoulder field is negative.  String,
--                                  the default is "The  field  shoulder
--                                  must be non-negative";
--    overflow-shoulder-error     - The   error   text  shown  when  the
--                                  shoulder is  too  big.  String,  the
--                                  default   is   "The  field  shoulder
--                                  contains a too large number";
--    shoulder-label              - The label of  the shoulder.  String,
--                                  the default is "Shoulder";
--    unit-shoulder-error         - The   error   text  shown  when  the
--                                  shoulder   field   contains  illegal
--                                  unit.   String,   the   default   is
--                                  "Illegal  unit   of   the   shoulder
--                                  field".
--  + Styles from Gtk_Float_Factory_Record

   type Gtk_Isosceles_Trapezoids_Factory_Record is
      new Gtk_Float_Factory_Record with private;
   type Gtk_Isosceles_Trapezoids_Factory is access
      all Gtk_Isosceles_Trapezoids_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access
                  Gtk_Isosceles_Trapezoids_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
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
-- fields  left  uninitialized.  When  Feature is valid it must refer an
-- isoceles    trapezoids   feature,   otherwise   Constraint_Error   is
-- propagated.
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Gtk_New
             (  Widget     : out Gtk_Isosceles_Trapezoids_Factory;
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
--
-- Exceptions :
--
--    Constraint_Error - Feature is not a trapezoids feature
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record'Class;
                Feature    : Feature_Handle;
                Editable   : Boolean;
                Preview_On : Boolean
             );
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature.Float_Factory...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Isosceles_Trapezoids_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             );
--
-- Update -- Overrides Gtk.Fuzzy_Feature.Float_Factory...
--
   overriding
   procedure Update
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             );

private
   type Gtk_Isosceles_Trapezoids_Factory_Record is
      new Gtk_Float_Factory_Record with
   record
      Shoulder_Label : Gtk_Label;
      Shoulder_Entry : Gtk_Entry;
      Shoulder_Hint  : Gtk_HBox;
   end record;
--
-- Get -- Current widget's data
--
   function Get
            (  Widget   : not null access
                          Gtk_Isosceles_Trapezoids_Factory_Record;
               Data     : not null access Float_Data;
               Shoulder : not null access Domain_Float
            )  return UTF8_String;
--
-- Get_Name -- Overrides Gtk.Fuzzy_Feature.Float_Factory...
--
   overriding
   function Get_Name
            (  Widget      : not null access
                             Gtk_Isosceles_Trapezoids_Factory_Record;
               Cardinality : Positive;
               Value       : Positive
            )  return String;
--
-- Style_Changed -- Overrides Gtk.Fuzzy_Feature.Float_Factory...
--
   overriding
   procedure Style_Changed
             (  Widget : not null access
                         Gtk_Isosceles_Trapezoids_Factory_Record
             );

end Gtk.Fuzzy_Feature.Float_Factory.Isosceles_Trapezoids;
