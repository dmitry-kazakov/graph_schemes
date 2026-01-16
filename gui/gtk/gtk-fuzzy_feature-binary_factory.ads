--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Binary_Factory            Luebeck            --
--  Interface                                      Summer, 2009       --
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
--
--  This package provides a GTK+ widget for viewing/editing binary fuzzy
--  features. It has the look:
--      .______________.___________________________________.________.
--      |              |                                   |        |
--      | Base feature | Feature path                      | Status |
--      |______________|_______.________.__________________|________|
--      |              |       |        |                           |
--      |          Bit | Combo | Status | [x] Independent check box |
--      |______________|_______|________|___________________________|
--
with Gtk.Fuzzy_Object;    use Gtk.Fuzzy_Object;
with Gtk.Box;             use Gtk.Box;
with Gtk.Check_Button;    use Gtk.Check_Button;
with Gtk.Combo_Box_Text;  use Gtk.Combo_Box_Text;
with Gtk.Label;           use Gtk.Label;
with Gtk.Widget;          use Gtk.Widget;

with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Binary_Factory is

   Binary_Factory_Class_Name : constant UTF8_String :=
                                  Prefix & "BinaryFeatureFactory";
--
-- Gtk_Binary_Factory_Record -- The widget type
--
-- Style properties :
--
--    base-feature-label    - The label of the base feature. String, the
--                            default is "Feature".
--    bit-label             - The  label  of  the bit combo. String, the
--                            default is "Bit position".
--    independent-label     - The label of the independent feature check
--                            button.    String,    the    default    is
--                            "Independent".
--    missing-feature-error - The  error  text  shown when no feature is
--                            specified. String, the default is "A  base
--                            feature must  be  specified.  The  feature
--                            value is to be  determined  from  bits  of
--                            positions  of  the  base  feature   domain
--                            values".
--    no-bit-error          - The error text shown when no  feature  bit
--                            is  specified.  String, the default is "No
--                            bit selected".
--    wrong-feature -error  - The eror text shown when the path does not
--                            specify a feature. String, the default  is
--                            "The path does not specify a feature".
--
--  + Styles from Install_Hints_Style_Properties
--
   type Gtk_Binary_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Binary_Factory is
      access all Gtk_Binary_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Binary_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Binary_Factory_Record
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
--    Widget      - The result
--    Picker      - Object picker for reference feature selection
--    Constraint  - Picking constraint to combine with
--    Feature     - To initialize the fields of
--    Editable    - Initial state
--    Independent - If Feature is invalid
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
             (  Widget      : out Gtk_Binary_Factory;
                Picker      : not null access
                              Gtk_Object_Picker_Record'Class;
                Constraint  : Picker_Constraint;
                Feature     : Feature_Handle := No_Feature;
                Editable    : Boolean        := True;
                Independent : Boolean        := False
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget      - The widget to initialize
--    Picker      - Object picker for reference feature selection
--    Constraint  - Picking constraint to combine with
--    Feature     - To initialize the fields of
--    Editable    - Initial state
--    Independent - If Feature is invalid
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Initialize
             (  Widget      : not null access
                              Gtk_Binary_Factory_Record'Class;
                Picker      : not null access
                              Gtk_Object_Picker_Record'Class;
                Constraint  : Picker_Constraint;
                Feature     : Feature_Handle;
                Editable    : Boolean;
                Independent : Boolean
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget : not null access Gtk_Binary_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access Gtk_Binary_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Binary_Factory_Record
             );
private
   type Gtk_Binary_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      Feature_Label : Gtk_Label;
      Feature_Entry : Gtk_Picker_Box;
      Feature_Hint  : Gtk_Box;
      Bit_Label     : Gtk_Label;
      Bit_Combo     : Gtk_Combo_Box_Text;
      Bit_Hint      : Gtk_Box;
      Independent   : Gtk_Check_Button;
      Modified      : Boolean := False;
   end record;

   procedure Changed
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             );
   procedure Get
             (  Widget       : not null access
                               Gtk_Binary_Factory_Record;
                Feature      : out Feature_Handle;
                Independent  : out Boolean;
                Bit_Position : out Natural
             );
   function Get_Feature
            (  Widget : not null access Gtk_Binary_Factory_Record
            )  return Feature_Handle;
   procedure Set_Feature
             (  Factory     : not null access Gtk_Binary_Factory_Record;
                Feature     : Feature_Handle;
                Change_Path : Boolean
             );
   procedure Set_Feature
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             );
   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Binary_Factory
             );

   package Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Binary_Factory
          );
end Gtk.Fuzzy_Feature.Binary_Factory;
