--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.                          Luebeck            --
--        Discrete_Factory                         Winter, 2007       --
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
--  This  package  provides  a  GTK+ widget for viewing/editing discrete
--  fuzzy features. It has the look:
--      ._______________.__________.
--      |               |          |
--      |  Values list  | Buttons  |
--      |               | box      |
--      |               |          |
--      |_______________|__________|
--
with Gtk.Frame;         use Gtk.Frame;
with Gtk.Fuzzy_Object;  use Gtk.Fuzzy_Object;

with Fuzzy.Feature.Domain_Floats;
with Gtk.Box;
with Gtk.Fuzzy_Feature.Linguistic_Set_Tree_View;

package Gtk.Fuzzy_Feature.Discrete_Factory is

   Discrete_Factory_Class_Name : constant UTF8_String :=
                                     Prefix & "DiscreteFeatureFactory";
--
-- Gtk_Discrete_Factory_Record -- The widget type
--
-- Style properties :
--
--    empty-error     - The error  text  shown  when  no  domain  values
--                      specified. String, the default is "here  has  to
--                      be  at  least  one  name in the discrete feature
--                      domain set".
--    name-error      - The  error  text  shown when the domain contains
--                      wrong  names. String, the default is "The domain
--                      contains   illegal   names.   Wrong   names  are
--                      highlighted by a different color".
--    duplicate-error - The  error  text  shown when the domain contains
--                      duplicated names. String, the  default  is  "The
--                      domain  contains  duplicated  names. These names
--                      are highlighted by a selection".
--
   type Gtk_Discrete_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with private;
   type Gtk_Discrete_Factory is
      access all Gtk_Discrete_Factory_Record'Class;
--
-- Create -- Overrides Gtk.Fuzzy.Feature...
--
   overriding
   function Create
            (  Widget : not null access Gtk_Discrete_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Edited -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Edited
            (  Widget : not null access Gtk_Discrete_Factory_Record
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
--    Editable - Initial widget state
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
             (  Widget   : out Gtk_Discrete_Factory;
                Feature  : Feature_Handle := No_Feature;
                Editable : Boolean        := True
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget   - The widget to initialize
--    Feature  - To initialize the fields of
--    Editable - Initial widget state
--
-- Exceptions :
--
--    Constraint_Error - Feature is not an integer feature
--
   procedure Initialize
             (  Widget   : not null access
                           Gtk_Discrete_Factory_Record'Class;
                Feature  : Feature_Handle;
                Editable : Boolean
             );
--
-- Is_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   function Is_Editable
            (  Widget : not null access Gtk_Discrete_Factory_Record
            )  return Boolean;
--
-- Set_Button_Spacing -- Overrides Gtk.Fuzzy.Feature...
--
   overriding
   procedure Set_Button_Spacing
             (  Widget  : not null access Gtk_Discrete_Factory_Record;
                Spacing : GUInt
             );
--
-- Set_Editable -- Overrides Gtk.Fuzzy_Feature...
--
   overriding
   procedure Set_Editable
             (  Widget   : not null access Gtk_Discrete_Factory_Record;
                Editable : Boolean
             );
--
-- Verify -- Overrides Gtk.Fuzzy.Feature...
--
   overriding
   procedure Verify
             (  Widget : not null access Gtk_Discrete_Factory_Record
             );

private
   use Fuzzy.Feature.Domain_Floats.Variables;
   use Fuzzy.Feature.Domain_Floats.Variable_Sets;
   use Gtk.Box;
   use Linguistic_Set_Tree_View;

   type Gtk_Discrete_Factory_Record is
      new Gtk_Fuzzy_Feature_Abstract_Factory_Record with
   record
      Frame   : Gtk_Frame;
      List    : Gtk_Fuzzy_Linguistic_Set_Tree_View;
      Buttons : Gtk_VBox;
   end record;
--
-- Get -- The current set
--
--    Widget  - The widget
--
-- Returns :
--
--    The set (only its domain is used)
--
-- Exceptions :
--
--    Ones of Create and Cerify
--
   function Get
            (  Widget : not null access Gtk_Discrete_Factory_Record
            )  return Linguistic_Set;

end Gtk.Fuzzy_Feature.Discrete_Factory;
