--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature                           Luebeck            --
--  Interface                                      Spring, 2007       --
--                                                                    --
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

with Fuzzy.Feature;              use Fuzzy.Feature;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Logic;                use Fuzzy.Logic;
with Gtk.Fuzzy_Set_Entry;        use Gtk.Fuzzy_Set_Entry;
with Gtk.Table;                  use Gtk.Table;

with Fuzzy.Abstract_Edit.Handle;
with Fuzzy.Intuitionistic;

package Gtk.Fuzzy_Feature is
   pragma Elaborate_Body (Gtk.Fuzzy_Feature);
--
-- Feature_Entry_Edit -- Fuzzy feature rendering and editing parameters.
--                       A handle to Feature_Entry_Edit object is passed
-- to entry and cell renderer widgets which use it for text to value and
-- backward conversions.
--
   type Feature_Entry_Edit is new Abstract_Entry_Edit with record
      Feature : Feature_Handle;
      Input   : Input_Parameters;
      Output  : Output_Parameters;
   end record;
--
-- Gtk_Fuzzy_Feature_Abstract_Factory_Record -- Base  type for all fuzzy
--                                              features factory widgets
-- to derive from. Factory widgets can be stored into GTK+  values.  See
-- the package GLib.Values.Feature_Factory.
--
   type Gtk_Fuzzy_Feature_Abstract_Factory_Record is
      abstract new Gtk_Table_Record with null record;
   type Gtk_Fuzzy_Feature_Abstract_Factory is
      access all Gtk_Fuzzy_Feature_Abstract_Factory_Record'Class;
--
-- Create -- Factory
--
--    Feature - A feature
--    Input   - Input parameters
--    Output  - Output parameters
--
-- Returns :
--
--    A handle to
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create
            (  Feature : Feature_Handle;
               Input   : Input_Parameters'Class  := Input_Defaults;
               Output  : Output_Parameters'Class := Output_Defaults
            )  return Entry_Domain;
--
-- Create -- The feature
--
--    Widget - The factory widget
--    Name   - The feature name
--
-- This function creates a  new  feature  according  to  the  fields  of
-- Widget.  A  handle to the newly created feature is the result of. The
-- parameter Name determines the feature name. The implementation  shall
-- verify  the fields in the widget. Constraint_Error is propagated when
-- fields are incorrectly set.
--
-- Returns :
--
--    A handle to the feature created
--
-- Exceptions :
--
--    Constraint_Error - Wrong fields
--
   function Create
            (  Widget : not null access
                        Gtk_Fuzzy_Feature_Abstract_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle is abstract;
--
-- Edited -- Get the modification flag
--
--    Widget - The widget
--
-- The result is true if the inidcated feature was edited by the user.
--
-- Returns :
--
--    The modification flag
--
   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Feature_Abstract_Factory_Record
            )  return Boolean is abstract;
--
-- Get_Cardinality -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   function Get_Cardinality (Editor : Feature_Entry_Edit)
      return Natural;
--
-- Get_Default -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   function Get_Default (Editor : Feature_Entry_Edit)
      return Fuzzy_Boolean;
--
-- Get_Domain -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   function Get_Domain
            (  Editor : Feature_Entry_Edit
            )  return Fuzzy.Abstract_Edit.Handle.Handle;
--
-- Image -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Set
            )  return UTF8_String;
   overriding
   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Set
            )  return UTF8_String;
   overriding
   function Image
            (  Editor : Feature_Entry_Edit;
               Value  : Fuzzy.Intuitionistic.Classification
            )  return UTF8_String;
--
-- Is_Editable -- Get editable flag
--
--    Widget   - The factory widget
--
-- Returns :

--    True when factory fields are editable
--
   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Feature_Abstract_Factory_Record
            )  return Boolean is abstract;
--
-- Set_Button_Spacing -- Set button spacing
--
--    Widget  - The factory widget
--    Spacing - The spacing
--
-- The  default  implementation  does  nothing.  The  property  controls
-- spacing between buttons of the specific factory widget.
--
   procedure Set_Button_Spacing
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Abstract_Factory_Record;
                Spacing : GUInt
             );
--
-- Set_Default -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   procedure Set_Default
             (  Editor  : in out Feature_Entry_Edit;
                Default : Fuzzy_Boolean
             );
--
-- Set_Editable -- Set editable flag
--
--    Widget   - The factory widget
--    Editable - True when factory fields are editable (default)
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Feature_Abstract_Factory_Record;
                Editable : Boolean
             )  is abstract;
--
-- Value -- Overrides Gtk.Fuzzy_Set_Entry...
--
   overriding
   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Set;
   overriding
   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function Value
            (  Editor : Feature_Entry_Edit;
               Text   : UTF8_String
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Verify -- The fields of the widget
--
--    Widget - The factory widget
--
-- This procedure verifies the fields in the widget. Constraint_Error is
-- propagated when fields are incorrectly set.
--
-- Exceptions :
--
--    Constraint_Error - Wrong fields
--
   procedure Verify
             (  Widget : not null access
                         Gtk_Fuzzy_Feature_Abstract_Factory_Record
             )  is abstract;

end Gtk.Fuzzy_Feature;
