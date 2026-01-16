--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Factory                   Luebeck            --
--  Interface                                      Winter, 2007       --
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
--
--  This package a GTK+ widget for fuzzy features factory:
--     ._____________________..___________________.
--     |                     ||                   |
--     |  Feature selection  ||  Feature factory  |
--     |    (tree view)      ||   (edit fields)   |
--     |                     ||                   |
--     |_____________________||___________________|
--                         HPaned
--
with Fuzzy.Feature.Handle;         use Fuzzy.Feature.Handle;
with Gtk.Fuzzy_Feature;            use Gtk.Fuzzy_Feature;
with Gtk.Fuzzy_Feature.Selection;  use Gtk.Fuzzy_Feature.Selection;
with Gtk.Fuzzy_Object;             use Gtk.Fuzzy_Object;
with Gtk.Paned;                    use Gtk.Paned;
with Gtk.Widget;                   use Gtk.Widget;

with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Factory is

   Class_Name : constant String := Prefix & "FeatureFactory";
--
-- Gtk_Fuzzy_Feature_Factory_Record -- Feature selection widget
--
-- Style properties :
--
--    button-spacing - The  spacing used in button boxes. GUInt. Default
--                     is 3.
--    column-spacing - The  column  spacing  used in the feature factory
--                     pane. The pane is a table widget. The elements of
--                     feature factory are table  cells.  This  property
--                     sets the column spacing. GUInt. Default is 3.
--    row-spacing    - The row spacing used in the feature factory pane.
--                     GUInt. Default is 3.
--
   type Gtk_Fuzzy_Feature_Factory_Record is
      new Gtk_HPaned_Record with private;
   type Gtk_Fuzzy_Feature_Factory is
      access all Gtk_Fuzzy_Feature_Factory_Record'Class;
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
--    Use_Error        - No feature type selected
--
   function Create
            (  Widget : not null access
                        Gtk_Fuzzy_Feature_Factory_Record;
               Name   : UTF8_String
            )  return Feature_Handle;
--
-- Get_Constraint -- The constraint
--
--    Widget - The widget
--
-- Returns :
--
--    The object picking constraint imposed by the widget's components
--
   function Get_Constraint
            (  Widget : not null access Gtk_Fuzzy_Feature_Factory_Record
            )  return Picker_Constraint;
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
--    Widget            - The result
--    Feature_Picker    - The feature objects picker
--    Classifier_Picker - The classifier objects picker
--    Feature           - To initialize the widget
--    Editable          - Initial state
--
-- When  Feature  is valid it is used to initialize the selection of the
-- widget and the corresponding factory widget.
--
   procedure Gtk_New
             (  Widget            : out Gtk_Fuzzy_Feature_Factory;
                Feature_Picker    : Gtk_Object_Picker := null;
                Classifier_Picker : Gtk_Object_Picker := null;
                Feature           : Feature_Handle    := No_Feature;
                Editable          : Boolean           := True
             );
--
-- Initialize -- Construction
--
--    Widget            - To initialize
--    Feature_Picker    - The feature objects picker
--    Classifier_Picker - The classifier objects picker
--    Feature           - To initialize the widget with
--
-- Any  derived  type  is responsible to call to Initialize from its own
-- Initialize.
--
   procedure Initialize
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Factory_Record'Class;
                Feature_Picker    : Gtk_Object_Picker;
                Classifier_Picker : Gtk_Object_Picker;
                Feature           : Feature_Handle;
                Editable          : Boolean
             );
--
-- Is_Editable -- Get editable flag
--
--    Widget   - The factory widget
--
-- Returns :

--    True when factory fields are editable
--
   function Is_Editable
            (  Widget : not null access Gtk_Fuzzy_Feature_Factory_Record
            )  return Boolean;
--
-- Set_Editable -- Set editable flag
--
--    Widget   - The factory widget
--    Editable - True when factory fields are editable (default)
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Feature_Factory_Record;
                Editable : Boolean
             );
--
-- Set_Spacing -- Set spacing values
--
--    Widget - The factory widget
--    Button - Button spacing to set
--    Column - Column spacing to set
--    Row    - Row spacing to set
--
   procedure Set_Spacing
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Factory_Record;
                Button  : GUInt;
                Column  : GUInt;
                Row     : GUInt
             );
--
-- Verify -- The fields of the widget
--
--    Widget - The factory widget
--
-- This procedure verifies the fields in the widget. Constraint_Error is
-- propagated when fields are incorrectly set.  This  procedure  can  be
-- used to check if Create could create the feature.
--
-- Exceptions :
--
--    Constraint_Error - Wrong fields
--    Use_Error        - No feature type selected
--
   procedure Verify
             (  Widget : not null access
                         Gtk_Fuzzy_Feature_Factory_Record
             );
private
   type Selection_Record
        (  Parent : not null access
                    Gtk_Fuzzy_Feature_Factory_Record'Class
        )  is new Gtk_Feature_Selection_Record with null record;
   type Selection_Ptr is access all Selection_Record'Class;

   overriding
      procedure On_Selection_Change
                (  Widget     : not null access Selection_Record;
                   Selected   : Gtk_Fuzzy_Feature_Abstract_Factory;
                   Deselected : Gtk_Fuzzy_Feature_Abstract_Factory
                );

   type Gtk_Fuzzy_Feature_Factory_Record is
      new Gtk_HPaned_Record with
   record
      Editable       : Boolean := True;
      Spacing_Set    : Boolean := False;
      Button_Spacing : GUInt;
      Column_Spacing : GUInt;
      Row_Spacing    : GUInt;
      Selection      : Selection_Ptr;
   end record;
--
-- Change_Spacing -- Change spacing values
--
--    Widget - The factory widget
--    Button - Button spacing to set
--    Column - Column spacing to set
--    Row    - Row spacing to set
--
   procedure Change_Spacing
             (  Widget  : not null access
                          Gtk_Fuzzy_Feature_Factory_Record;
                Button  : GUInt;
                Column  : GUInt;
                Row     : GUInt
             );
--
-- On_Destroy -- The handler of destroy event
--
   procedure On_Destroy
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Fuzzy_Feature_Factory
             );
--
-- Style_Updated -- The handler for style-updated event
--
   procedure Style_Updated
             (  Widget  : access Gtk_Widget_Record'Class;
                Factory : Gtk_Fuzzy_Feature_Factory
             );
--
-- Widget_Handlers -- Widget signal handlers
--
   package Widget_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Widget_Record,
             Gtk_Fuzzy_Feature_Factory
          );

end Gtk.Fuzzy_Feature.Factory;
