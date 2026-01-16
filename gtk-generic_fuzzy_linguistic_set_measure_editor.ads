--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_               Luebeck            --
--        Set_Measure_Editor                       Spring, 2007       --
--  Interface                                                         --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Gtk.Box;     use Gtk.Box;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Paned;   use Gtk.Paned;
with Gtk.Widget;  use Gtk.Widget;
with Units;       use Units;

with Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Zoom_Panel;
with Gtk.Generic_Fuzzy_Linguistic_Set_Editor;
with Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View;

generic
   with package Measure_Tree_View is
      new Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Tree_View (<>);
   with package Zoom_Panel is
      new Measure_Tree_View.Domain_Of.Generic_Zoom_Panel (<>);
   with package Editor is
      new Gtk.Generic_Fuzzy_Linguistic_Set_Editor
          (  Domain     => Measure_Tree_View.Domain_Of,
             Zoom_Panel => Zoom_Panel,
             Tree_View  => Measure_Tree_View.Tree_View_Of
          );
package Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Editor is
   package Measure_Tree_View_Of renames Measure_Tree_View;
   package Zoom_Panel_Of        renames Zoom_Panel;
   package Editor_Of            renames Editor;
   use Measure_Tree_View_Of;
   use Fuzzy_Measure_Linguistic_Sets_Of;
   use Measure_Tree_View.Fuzzy_Measures_Of.
       Interval_Measures.Float_Measures;
   use Measure_Tree_View.Fuzzy_Measure_Linguistic_Sets_Of.
       Fuzzy_Linguistic_Sets_Of;
--
-- Gtk_Fuzzy_Linguistic_Set_Measure_Editor -- Editor of linguistic sets
--
   type Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Measure_Editor is
      access all Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record'Class;
--
-- Edited -- Get the modification flag
--
--    Widget - The widget
--
-- The result is true if the inidcated set was edited by the user.
--
-- Returns :
--
--    The modification flag
--
   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Boolean;
--
-- Get -- The fuzzy linguistic set of
--
--    Widget - The widget
--    Purge  - Remove redindant points of the variables
--
-- This  function  returns the set currently indicated by the widget. It
-- is  checked  for  being correct, that is it contains no duplicated or
-- improperly spelled  names  of  variables.  When  Purge  is  True  the
-- variables  of  the  result  set  are checked for redundant membership
-- points and these points are removed.
--
-- Returns :
--
--    The linguistic set
--
-- Exceptions :
--
--    Constraint_Error - Illegal names in the set
--    Name_Error       - Duplicated names in the set
--
   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
               Purge  : Boolean := True
            )  return Set_Measure;
--
-- Get -- The fuzzy linguistic set of
--
--    Widget - The widget
--    Value  - Dimensionless fuzzy linguistic set
--    Scale  - The scale
--    Purge  - Remove redindant points of the variables
--
-- This procedure returns the set currently indicated by the widget as a
-- dimensionless  set  and  its  scale. It is checked for being correct,
-- that is it contains no duplicated  or  improperly  spelled  names  of
-- variables. When Purge is True the variables of  the  result  set  are
-- checked for redundant membership points and these points are removed.
--
-- Exceptions :
--
--    Constraint_Error - Illegal names in the set
--    Name_Error       - Duplicated names in the set
--
   procedure Get
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : out Linguistic_Set;
                Scale  : out Measure;
                Purge  : Boolean := True
             );
--
-- Get_Edit_Buttons -- The box with editing buttons
--
--    Widget - The widget
--
-- Returns :
--
--    The box of the widget with the editing buttons
--
   function Get_Edit_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box;
--
-- Get_Pane -- The widget's pane
--
--    Widget - The widget
--
-- Returns :
--
--    The pane separating tree view and the domain view
--
   function Get_Pane
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Paned;
--
-- Get_Tracker -- The box with the mouse tracking labels
--
--    Widget - The widget
--
-- Returns :
--
--    The box of the widget with the mouse tracking labels
--
   function Get_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box;
--
-- Get_Tree_View -- The fuzzy linguistic set of
--
--    Widget - The widget
--
-- This function returns the widget that the editor  or  viewer  use  to
-- display the variables list.
--
-- Returns :
--
--    The tree view widget used in
--
   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View;
--
-- Get_View_Buttons -- The box with viewing buttons
--
--    Widget - The widget
--
-- Returns :
--
--    The box of the widget with the viewing buttons
--
   function Get_View_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box;
--
-- Get_Unit -- Get unit
--
--    Widget - The widget
--
-- Returns :
--
--    SI component
--
   function Get_Unit
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Unit;
--
-- Gtk_New -- Factory
--
--    Widget   - The result, an editor or viewer (output)
--    Value    - The initial value
--    Scale    - The preferred unit
--    Layout   - Of the result (vertical or horizontal pane
--    Editable - True if the widget is editable
--
-- Exceptions :
--
--     Unit_Error - Incompatible scale
--     Data_Error - Illegal scale
--
   procedure Gtk_New
             (  Widget   : out Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
                Value    : Set_Measure;
                Scale    : UTF8_String     := "";
                Layout   : Gtk_Orientation := Orientation_Vertical;
                Editable : Boolean         := True
             );
--
-- Initialize -- Construction
--
--    Widget   - The result, an editor or viewer (output)
--    Value    - The initial value
--    Scale    - The preferred unit
--    Layout   - Of the result (vertical or horizontal pane
--    Editable - True if the widget is editable
--  [ View ]   - The tree view widget to use (a factory of)
--
-- A  derived  type  is  responsible  to call to this procedure from its
-- Initialize.
--
-- Exceptions :
--
--    Constraint_Error - Scale's gain is not positiive
--    Data_Error       - Illegal scale
--    Unit_Error       - Incompatible scale
--
   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record'Class;
                Value    : Set_Measure;
                Scale    : UTF8_String;
                Layout   : Gtk_Orientation;
                Editable : Boolean
             );
   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record'Class;
                Value    : Set_Measure;
                Scale    : UTF8_String;
                Layout   : Gtk_Orientation;
                Editable : Boolean;
                View     : Gtk_Fuzzy_Measure_Tree_View_Factory'Class
             );
--
-- Is_Editable -- Get the editable flag
--
--    Widget - The widget
--
-- Returns :
--
--    The editable flag
--
   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Boolean;
--
-- Put -- A set of linguistic variables
--
--    Widget - The widget
--    Value  - To be set
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections  made  and erases the undo and redo buffers. The dimension
-- of Value must be  compatible  with  the  widget's  units.  Otherwise,
-- Constraint_Error is propagated.
--
-- Exceptions :
--
--    Ubit_Error - Incompatible units
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : Set_Measure
             );
--
-- Put -- A set of linguistic variables
--
--    Widget - The widget
--    Value  - To be set
--    Scale  - The preferred unit
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections  made  and erases the undo and redo buffers. The parameter
-- Scale is the preferred  dimension  to  be  used  to  indicate  domain
-- values.  It  must  be  a valid dimension specification as expected by
-- Measures_UTF8_Edit.Value. The dimension must be compatible  with  one
-- of Value otherwise Unit_Error is propagated. Scale can  be  an  empty
-- string, in which case the unit (Value.SI, 1.0, Value.Offset) will  be
-- used as the scale.
--
-- Exceptions :
--
--     Unit_Error - Incompatible scale
--     Data_Error - Illegal scale
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : Set_Measure;
                Scale  : UTF8_String
             );
--
-- Set_Editable -- Change the editable flag
--
--    Widget   - The widget
--    Editable - The value of editable flag to set
--
-- This procedure changes the editable flag.
--
   procedure Set_Editable
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Editable : Boolean
             );
private
   use Fuzzy_Measures.Interval_Measures_Of.Float_Measures_Of;
   use Fuzzy_Measure_Linguistic_Sets.Fuzzy_Linguistic_Sets_Of;
   use Editor;
--
-- Gtk_Fuzzy_Linguistic_Set_Measure_Editor
--
   type Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record is
      new Gtk_Fuzzy_Linguistic_Set_Editor_Record with null record;

end Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Editor;
