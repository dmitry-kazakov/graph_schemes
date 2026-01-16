--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Editor     Luebeck            --
--  Interface                                      Spring, 2007       --
--                                                                    --
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
--
--  This generic package provides a composite widget fot editing sets of
--  fuzzy  linguistic  variables.  The  formal parameters of the package
--  are:
--
--  (o)  Domain is an instance  of  the  generic  domain  representation
--       package Gtk.Generic_Fuzzy_Linguistic_Set_Domain;
--  (o)  Zoom_Panel is an instance of its child Generic_Zoom_Panel;
--  (o)  Tree_View is an instance of the child Generic_Tree_View.
--
with Gtk.Box;     use Gtk.Box;
with Gtk.Enums;   use Gtk.Enums;
with Gtk.Paned;   use Gtk.Paned;
with Gtk.Table;   use Gtk.Table;
with Gtk.Widget;  use Gtk.Widget;

with Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Tree_View;
with Gtk.Generic_Fuzzy_Linguistic_Set_Domain.Generic_Zoom_Panel;

generic
   with package Domain is
      new Gtk.Generic_Fuzzy_Linguistic_Set_Domain (<>);
   with package Zoom_Panel is new Domain.Generic_Zoom_Panel;
   with package Tree_View  is new Domain.Generic_Tree_View;
package Gtk.Generic_Fuzzy_Linguistic_Set_Editor is
   package Domain_Of     renames Domain;
   package Zoom_Panel_Of renames Zoom_Panel;
   package Tree_View_Of  renames Tree_View;
   use Domain_Of.Fuzzy_Linguistic_Sets_Of;
   use Tree_View_Of;
--
-- Gtk_Fuzzy_Linguistic_Set_Editor -- Editor of linguistic sets
--
   type Gtk_Fuzzy_Linguistic_Set_Editor_Record is
      new Gtk_Widget_Record with private;
   type Gtk_Fuzzy_Linguistic_Set_Editor is
      access all Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class;
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record;
               Purge  : Boolean := True
            )  return Linguistic_Set;
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box;
--
-- Get_Exec_Buttons -- The box with execute button and its combo box
--
--    Widget - The widget
--
-- Returns :
--
--    The box of the widget with the operation execution tools
--
   function Get_Exec_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box;
--
-- Get_Tree_View -- Get the view widget
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View;
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
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box;
--
-- Gtk_New -- Factory
--
--    Widget   - The result, an editor or viewer (output)
--    Value    - The initial value
--    Layout   - Of the result (vertical or horizontal pane
--    Editable - Editable flag
--
   procedure Gtk_New
             (  Widget   : out Gtk_Fuzzy_Linguistic_Set_Editor;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation := Orientation_Vertical;
                Editable : Boolean         := True
             );
--
-- Initialize -- Construction
--
--    Widget   - The result, an editor or viewer (output)
--    Value    - The initial value
--    Layout   - Of the result (vertical or horizontal pane
--    Editable - Editable flag
--  [ View ]   - The tree view widget to use (a factory of)
--
-- A  derived  type  is  responsible  to call to this procedure from its
-- Initialize.
--
   procedure Initialize
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation;
                Editable : Boolean
             );
   procedure Initialize
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation;
                Editable : Boolean;
                View     : Gtk_Fuzzy_Tree_View_Factory'Class
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
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Boolean;
--
-- Put -- A fuzzy linguistic set into the widget
--
--    Widget - The widget
--    Value  - To be set
--
-- This  procedure  replaces  the  whole  widget content. It removes any
-- selections made and erases the undo and redo buffers.
--
   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Editor_Record;
                Value  : Linguistic_Set
             );
--
-- Set_Editable -- Set editable flag
--
--    Widget   - The factory widget
--    Editable - True when editable
--
   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record;
                Editable : Boolean
             );
private
   use Zoom_Panel_Of;

   type Gtk_Fuzzy_Linguistic_Set_Editor_Record is
      new Gtk_Paned_Record with
   record
      Tree        : Gtk_Fuzzy_Linguistic_Set_Tree_View;
      Table       : Gtk_Table; -- Domain view and sliders
      Tracker_Box : Gtk_Box;   -- Trackers
      Exec_Box    : Gtk_Box;   -- Execution buttons
      Edit_Box    : Gtk_Box;   -- Editing buttons
      View_Box    : Gtk_Box;   -- Viewing buttons
      Tool_Box    : Gtk_Box;   -- Contains Buttons
      Pane_Box    : Gtk_Box;   -- Split part of pane
      Sub_Box     : Gtk_Box;   -- Subbox of Pane_Box or null
   end record;

   type Gtk_Fuzzy_Linguistic_Set_Viewer_Record is
      new Gtk_Fuzzy_Linguistic_Set_Editor_Record with null record;

end Gtk.Generic_Fuzzy_Linguistic_Set_Editor;
