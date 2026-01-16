--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Feature.Selection                 Luebeck            --
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
--  The package provides a tree view widget for choosing a feature type.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Gtk.Fuzzy_Feature;     use Gtk.Fuzzy_Feature;
with Gtk.Fuzzy_Object;      use Gtk.Fuzzy_Object;
with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_View;         use Gtk.Tree_View;
with Gtk.Tree_Selection;    use Gtk.Tree_Selection;
with Gtk.Tree_Store;        use Gtk.Tree_Store;

with Gtk.Handlers;

package Gtk.Fuzzy_Feature.Selection is

   Class_Name : constant String := Prefix & "FeatureSelection";
--
-- Gtk_Feature_Selection_Record -- Feature selection widget
--
   type Gtk_Feature_Selection_Record is
      new Gtk_Tree_View_Record with private;
   type Gtk_Feature_Selection is
      access all Gtk_Feature_Selection_Record'Class;
--
-- Get_Constraint -- The constraint
--
--    Widget - The widget
--
-- Returns :
--
--    The object picking constraint
--
   function Get_Constraint
            (  Widget : not null access Gtk_Feature_Selection_Record
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
--    Editable          - The initial state of
--
-- When  Feature  is valid it is used to initialize the selection of the
-- widget and the corresponding factory widget.
--
   procedure Gtk_New
             (  Widget            : out Gtk_Feature_Selection;
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
--    Editable          - The initial state of
--
-- Any  derived  type  is responsible to call to Initialize from its own
-- Initialize.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Feature_Selection_Record'Class;
                Feature_Picker    : Gtk_Object_Picker;
                Classifier_Picker : Gtk_Object_Picker;
                Feature           : Feature_Handle;
                Editable          : Boolean
             );
--
-- Is_Editable -- Check if selection is editable
--
--    Widget - The widget
--
-- Returns :
--
--    True if selection is editable
--
   function Is_Editable
            (  Widget : not null access Gtk_Feature_Selection_Record
            )  return Boolean;
--
-- Set_Editable -- Changes the flag
--
--    Widget   - The widget
--    Editable - The new flag of
--
   procedure Set_Editable
             (  Widget   : not null access Gtk_Feature_Selection_Record;
                Editable : Boolean
             );
--
-- On_Category_Change -- Feature category change
--
--    Widget - The widget
--
-- This procedure is called on navigation in the  selection  tree  which
-- does not lead to selection change.
--
   procedure On_Category_Change
             (  Widget : not null access Gtk_Feature_Selection_Record
             )  is null;
--
-- On_Selection_Change -- Feature type change
--
--    Widget     - The widget
--    Selected   - The feature factory which has been selected
--    Deselected - The feature factory which has been deselected
--
-- This procedure is called on selection change. The parameters Selected
-- and  Deselected  indicate the change made. Normally an implementation
-- replaces  Deselected  in  a container widget with Selected. Note that
-- Deselected is not destroyed, the widget holds a reference to it.  The
-- default implementation does nothing.
--
   procedure On_Selection_Change
             (  Widget     : not null access
                             Gtk_Feature_Selection_Record;
                Selected   : Gtk_Fuzzy_Feature_Abstract_Factory;
                Deselected : Gtk_Fuzzy_Feature_Abstract_Factory
             )  is null;
private
   type Gtk_Feature_Selection_Record is
      new Gtk_Tree_View_Record with
   record
      List     : Gtk_Tree_Store;
      Feature  : Feature_Handle;
      Selected : Gtk_Fuzzy_Feature_Abstract_Factory;
      Constraint        : Picker_Constraint;
      Row               : Gtk_Tree_Iter;
      Feature_Picker    : Gtk_Object_Picker;
      Classifier_Picker : Gtk_Object_Picker;
      Handler           : Boolean := False;
      Editable          : Boolean := False;
   end record;
--
-- On_Destroy -- Event handler
--
   procedure On_Destroy
             (  Widget : access Gtk_Feature_Selection_Record'Class
             );
--
-- On_Selection -- Event handler
--
   procedure On_Selection
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                Widget    : Gtk_Feature_Selection
             );
--
-- Style_Updated -- Style-update handler
--
   procedure Style_Updated
             (  Widget : access Gtk_Feature_Selection_Record'Class
             );

   package Feature_Selection_Handlers is
      new Gtk.Handlers.Callback (Gtk_Feature_Selection_Record);

   package Feature_Type_Selection_Handlers is
      new Gtk.Handlers.User_Callback
          (  Gtk_Tree_Selection_Record,
             Gtk_Feature_Selection
          );
end Gtk.Fuzzy_Feature.Selection;
