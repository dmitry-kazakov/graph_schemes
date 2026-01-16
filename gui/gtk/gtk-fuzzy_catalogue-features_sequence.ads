--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Fuzzy_Catalogue.                        Luebeck            --
--        Features_Sequence                        Winter, 2009       --
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

with Fuzzy.Feature;             use Fuzzy.Feature;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Fuzzy_Features_List;   use Gtk.Fuzzy_Features_List;
with Gtk.Fuzzy_Object;          use Gtk.Fuzzy_Object;
with Gtk.List_Store;            use Gtk.List_Store;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Widget;                use Gtk.Widget;
with Object.Archived;           use Object.Archived;
with Object.Archived.Features;  use Object.Archived.Features;

with Backward_Link_Handles.Sets;
with Fuzzy.Feature.Handle.Bounded_Arrays;
with Gtk.Handlers;

package Gtk.Fuzzy_Catalogue.Features_Sequence is
   pragma Elaborate_Body (Gtk.Fuzzy_Catalogue.Features_Sequence);
   use Fuzzy.Feature.Handle.Bounded_Arrays;
--
-- Gtk_Fuzzy_Features_Sequence -- Editing features list
--
   type Gtk_Fuzzy_Features_Sequence_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_VBox_Record with private;
   type Gtk_Fuzzy_Features_Sequence is
      access all Gtk_Fuzzy_Features_Sequence_Record'Class;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType;
--
-- Gtk_New -- Construction
--
--    Widget  - The result
--    Browser - The the parent catalogue widget
--
   procedure Gtk_New
             (  Widget  : out Gtk_Fuzzy_Features_Sequence;
                Browser : not null access
                          Gtk_Fuzzy_Catalogue_Record'Class
             );
--
-- Get_Button_Box
--
--    Widget - The widget
--
-- Returns :
--
--    The button box
--
   function Get_Button_Box
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Gtk_VBox;
--
-- Get_Constraint -- The constraint
--
--    Widget - The widget
--
-- Returns :
--
--    The constraint of the sequence
--
   function Get_Constraint
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Picker_Constraint;
--
-- Get_Selected
--
--    Widget - The widget
--
-- Returns :
--
--    The list of currently selected features
--
   function Get_Selected
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Bounded_Array;
--
-- Get_Selected_Tree_View
--
--    Widget - The widget
--
-- Returns :
--
--    The tree view of the selected features
--
   function Get_Selected_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Features_Sequence_Record
            )  return Gtk_Tree_View;
--
-- Initialize -- To be called by any derived type
--
--    Widget - The widget to initialize
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Features_Sequence_Record
             );

private
   use Fuzzy.Feature.Handle;

   type Feature_Observer
        (  Sequence : not null access
                      Gtk_Fuzzy_Features_Sequence_Record'Class
        )  is new Feature_Link with
   record
      Constraint : Picker_Constraint;
   end record;
   type Feature_Observer_Ptr is access Feature_Observer'Class;
--
-- Deleted -- Overrides Object.Archived...
--
   overriding
   procedure Deleted
             (  Link  : in out Feature_Observer;
                Temps : in out Deposit_Container'Class
             );
--
-- Destroyed -- Overrides Object.Archived...
--
   overriding
   procedure Destroyed (Link : in out Feature_Observer);
--
-- Renamed -- Overrides Object.Archived.Features...
--
   overriding
   procedure Renamed
             (  Link     : in out Feature_Observer;
                Old_Name : String;
                New_Name : String
             );
--
-- Gtk_Fuzzy_Features_Sequence -- Editing features list
--
   type Gtk_Fuzzy_Features_Sequence_Record
        (  Browser : not null access Gtk_Fuzzy_Catalogue_Record'Class
        )  is new Gtk_VBox_Record with
   record
      Buttons_Box   : Gtk_VBox;
      Up_Button     : Up_Buttons.Gtk_Style_Button;
      Down_Button   : Down_Buttons.Gtk_Style_Button;
      Left_Button   : Left_Buttons.Gtk_Style_Button;
      Right_Button  : Right_Buttons.Gtk_Style_Button;
      Selected      : Gtk_Tree_View;
      Selected_List : Gtk_List_Store;
      Observers     : Backward_Link_Handles.Sets.Set;
      Constraint    : Picker_Constraint;
   end record;
--
-- Features_Selection_Changed -- Selection changed event
--
   procedure Features_Selection_Changed
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );
--
-- Destroy -- Event handler
--
   procedure Destroy
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );
--
-- Down -- Button click event
--
   procedure Down
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );
--
-- Left -- Button click event
--
   procedure Left
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );
--
-- Right -- Button click event
--
   procedure Right
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );
--
-- Selected_Changed -- Selection changed event
--
   procedure Selected_Changed
             (  Selection : access Gtk_Tree_Selection_Record'Class;
                List      : Gtk_Fuzzy_Features_Sequence
             );
--
-- Up -- Button click event
--
   procedure Up
             (  Widget : access Gtk_Widget_Record'Class;
                List   : Gtk_Fuzzy_Features_Sequence
             );

   package Widget_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Widget_Record,
              Gtk_Fuzzy_Features_Sequence
           );

   package Selection_Handlers is new Gtk.Handlers.User_Callback
           (  Gtk_Tree_Selection_Record,
              Gtk_Fuzzy_Features_Sequence
           );

   type Deleted_Data
        (  Observer : not null access Feature_Observer'Class
        )  is new Request_Data with null record;
   procedure Service (Data : in out Deleted_Data);

   type Renamed_Data
        (  Observer : not null access Feature_Observer'Class;
           Length   : Natural
        )  is new Request_Data with
   record
      Name : String (1..Length);
   end record;
   procedure Service (Data : in out Renamed_Data);

end Gtk.Fuzzy_Catalogue.Features_Sequence;
