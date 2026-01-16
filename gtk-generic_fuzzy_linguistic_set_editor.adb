--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_Set_Editor     Luebeck            --
--  Implementation                                 Spring, 2007       --
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

with Ada.Exceptions;  use Ada.Exceptions;
with Glib.Messages;   use Glib.Messages;
with Gtk.Alignment;   use Gtk.Alignment;
with Gtk.Frame;       use Gtk.Frame;
with Gtk.Missed;      use Gtk.Missed;
with Gtk.Separator;   use Gtk.Separator;

with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;

package body Gtk.Generic_Fuzzy_Linguistic_Set_Editor is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Generic_Fuzzy_Linguistic_Set_Editor." & Name;
   end Where;

   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Boolean is
   begin
      return Edited (Widget.Tree);
   end Edited;

   procedure Editable_Off
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class
             )  is
      Child : Gtk_Widget;
   begin
      -- Moving bars
      Widget.Table.Remove (Widget.Tree.Get_X_Move_Bar);
      Widget.Table.Remove (Widget.Tree.Get_Y_Move_Bar);
      -- Zooming bars
      Child :=
         Get_Zoom_Y_Scale (Widget.Tree.Get_Domain_View).all'Access;
      Child.Ref;
      Widget.Table.Remove (Child);
      Widget.Table.Attach (Child, 0, 1, 0, 2, XOptions => 0);
      Unref (Child);

      Child :=
         Get_Zoom_X_Scale (Widget.Tree.Get_Domain_View).all'Access;
      Child.Ref;
      Widget.Table.Remove (Child);
      Widget.Table.Attach (Child, 1, 3, 2, 3, YOptions => 0);
      Child.Unref;

      Erase (Widget.Edit_Box);
      Erase (Widget.Exec_Box);
   end Editable_Off;

   procedure Editable_On
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class
             )  is
      Child : Gtk_Widget;
   begin
      -- Zooming bars
      Child :=
         Get_Zoom_Y_Scale (Widget.Tree.Get_Domain_View).all'Access;
      Child.Ref;
      Widget.Table.Remove (Child);
      Widget.Table.Attach (Child, 0, 1, 1, 2, XOptions => 0);
      Child.Unref;

      Child :=
         Get_Zoom_X_Scale (Widget.Tree.Get_Domain_View).all'Access;
      Child.Ref;
      Widget.Table.Remove (Child);
      Widget.Table.Attach (Child, 1, 2, 2, 3, YOptions => 0);
      Child.Unref;
      -- Moving bars
      Widget.Table.Attach
      (  Widget.Tree.Get_Y_Move_Bar,
         0, 1, 0, 1,
         XOptions => 0
      );
      Widget.Table.Attach
      (  Widget.Tree.Get_X_Move_Bar,
         2, 3, 2, 3,
         YOptions => 0
      );
      -- Edit box buttons
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_New_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Copy_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Add_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Purge_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Remove_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Up_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Down_Button,
         False,
         False
      );
      declare
         Line : Gtk_Separator;
      begin
         if Widget.Sub_Box = null then
            Gtk_New_VSeparator (Line);
         else
            Gtk_New_HSeparator (Line);
         end if;
         Widget.Edit_Box.Pack_Start (Line, False, False);
      end;
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Undo_Button,
         False,
         False
      );
      Widget.Edit_Box.Pack_Start
      (  Widget.Tree.Get_Redo_Button,
         False,
         False
      );
      -- Exec box
      Widget.Exec_Box.Pack_Start
      (  Widget.Tree.Get_Exec_Button.Get_Combo,
         False,
         False
      );
      Widget.Exec_Box.Pack_Start
      (  Widget.Tree.Get_Exec_Button,
         False,
         False
      );
   end Editable_On;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record;
               Purge  : Boolean := True
            )  return Linguistic_Set is
   begin
      return Get (Widget.Tree, Purge);
   end Get;

   function Get_Edit_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.Edit_Box;
   end Get_Edit_Buttons;

   function Get_Exec_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.Exec_Box;
   end Get_Exec_Buttons;

   function Get_Pane
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Paned is
   begin
      return Widget.all'Access;
   end Get_Pane;

   function Get_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.Tracker_Box;
   end Get_Tracker;

   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Tree_View is
   begin
      return Widget.Tree;
   end Get_Tree_View;

   function Get_View_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Gtk_Box is
   begin
      return Widget.View_Box;
   end Get_View_Buttons;

   procedure Gtk_New
             (  Widget   : out Gtk_Fuzzy_Linguistic_Set_Editor;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation := Orientation_Vertical;
                Editable : Boolean         := True
             )  is
   begin
      Widget := new Gtk_Fuzzy_Linguistic_Set_Editor_Record;
      begin
         Initialize (Widget, Value, Layout, Editable);
      exception
         when Error : others =>
            Log
            (  Gtk.Fuzzy_Boolean_Drawing.Fuzzy_Domain,
               Log_Level_Critical,
               (  "Fault: " & Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation;
                Editable : Boolean
             )  is
      Factory : Gtk_Fuzzy_Tree_View_Factory;
   begin
      Initialize (Widget, Value, Layout, Editable, Factory);
   end Initialize;

   procedure Initialize
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record'Class;
                Value    : Linguistic_Set;
                Layout   : Gtk_Orientation;
                Editable : Boolean;
                View     : Gtk_Fuzzy_Tree_View_Factory'Class
             )  is
      Frame : Gtk_Frame;
   begin
      Widget.Tree := Create (View, Value);
      Set_Editable (Widget.Tree, Editable);
      -- View box
      Gtk_New_HBox (Widget.View_Box, False, 2);
      Widget.View_Box.Pack_Start
      (  Get_Zoom_In_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      Widget.View_Box.Pack_Start
      (  Get_Zoom_100_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      Widget.View_Box.Pack_Start
      (  Get_Zoom_Out_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      Widget.View_Box.Pack_Start
      (  Get_Zoom_Fit_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      declare
         Line : Gtk_Separator;
      begin
         Gtk_New_VSeparator (Line);
         Widget.View_Box.Pack_Start (Line, False, False);
      end;
      Widget.View_Box.Pack_Start
      (  Get_Zoom_Undo_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      Widget.View_Box.Pack_Start
      (  Get_Zoom_Redo_Button (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      declare
         Line : Gtk_Separator;
      begin
         Gtk_New_VSeparator (Line);
         Widget.View_Box.Pack_Start (Line, False, False);
      end;
      Widget.View_Box.Pack_Start
      (  Widget.Tree.Get_Find_Button,
         False,
         False
      );
      Pack_Start
      (  Widget.View_Box,
         Get_Accumulate_Button (Widget.Tree),
         False,
         False
      );
      -- Tracker box
      Gtk_New_Hbox (Widget.Tracker_Box, False, 4);
      Widget.Tracker_Box.Pack_Start
      (  Get_X_Tracker (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      declare
         Line : Gtk_Separator;
      begin
         Gtk_New_VSeparator (Line);
         Widget.Tracker_Box.Pack_Start (Line, False, False);
      end;
      Widget.Tracker_Box.Pack_Start
      (  Get_Y_Tracker (Widget.Tree.Get_Domain_View),
         False,
         False
      );
      -- Table
      Gtk_New (Widget.Table, 3, 3, False);
      Widget.Table.Attach
      (  Get_Zoom_Y_Scale (Widget.Tree.Get_Domain_View),
         0, 1, 1, 2,
         XOptions => 0
      );
      Widget.Table.Attach
      (  Get_Zoom_X_Scale (Widget.Tree.Get_Domain_View),
         1, 3, 2, 3,
         YOptions => 0
      );
      Widget.Table.Attach (Widget.Tree.Get_Domain_View, 1, 3, 0, 2);
      -- Exec box
      Gtk_New_HBox (Widget.Exec_Box, False, 2);
      Gtk_New_HBox (Widget.Pane_Box, False, 3);
      case Layout is
         when Orientation_Vertical =>
            Initialize_VPaned (Widget);
            Widget.Add1 (Widget.Pane_Box);
            Widget.Add2 (Widget.Table);
            -- Box of button boxes
            Gtk_New_VBox (Widget.Tool_Box, False, 3);
            Gtk_New_HBox (Widget.Edit_Box, False, 3);
            Widget.Tool_Box.Pack_Start (Widget.Edit_Box, False, False);
            -- Pane box
            Gtk_New (Frame);
            Frame.Set_Shadow_Type (Shadow_In);
            Frame.Add (Widget.Tree);
            Widget.Pane_Box.Pack_Start (Frame);
            Widget.Pane_Box.Pack_Start (Widget.Tool_Box, False, False);
         when Orientation_Horizontal =>
            --  .______.._________________________.
            --  | Tree || Pane_Box                |
            --  |      ||.__________.____________.|
            --  |      ||| Edit_Box |   Sub_Box  ||
            --  |      |||          |.__________.||
            --  |      |||          ||   Table  |||
            --  |      |||          ||__________|||
            --  |      |||          || Tool_Box |||
            --  |      |||          ||__________|||
            --  |      |||__________|____________||
            --  |______||_________________________|
            --
            Initialize_HPaned (Widget);
            Gtk_New (Frame);
            Frame.Set_Shadow_Type (Shadow_In);
            Widget.Pack1 (Frame, False, False);
            Frame.Add (Widget.Tree);
            Widget.Pack2 (Widget.Pane_Box, True, True);
            -- Box of button boxes
            Gtk_New_HBox (Widget.Tool_Box, False, 3);
            -- Pane box
            Gtk_New_VBox (Widget.Edit_Box, False, 3);
            Gtk_New_VBox (Widget.Sub_Box,  False, 3);
            Widget.Pane_Box.Pack_Start (Widget.Edit_Box, False, False);
            Widget.Pane_Box.Pack_Start (Widget.Sub_Box);
            -- Subbox
            Widget.Sub_Box.Pack_Start (Widget.Table);
            Widget.Sub_Box.Pack_Start (Widget.Tool_Box, False, False);
      end case;
      Widget.Tool_Box.Pack_Start (Widget.View_Box,  False, False);
      Widget.Tool_Box.Pack_Start (Widget.Exec_Box,  False, False);
      Widget.Tool_Box.Pack_End (Widget.Tracker_Box, False, False);

      if Editable then
         Widget.Editable_On;
      end if;
      Widget.Tree.Set_Size_Request (220, 100);
   end Initialize;

   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Editor_Record
            )  return Boolean is
   begin
      return Widget.Tree.Is_Editable;
   end Is_Editable;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Editor_Record;
                Value  : Linguistic_Set
             )  is
   begin
      Widget.Tree.Put (Value);
   end Put;

   procedure Set_Editable
             (  Widget   : not null access
                           Gtk_Fuzzy_Linguistic_Set_Editor_Record;
                Editable : Boolean
             )  is
   begin
      if Widget.Is_Editable xor Editable then
         if Editable then
            Widget.Tree.Set_Editable (True);
            Widget.Editable_On;
         else
            Widget.Tree.Set_Editable (False);
            Widget.Editable_Off;
         end if;
      end if;
   end Set_Editable;

end Gtk.Generic_Fuzzy_Linguistic_Set_Editor;
