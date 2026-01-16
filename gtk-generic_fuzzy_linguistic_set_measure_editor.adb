--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Generic_Fuzzy_Linguistic_               Luebeck            --
--        Set_Measure_Editor                       Spring, 2007       --
--  Implementation                                                    --
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
with Strings_Edit;    use Strings_Edit;
with Units.Base;      use Units.Base;

with Ada.IO_Exceptions;
with Gtk.Fuzzy_Boolean_Drawing;
with GLib.Object.Checked_Destroy;

package body Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Editor is

   function Where (Name : String) return String is
   begin
      return
      (  " in Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Editor."
      &  Name
      );
   end Where;

   function Edited
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Boolean is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Edited;
   end Edited;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
               Purge  : Boolean := True
            )  return Set_Measure is
   begin
      return Widget.Get_Tree_View.Get (Purge);
   end Get;

   procedure Get
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : out Linguistic_Set;
                Scale  : out Measure_Tree_View.Fuzzy_Measures_Of.
                             Interval_Measures.Float_Measures.Measure;
                Purge  : Boolean := True
             )  is
   begin
      Widget.Get_Tree_View.Get
      (  Value  => Value,
         Scale  => Scale,
         Purge  => Purge
      );
   end Get;

   function Get_Edit_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Get_Edit_Buttons;
   end Get_Edit_Buttons;

   function Get_Pane
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Paned is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Get_Pane;
   end Get_Pane;

   function Get_Tracker
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Get_Tracker;
   end Get_Tracker;

   function Get_Tree_View
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return
         Gtk_Fuzzy_Linguistic_Set_Measure_Tree_View_Record'Class
         (  Parent.Get_Tree_View.all
         ) 'Access;
   end Get_Tree_View;

   function Get_View_Buttons
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Gtk_Box is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Get_View_Buttons;
   end Get_View_Buttons;

   function Get_Unit
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Unit is
   begin
      return Widget.Get_Tree_View.Get_Unit;
   end Get_Unit;

   procedure Gtk_New
             (  Widget   : out Gtk_Fuzzy_Linguistic_Set_Measure_Editor;
                Value    : Set_Measure;
                Scale    : UTF8_String     := "";
                Layout   : Gtk_Orientation := Orientation_Vertical;
                Editable : Boolean         := True
             )  is
   begin
      Widget := new Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
      begin
         Initialize (Widget, Value, Scale, Layout, Editable);
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
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record'Class;
                Value    : Set_Measure;
                Scale    : UTF8_String;
                Layout   : Gtk_Orientation;
                Editable : Boolean
             )  is
      Factory : Gtk_Fuzzy_Measure_Tree_View_Factory (Scale'Length);
   begin
      Factory.Scale  := Scale;
      Factory.SI     := Value.SI;
      Factory.Offset := Value.Offset;
      Initialize
      (  Widget,
         Value,
         Scale,
         Layout,
         Editable,
         Factory
      );
   end Initialize;

   procedure Initialize
             (  Widget : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record'Class;
                Value    : Set_Measure;
                Scale    : UTF8_String;
                Layout   : Gtk_Orientation;
                Editable : Boolean;
                View     : Gtk_Fuzzy_Measure_Tree_View_Factory'Class
             )  is
   begin
      Initialize (Widget, Value.Gain, Layout, Editable, View);
   end Initialize;

   function Is_Editable
            (  Widget : not null access
                        Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record
            )  return Boolean is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      return Parent.Is_Editable;
   end Is_Editable;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : Set_Measure
             )  is
   begin
      Widget.Get_Tree_View.Put (Value);
   end Put;

   procedure Put
             (  Widget : not null access
                         Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Value  : Set_Measure;
                Scale  : UTF8_String
             )  is
   begin
      Widget.Get_Tree_View.Put (Value, Scale);
   end Put;

   procedure Set_Editable
             (  Widget   : not null access
                   Gtk_Fuzzy_Linguistic_Set_Measure_Editor_Record;
                Editable : Boolean
             )  is
      Parent : Gtk_Fuzzy_Linguistic_Set_Editor_Record renames
               Gtk_Fuzzy_Linguistic_Set_Editor_Record (Widget.all);
   begin
      Parent.Set_Editable (Editable);
   end Set_Editable;

end Gtk.Generic_Fuzzy_Linguistic_Set_Measure_Editor;
