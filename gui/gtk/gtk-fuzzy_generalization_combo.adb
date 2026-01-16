--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Generalization_Combo             Luebeck            --
--  Implementation                                 Autumn, 2008       --
--                                                                    --
--                                Last revision :  08:56 08 Apr 2022  --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Cell_Layout;           use Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Generalization_Combo is

   Mode_None    : constant GInt := 0;
   Mode_Nearest : constant GInt := 1;
   Mode_Linear  : constant GInt := 2;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Generalization_Combo." & Name;
   end Where;

   function Get
            (  Widget : not null access
                        Gtk_Fuzzy_Generalization_Combo_Record
            )  return Generalization_Mode is
   begin
      case Get_Active (Widget) is
         when Mode_None    => return None;
         when Mode_Nearest => return Nearest;
         when others       => return Linear;
      end case;
   end Get;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Combo_Box.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "generalization-linear",
               Nick    => "Linear",
               Blurb   => "The generalization linear name",
               Default => "Linear"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "generalization-nearest",
               Nick    => "Nearest",
               Blurb   => "The generalization nearest name",
               Default => "Nearest"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "generalization-none",
               Nick    => "None",
               Blurb   => "The generalization none name",
               Default => "None"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Generalization_Combo;
                Mode   : Generalization_Mode
             )  is
   begin
      Widget := new Gtk_Fuzzy_Generalization_Combo_Record;
      begin
         Initialize (Widget, Mode);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Fuzzy_Generalization_Combo_Record'Class;
                Mode   : Generalization_Mode
             )  is
      Text : Gtk_Cell_Renderer_Text;
      Row  : Gtk_Tree_Iter;
   begin
      Gtk_New (Widget.List, (GType_String, GType_Int));
      Widget.List.Append (Row);
      Widget.List.Set (Row, 1, Mode_None);
      Widget.List.Append (Row);
      Widget.List.Set (Row, 1, Mode_Nearest);
      Widget.List.Append (Row);
      Widget.List.Set (Row, 1, Mode_Linear);

      G_New (Widget, Get_Type);
      Initialize_With_Model (Widget, To_Interface (Widget.List));
--    Widget.List.Unref;

      Gtk_New (Text);
      Pack_Start (+Widget, Text, False);
      Add_Attribute (+Widget, Text, "text", 0);
      Widget.Set (Mode);

      Handlers.Connect (Widget, "style-updated", Style_Updated'Access);
      Style_Updated (Widget);
   end Initialize;

   procedure Set
             (  Widget : not null access
                         Gtk_Fuzzy_Generalization_Combo_Record;
                Mode   : Generalization_Mode
             )  is
   begin
      case Mode is
         when None    => Widget.Set_Active (Mode_None);
         when Nearest => Widget.Set_Active (Mode_Nearest);
         when Linear  => Widget.Set_Active (Mode_Linear);
      end case;
   end Set;

   procedure Style_Updated
             (  Widget : access
                         Gtk_Fuzzy_Generalization_Combo_Record'Class
             )  is
   begin
      Widget.List.Set
      (  Widget.List.Nth_Child (Null_Iter, Mode_None),
         0,
         String'(Style_Get (Widget, "generalization-none"))
      );
      Widget.List.Set
      (  Widget.List.Nth_Child (Null_Iter, Mode_Nearest),
         0,
         String'(Style_Get (Widget, "generalization-nearest"))
      );
      Widget.List.Set
      (  Widget.List.Nth_Child (Null_Iter, Mode_Linear),
         0,
         String'(Style_Get (Widget, "generalization-linear"))
      );
   end Style_Updated;

end Gtk.Fuzzy_Generalization_Combo;
