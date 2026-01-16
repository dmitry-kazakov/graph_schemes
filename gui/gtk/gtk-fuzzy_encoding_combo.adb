--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Gtk.Fuzzy_Encoding_Combo                   Luebeck            --
--  Implementation                                 Winter, 2008       --
--                                                                    --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Fuzzy.Gtk_Icon_Factory;    use Fuzzy.Gtk_Icon_Factory;
with GLib.Messages;             use GLib.Messages;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;

with GLib.Object.Checked_Destroy;

package body Gtk.Fuzzy_Encoding_Combo is

   Mode_ASCII   : constant GInt := 0;
   Mode_Latin1  : constant GInt := 1;
   Mode_UTF8    : constant GInt := 2;

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Fuzzy_Encoding_Combo." & Name;
   end Where;

   function Get
            (  Widget : not null access Gtk_Fuzzy_Encoding_Combo_Record
            )  return Code_Set is
   begin
      case Get_Active (Widget) is
         when Mode_ASCII  => return ASCII_Set;
         when Mode_Latin1 => return Latin1_Set;
         when others      => return UTF8_Set;
      end case;
   end Get;

   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Combo_Box_Text.Get_Type,
            Class_Record => Class_Record'Access,
            Type_Name    => Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "encoding-ascii",
               Nick    => "ASCII",
               Blurb   => "The ASCII encoding name",
               Default => "ASCII"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "encoding-latin-1",
               Nick    => "Latin-1",
               Blurb   => "The Latin-1 encoding name",
               Default => "Latin-1 (ISO-8859-1)"
         )  );
         Install_Style_Property
         (  Class_Ref (Class_Record.The_Type),
            Gnew_String
            (  Name    => "encoding-utf-8",
               Nick    => "UTF-8",
               Blurb   => "The UTF-8 encoding name",
               Default => "UTF-8"
         )  );
      end if;
      return Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New
             (  Widget : out Gtk_Fuzzy_Encoding_Combo;
                Mode   : Code_Set
             )  is
   begin
      Widget := new Gtk_Fuzzy_Encoding_Combo_Record;
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
                         Gtk_Fuzzy_Encoding_Combo_Record'Class;
                Mode   : Code_Set
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Combo_Box_Text.Initialize (Widget);
      Append_Text (Widget, "0");
      Append_Text (Widget, "1");
      Append_Text (Widget, "2");
      Set (Widget, Mode);
      Handlers.Connect (Widget, "style-updated", Style_Updated'Access);
      Style_Updated (Widget);
   end Initialize;

   procedure Set
             (  Widget : not null access
                         Gtk_Fuzzy_Encoding_Combo_Record;
                Mode   : Code_Set
             )  is
   begin
      case Mode is
         when ASCII_Set  => Set_Active (Widget, Mode_ASCII);
         when Latin1_Set => Set_Active (Widget, Mode_Latin1);
         when UTF8_Set   => Set_Active (Widget, Mode_UTF8);
      end case;
   end Set;

   procedure Style_Updated
             (  Widget : access Gtk_Fuzzy_Encoding_Combo_Record'Class
             )  is
      Mode : constant Code_Set := Get (Widget);
   begin
      Remove_All (Widget);
      Append_Text
      (  Widget,
         String'(Style_Get (Widget, "encoding-ascii"))
      );
      Append_Text
      (  Widget,
         String'(Style_Get (Widget, "encoding-latin-1"))
      );
      Append_Text
      (  Widget,
         String'(Style_Get (Widget, "encoding-utf-8"))
      );
      Set (Widget, Mode);
   exception
      when Error : others =>
         Log
         (  Fuzzy_ML_Domain,
            Log_Level_Critical,
            (  "Fault: " & Exception_Information (Error)
            &  Where ("Style_Updated")
         )  );
   end Style_Updated;

end Gtk.Fuzzy_Encoding_Combo;
