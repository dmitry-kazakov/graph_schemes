--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.Counter                      Luebeck            --
--  Implementation                                 Spring, 2006       --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;
with Glib.Messages;           use Glib.Messages;

with GNAT.Traceback.Symbolic;
with GLib.Object.Checked_Destroy;
with Indicator.Gtk_IO;

package body Gtk.Indicators.Counter is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Indicators." & Name;
   end Where;

   procedure Gtk_New
             (  Widget           : out Gtk_Counter;
                Label_Text       : UTF8_String   := "";
                Button           : Gtk_Button    := null;
                Counter_Position : Gtk_Rectangle := (0, 1, 0, 1);
                Label_Position   : Gtk_Rectangle := (1, 2, 0, 1);
                Button_Position  : Gtk_Rectangle := (0, 2, 1, 2);
                Size             : Gtk_Size      := (2, 2);
                Spacing          : Gtk_Size      := (3, 3);
                Homogeneous      : Boolean       := False
             )  is
   begin
      Widget := new Gtk_Counter_Record;
      begin
         Initialize
         (  Widget           => Widget,
            Label_Text       => Label_Text,
            Button           => Button,
            Counter_Position => Counter_Position,
            Label_Position   => Label_Position,
            Button_Position  => Button_Position,
            Size             => Size,
            Spacing          => Spacing,
            Homogeneous      => Homogeneous,
            No_Viewer        => False
         );
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               (  "Fault: "
               &  Exception_Information (Error)
               &  Where ("Gtk_New")
            )  );
            Log
            (  Fuzzy_ML_Domain,
               Log_Level_Critical,
               GNAT.Traceback.Symbolic.Symbolic_Traceback (Error)
            );
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Initialize
             (  Widget           : not null access
                                   Gtk_Counter_Record'Class;
                Label_Text       : UTF8_String;
                Button           : Gtk_Button;
                Counter_Position : Gtk_Rectangle;
                Label_Position   : Gtk_Rectangle;
                Button_Position  : Gtk_Rectangle;
                Size             : Gtk_Size;
                Spacing          : Gtk_Size;
                Homogeneous      : Boolean;
                No_Viewer        : Boolean
             )  is
      Viewer : Indicator_Handle;
   begin
      Gtk_New (Widget.Counter_Text);
      if not No_Viewer then
         Viewer :=
            Ref (new Indicator.Gtk_IO.Counter (Widget.Counter_Text));
      end if;
      if Button = null then
         Initialize
         (  Widget          => Widget,
            Button_Position => Button_Position,
            Size            => Size,
            Spacing         => Spacing,
            Homogeneous     => Homogeneous,
            Viewer          => Viewer
         );
      else
         Initialize
         (  Widget          => Widget,
            Button          => Button,
            Button_Position => Button_Position,
            Size            => Size,
            Spacing         => Spacing,
            Homogeneous     => Homogeneous,
            Viewer          => Viewer
         );
      end if;
      Attach
      (  Widget,
         Widget.Counter_Text,
         Counter_Position.Left,
         Counter_Position.Right,
         Counter_Position.Top,
         Counter_Position.Bottom,
         Yoptions => Shrink
      );
      if Label_Text'Length /= 0 then
         Gtk_New (Widget.Label_Text, Label_Text);
         if Label_Position.Left = Counter_Position.Right then
            Widget.Counter_Text.Set_Halign (Align_End);
            Widget.Counter_Text.Set_Valign (Align_Center);
--          Set_Alignment (Widget.Counter_Text, 1.0, 0.5);
            Widget.Label_Text.Set_Halign (Align_Start);
            Widget.Label_Text.Set_Valign (Align_Center);
--          Set_Alignment (Widget.Label_Text, 0.0, 0.5);
         elsif Label_Position.Right = Counter_Position.Left then
            Widget.Counter_Text.Set_Halign (Align_Start);
            Widget.Counter_Text.Set_Valign (Align_Center);
--          Set_Alignment (Widget.Counter_Text, 0.0, 0.5);
            Widget.Label_Text.Set_Halign (Align_End);
            Widget.Label_Text.Set_Valign (Align_Center);
--          Set_Alignment (Widget.Label_Text, 1.0, 0.5);
         end if;
         Attach
         (  Widget,
            Widget.Label_Text,
            Label_Position.Left,
            Label_Position.Right,
            Label_Position.Top,
            Label_Position.Bottom,
            Yoptions => Shrink
         );
         Show (Widget.Label_Text);
      end if;
      Show (Widget.Counter_Text);
   end Initialize;

end Gtk.Indicators.Counter;
