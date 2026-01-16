--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.Progress                     Luebeck            --
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

with GLib.Object.Checked_Destroy;

package body Gtk.Indicators.Progress is

   function Where (Name : String) return String is
   begin
      return " in Gtk.Indicators.Progress." & Name;
   end Where;

   procedure Gtk_New
             (  Widget           : out Gtk_Progress;
                Button           : access Gtk_Button_Record'Class;
                Button_Position  : Gtk_Rectangle := (0, 1, 1, 2);
                Bar_Position     : Gtk_Rectangle := (0, 1, 0, 1);
                Timed            : Boolean       := True;
                Size             : Gtk_Size      := (1, 2);
                Spacing          : Gtk_Size      := (3, 3);
                Homogeneous      : Boolean       := False
             )  is
   begin
      Widget := new Gtk_Progress_Record;
      begin
         Initialize
         (  Widget          => Widget,
            Button          => Button,
            Button_Position => Button_Position,
            Bar_Position    => Bar_Position,
            Timed           => Timed,
            Size            => Size,
            Spacing         => Spacing,
            Homogeneous     => Homogeneous,
            No_Viewer       => False
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
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   procedure Gtk_New
             (  Widget           : out Gtk_Progress;
                Button_Position  : Gtk_Rectangle := (0, 1, 1, 2);
                Bar_Position     : Gtk_Rectangle := (0, 1, 0, 1);
                Timed            : Boolean       := True;
                Size             : Gtk_Size      := (1, 2);
                Spacing          : Gtk_Size      := (3, 3);
                Homogeneous      : Boolean       := False
             )  is
   begin
      Widget := new Gtk_Progress_Record;
      begin
         Initialize
         (  Widget          => Widget,
            Button_Position => Button_Position,
            Bar_Position    => Bar_Position,
            Timed           => Timed,
            Size            => Size,
            Spacing         => Spacing,
            Homogeneous     => Homogeneous,
            No_Viewer       => False
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
            GLib.Object.Checked_Destroy (Widget);
            Widget := null;
            raise;
      end;
   end Gtk_New;

   function Get_Bar (Widget : access Gtk_Progress_Record)
      return Gtk_Timed_Progress_Bar is
   begin
      return Widget.Status;
   end Get_Bar;

   procedure Initialize
             (  Widget          : access Gtk_Progress_Record'Class;
                Button          : access Gtk_Button_Record'Class;
                Button_Position : Gtk_Rectangle;
                Bar_Position    : Gtk_Rectangle;
                Timed           : Boolean;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean;
                No_Viewer       : Boolean
             )  is
      Viewer : Indicator_Handle;
   begin
      Gtk_New (Widget.Status);
      if not No_Viewer then
         if Timed then
            Viewer :=
               Ref (new Indicator.Gtk_IO.Timed_Bar (Widget.Status));
         else
            Viewer :=
               Ref (new Indicator.Gtk_IO.Bar (Widget.Status));
         end if;
      end if;
      Initialize
      (  Widget          => Widget,
         Button          => Button,
         Button_Position => Button_Position,
         Size            => Size,
         Spacing         => Spacing,
         Homogeneous     => Homogeneous,
         Viewer          => Viewer
      );
      Attach
      (  Widget,
         Widget.Status,
         Bar_Position.Left,
         Bar_Position.Right,
         Bar_Position.Top,
         Bar_Position.Bottom,
         Yoptions => Shrink
      );
      Show (Widget.Status);
   end Initialize;

   procedure Initialize
             (  Widget          : access Gtk_Progress_Record'Class;
                Button_Position : Gtk_Rectangle;
                Bar_Position    : Gtk_Rectangle;
                Timed           : Boolean;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean;
                No_Viewer       : Boolean
             )  is
      Viewer : Indicator_Handle;
   begin
      Gtk_New (Widget.Status);
      if not No_Viewer then
         if Timed then
            Viewer :=
               Ref (new Indicator.Gtk_IO.Timed_Bar (Widget.Status));
         else
            Viewer :=
               Ref (new Indicator.Gtk_IO.Bar (Widget.Status));
         end if;
      end if;
      Initialize
      (  Widget          => Widget,
         Button_Position => Button_Position,
         Size            => Size,
         Spacing         => Spacing,
         Homogeneous     => Homogeneous,
         Viewer          => Viewer
      );
      Attach
      (  Widget,
         Widget.Status,
         Bar_Position.Left,
         Bar_Position.Right,
         Bar_Position.Top,
         Bar_Position.Bottom,
         Yoptions => Shrink
      );
      Show (Widget.Status);
   end Initialize;

end Gtk.Indicators.Progress;
