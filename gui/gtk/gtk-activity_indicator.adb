--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Activity_Indicator                      Luebeck            --
--  Implementation                                 Autumn, 2008       --
--                                                                    --
--                                 Last revision : 12:32 10 Jun 2003  --
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
with Glib.Messages;           use Glib.Messages;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Image;               use Gtk.Image;
with Gtk.Missed;              use Gtk.Missed;
with Fuzzy.Gtk_Icon_Factory;  use Fuzzy.Gtk_Icon_Factory;

with GLib.Object.Checked_Destroy;
with Image_Busy_1_XPM.Image;
with Image_Busy_2_XPM.Image;
with Image_Busy_3_XPM.Image;
with Image_Busy_4_XPM.Image;
with Image_Busy_5_XPM.Image;
with Image_Busy_6_XPM.Image;
with Image_Busy_7_XPM.Image;
with Image_Busy_8_XPM.Image;

package body Gtk.Activity_Indicator is

   Period_In_Milliseconds : constant := 150;

   function Where (Name : String) return String is
   begin
      return " in Gtk.Activity_Indicator." & Name;
   end Where;

   function Callback (Widget : in Gtk_Activity_Indicator)
      return Boolean is
      Image : Gtk_Image;
   begin
      Erase (Widget);
      case Widget.No is
         when 0 => Image := Image_Busy_1_XPM.Image;
         when 1 => Image := Image_Busy_2_XPM.Image;
         when 2 => Image := Image_Busy_3_XPM.Image;
         when 3 => Image := Image_Busy_4_XPM.Image;
         when 4 => Image := Image_Busy_5_XPM.Image;
         when 5 => Image := Image_Busy_6_XPM.Image;
         when 6 => Image := Image_Busy_7_XPM.Image;
         when 7 => Image := Image_Busy_8_XPM.Image;
      end case;
      Add (Widget, Image);
      Show (Image);
      Widget.No := Widget.No + 1;
      if Widget.On = Stopping and then Widget.No = 5 then
         Widget.On := Inactive;
         return False;
      else
         return True;
      end if;
   end Callback;

   procedure On_Destroy
             (  Widget : access Gtk_Activity_Indicator_Record'Class
             )  is
      Result : Boolean;
   begin
      case Widget.On is
         when Active | Stopping =>
            Widget.On := Inactive;
            Result := Remove (Widget.ID);
         when Inactive =>
            null;
      end case;
   end On_Destroy;

   procedure Gtk_New (Widget : in out Gtk_Activity_Indicator) is
   begin
      Widget := new Gtk_Activity_Indicator_Record;
      begin
         Gtk.Activity_Indicator.Initialize (Widget);
      exception
         when Error : others =>
            Log
            (  Fuzzy_ML_Domain,
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
                         Gtk_Activity_Indicator_Record'Class
             )  is
   begin
      Initialize (Widget, "");
      Set_Shadow_Type (Widget, Shadow_Etched_In);
      Activity_Handlers.Connect (Widget, "destroy", On_Destroy'Access);
      Widget.ID :=
         Timeout_Add
         (  Period_In_Milliseconds,
            Callback'Access,
            Widget.all'Unchecked_Access
         );
   end Initialize;

   procedure On
             (  Widget : not null access Gtk_Activity_Indicator_Record
             )  is
   begin
      case Widget.On is
         when Active =>
            null;
         when Stopping | Inactive =>
            Widget.On := Active;
            Widget.ID :=
               Timeout_Add
               (  Period_In_Milliseconds,
                  Callback'Access,
                  Widget.all'Unchecked_Access
               );
      end case;
   end On;

   procedure Off
             (  Widget : not null access Gtk_Activity_Indicator_Record
             )  is
   begin
      case Widget.On is
         when Active =>
            Widget.On := Stopping;
         when Stopping | Inactive =>
            null;
      end case;
   end Off;

end Gtk.Activity_Indicator;
