--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Activity_Indicator                      Luebeck            --
--  Interface                                      Autumn, 2008       --
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

with GLib.Main;  use GLib.Main;
with Gtk.Frame;  use Gtk.Frame;

with Gtk.Handlers;

package Gtk.Activity_Indicator is
   pragma Elaborate_Body (Gtk.Activity_Indicator);
--
-- Gtk_Activity_Indicator_Record -- An activity animation widget
--
   type Gtk_Activity_Indicator_Record is
      new Gtk_Frame_Record with private;
   type Gtk_Activity_Indicator is
      access all Gtk_Activity_Indicator_Record'Class;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--
   procedure Gtk_New (Widget : in out Gtk_Activity_Indicator);
--
-- Initialize -- Consruction to be called by a derived type
--
--    Widget - To initialize
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Activity_Indicator_Record'Class
             );
--
-- On -- Indication of ongoing activity
--
--    Widget - The widget
--
   procedure On
             (  Widget : not null access Gtk_Activity_Indicator_Record
             );
--
-- Off -- Indication of stopped activity
--
--    Widget - The widget
--
   procedure Off
             (  Widget : not null access Gtk_Activity_Indicator_Record
             );

private
   package Activity_Timeouts is
      new Generic_Sources (Gtk_Activity_Indicator);
   use Activity_Timeouts;

   type Image_No is mod 8;
   type State is (Active, Stopping, Inactive);

   type Gtk_Activity_Indicator_Record is
      new Gtk_Frame_Record with
   record
      ID : G_Source_ID;
      No : Image_No := 4;
      On : State    := Active;
   end record;

   function Callback (Widget : in Gtk_Activity_Indicator)
      return Boolean;
--
-- On_Destroy -- Destroyed event
--
   procedure On_Destroy
             (  Widget : access Gtk_Activity_Indicator_Record'Class
             );

   package Activity_Handlers is
      new Gtk.Handlers.Callback (Gtk_Activity_Indicator_Record);

end Gtk.Activity_Indicator;
