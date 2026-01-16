--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.Counter                      Luebeck            --
--  Interface                                      Spring, 2006       --
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
--  This  package provides counter progress indicator based on GTK+ I/O.
--  The indicator's visual appearance might look as follows:
--       .___________.___________.
--       |           |           |
--       |  Counter  |   Label   |
--       |___________|___________|
--       |                       |
--       |       [ Button ]      |
--       |_______________________|
--
--  The indicator is a GTK+ widget derived from Gtk_Table_Record.
--
with Glib;       use Glib;
with Gtk.Label;  use Gtk.Label;

package Gtk.Indicators.Counter is
   pragma Elaborate_Body (Gtk.Indicators.Counter);
--
-- Gtk_Counter_Record -- The widget type
--
   type Gtk_Counter_Record is new Gtk_Indicator_Record with private;
   type Gtk_Counter is access all Gtk_Counter_Record'Class;
--
-- Gtk_New -- Factory
--
--    Widget           - The result
--    Label_Text       - The text to use as the label
--  [ Button ]         - The cancel button
--    Counter_Position - Where to place the counter
--    Label_Position   - Where to place the label
--    Button_Position  - Where to place the button
--    Size             - Of the table
--    Spacing          - The default spacing to set
--    Homogeneous      - Table type
--
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
             );
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget           - The result
--    Label_Text       - The text to use as the label
--    Button           - The cancel button (can be null)
--    Counter_Position - Where to place the counter
--    Label_Position   - Where to place the label
--    Button_Position  - Where to place the button
--    Size             - Of the table
--    Spacing          - The default spacing to set
--    No_Viewer        - Postpone viewer creation
--
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
             );

private
   type Gtk_Counter_Record is new Gtk_Indicator_Record with record
      Label_Text   : Gtk_Label;
      Counter_Text : Gtk_Label;
   end record;

end Gtk.Indicators.Counter;
