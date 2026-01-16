--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Gtk.Indicators.Progress                     Luebeck            --
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
--  This  package  provides  progress  indicator  based on GTK+ I/O. The
--  indicator's visual appearance might look as follows:
--       ._______________________.
--       |                       |
--       |  ||||||||          |  |
--       |_______________________|
--       |                       |
--       |       [ Button ]      |
--       |_______________________|
--
--  The indicator is a GTK+ widget derived from Gtk_Table_Record.
--
with Glib;              use Glib;
with Indicator.Gtk_IO;  use Indicator.Gtk_IO;

package Gtk.Indicators.Progress is
   pragma Elaborate_Body (Gtk.Indicators.Progress);
--
-- Gtk_Progress_Record -- The widget type
--
   type Gtk_Progress_Record is new Gtk_Indicator_Record with private;
   type Gtk_Progress is access all Gtk_Progress_Record'Class;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType renames Gtk.Indicators.Get_Type;
--
-- Gtk_New -- Factory
--
--    Widget          - The result
--  [ Button [        - The button
--    Button_Position - Where to place the button
--    Bar_Position    - Where to place the indicator
--    Timed           - If time estimation should be shown
--    Size            - Of the table
--    Spacing         - The default spacing to set
--    Homogeneous     - Table type
--
   procedure Gtk_New
             (  Widget           : out Gtk_Progress;
                Button           : access Gtk_Button_Record'Class;
                Button_Position  : Gtk_Rectangle := (0, 1, 1, 2);
                Bar_Position     : Gtk_Rectangle := (0, 1, 0, 1);
                Timed            : Boolean       := True;
                Size             : Gtk_Size      := (1, 2);
                Spacing          : Gtk_Size      := (3, 3);
                Homogeneous      : Boolean       := False
             );
   procedure Gtk_New
             (  Widget           : out Gtk_Progress;
                Button_Position  : Gtk_Rectangle := (0, 1, 1, 2);
                Bar_Position     : Gtk_Rectangle := (0, 1, 0, 1);
                Timed            : Boolean       := True;
                Size             : Gtk_Size      := (1, 2);
                Spacing          : Gtk_Size      := (3, 3);
                Homogeneous      : Boolean       := False
             );
--
-- Get_Bar -- Get the progress bar of the indicator
--
--    Widget - The progress indicator widget (a pointer to)
--
-- Returns :
--
--    The progress bar of
--
   function Get_Bar (Widget : access Gtk_Progress_Record)
      return Gtk_Timed_Progress_Bar;
--
-- Initialize -- Construction to be called once by any derived type
--
--    Widget          - The result
--  [ Button ]        - The button
--    Button_Position - Where to place the button
--    Bar_Position    - Where to place the indicator
--    Timed           - If time estimation should be shown
--    Size            - Of the table
--    Spacing         - The default spacing to set
--    Homogeneous     - Table type
--    No_Viewer       - Postpone viewer creation
--
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
             );
   procedure Initialize
             (  Widget          : access Gtk_Progress_Record'Class;
                Button_Position : Gtk_Rectangle;
                Bar_Position    : Gtk_Rectangle;
                Timed           : Boolean;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean;
                No_Viewer       : Boolean
             );
private
   type Gtk_Progress_Record is new Gtk_Indicator_Record with record
      Status : Gtk_Timed_Progress_Bar;
   end record;

end Gtk.Indicators.Progress;
