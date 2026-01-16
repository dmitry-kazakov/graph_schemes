--                                                                    --
--  package Gtk.Indicators          Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2006       --
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
--  This package provides abstract progress indicator based on GTK+ I/O.
--  The progress indicator is a GTK+ table widget.  It  can  be  derived
--  from to obtain a concrete indicator widget. Destroyng the widget  is
--  equivalent  to  calling Cancel on the indicator, as well as pressing
--  the button of the widget.
--
with Glib;              use Glib;
with Gtk.Button;        use Gtk.Button;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Handlers;      use Gtk.Handlers;
with Gtk.Table;         use Gtk.Table;
with Gtk.Widget;        use Gtk.Widget;
with Indicator.Handle;  use Indicator.Handle;

package Gtk.Indicators is
   pragma Elaborate_Body (Gtk.Indicators);

   type Gtk_Size is record
      Width  : Guint;
      Height : Guint;
   end record;

   type Gtk_Rectangle is record
      Left   : Guint;
      Right  : Guint;
      Top    : Guint;
      Bottom : Guint;
   end record;
--
-- Gtk_Indicator_Record -- Indicator widget, abstract base
--
   type Gtk_Indicator_Record is abstract
      new Gtk_Table_Record with private;
   type Gtk_Indicator is access all Gtk_Indicator_Record'Class;
--
-- Get_Button -- Get the button of the indicator
--
--    Widget - The widget
--
-- Returns :
--
--    The button of the indicator (can be null)
--
   function Get_Button (Widget : not null access Gtk_Indicator_Record)
      return Gtk_Button;
--
-- Get_Indicator -- Get a handle to the indicator associated with
--
--    Widget - The widget
--
-- Returns :
--
--    A handle to the indicator of
--
   function Get_Indicator
            (  Widget : not null access Gtk_Indicator_Record
            )  return Indicator_Handle;
--
-- Get_Type -- Widget type
--
-- Returns :
--
--    The widget type
--
   function Get_Type return GType renames Gtk.Table.Get_Type;
--
-- Initialize -- To be called by any derived type after construction
--
--    Widget          - The widget
--    Viewer          - The indicator to attach to
--  [ Button ]        - The cancel button
--    Button_Position - Button location
--    Size            - Of the table
--    Spacing         - The default row spacing to set
--    Homogeneous     - Table type
--
-- The parameters Rows and Columns determine the table size. The  button
-- spans rows when left or right, and columns when top  or  bottom.  The
-- parameter Button_Position determines the button location.  The widget
-- references Button if not null.
--
--   Pos_Left, Size=(3,4)     Pos_Bottom, Size=(4,3)
--   .________.____.____.     .____.____.____.____.
--   |        |    |    |     |    |    |    |    |
--   |        |____|____|     |____|____|____|____|
--   |        |    |    |     |    |    |    |    |
--   | Button |____|____|     |____|____|____|____|
--   |        |    |    |     |                   |
--   |        |____|____|     |                   |
--   |        |    |    |     |      Button       |
--   |________|____|____|     |___________________|
--
   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button          : not null access
                                  Gtk_Button_Record'Class;
                Button_Position : Gtk_Position_Type;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             );
   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button_Position : Gtk_Position_Type;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             );
   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button          : not null access
                                  Gtk_Button_Record'Class;
                Button_Position : Gtk_Rectangle;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             );
   procedure Initialize
             (  Widget          : not null access
                                  Gtk_Indicator_Record'Class;
                Viewer          : Indicator_Handle;
                Button_Position : Gtk_Rectangle;
                Size            : Gtk_Size;
                Spacing         : Gtk_Size;
                Homogeneous     : Boolean
             );
--
-- Set_Viewer -- Set / change the viewer associated with
--
--    Widget - The widget
--    Viewer - The indicator to attach to
--
   procedure Set_Viewer
             (  Widget : not null access Gtk_Indicator_Record;
                Viewer : Indicator_Handle
             );
private
   use Indicator;

   type Gtk_Indicator_Record is new Gtk_Table_Record with record
      Cancel_Button : Gtk_Button;
      Viewer        : Indicator_Handle;
   end record;
--
-- Destroy -- Overrides Gtk.Object...
--
--    Viewer - The indicator
--
   overriding
   procedure Destroy (Viewer : not null access Gtk_Indicator_Record);
--
-- Indicator_Callbacks -- This   package   provides  Interface  to  GTK+
--                        objects,   takes   Indicator_Object_Ptr  as  a
-- parameter, can be emitted from any widget.
--
   package Indicator_Callbacks is
      new User_Callback (Gtk_Widget_Record, Indicator_Object_Ptr);
--
-- Widget_Callbacks -- Interface to GTK+ objects, can be emitted from an
--                     indicator widget only
--
   package Widget_Callbacks is new Callback (Gtk_Indicator_Record);
--
-- Return_Callback -- Interface to GTK+ objects, can be emitted from  an
--                    indicator widget only
--
   package Function_Callbacks is
      new Return_Callback (Gtk_Indicator_Record, Boolean);

end Gtk.Indicators;
