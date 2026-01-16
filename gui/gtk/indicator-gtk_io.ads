--                                                                    --
--  package Indicators.Gtk_IO       Copyright (c)  Dmitry A. Kazakov  --
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
--  This package provides progress indicators based  on  GTK+  I/O.  The
--  implementation is task safe, that is the indicators  used  with  the
--  widgets provided  in  this  package  can  be  used  with  the  tasks
--  different from the task running the GTK+ loop.
--
with Gtk;               use Gtk;
with Gtk.Progress_Bar;  use Gtk.Progress_Bar;
with Gtk.Label;         use Gtk.Label;

with Indicator.Advance;

package Indicator.Gtk_IO is
   pragma Elaborate_Body (Indicator.Gtk_IO);
--
-- Timed_Progress_Bar_Class_Name
--
   Timed_Progress_Bar_Class_Name : constant String :=
      "GtkTimedProgressBar";
--
-- Gtk_Timed_Progress_Bar_Record -- Custom progress bar
--
-- This is a specialization of the standard progress  bar.  It  has  the
-- following additional style properties :
--
--    years   - The text "years";
--    days    - The text "days";
--    hours   - The text "h";
--    minutes - The text "min";
--    seconds - The text "s".
--
   type Gtk_Timed_Progress_Bar_Record is
      new Gtk_Progress_Bar_Record with null record;
   type Gtk_Timed_Progress_Bar is
      access all Gtk_Timed_Progress_Bar_Record'Class;
--
-- Get_Type -- Get the type of the progress bar widget
--
-- Returns :
--
--    The GTK+ widget type
--
   function Get_Type return Gtk_Type;
--
-- Gtk_New -- Factory
--
--    Widget - The result
--
   procedure Gtk_New
             (  Widget : out Gtk_Timed_Progress_Bar
             );
--
-- Initialize -- Construction
--
--     Widget - To initialize
--
-- This procedure must be called from a derived type in the beginning of
-- its own initialization.
--
   procedure Initialize
             (  Widget : not null access
                         Gtk_Timed_Progress_Bar_Record'Class
             );
--
-- Gtk_Indicator_Object -- Abstract GTK+ indicator
--
   type Gtk_Indicator_Object is
      abstract new Indicator.Advance.Progress_Object with private;
   type Gtk_Indicator_Object_Ptr is access Gtk_Indicator_Object'Class;
--
-- Cancel -- Overrides Indicators...
--
   overriding
   procedure Cancel (Viewer : in out Gtk_Indicator_Object);
--
-- Check -- Overrides Indicator...
--
   overriding
   procedure Check (Viewer : in out Gtk_Indicator_Object);
--
-- Done -- Overrides Indicator...
--
   overriding
   procedure Done (Viewer : in out Gtk_Indicator_Object);
--
-- Draw -- The visual appearance of the indicator
--
--    Viewer - The indicator
--
   procedure Draw (Viewer : in out Gtk_Indicator_Object) is abstract;
--
-- Reset -- Overrides Indicators...
--
   procedure Reset
             (  Viewer : in out Gtk_Indicator_Object;
                Total  : Natural := 0
             );
--
-- Bar -- Bar indicator
--
--    Widget - A Gtk_Progress_Bar widget to display the bar
--
-- This indicator shows progress bar with percents.
--
   type Bar (Widget : not null access Gtk_Progress_Bar_Record'Class) is
      new Gtk_Indicator_Object with private;
--
-- Timed_Bar -- Bar indicator with time to go
--
--    Widget - A Gtk_Timed_Progress_Bar widget to display the bar
--
-- This indicator shows progress bar with the estimated time to  go  and
-- percents.
--
   type Timed_Bar
        (  Widget  : not null access Gtk_Progress_Bar_Record'Class
        )  is new Gtk_Indicator_Object with private;
--
-- Counter -- Counter indicator
--
--    Widget - A Gtk_Text widget to display the counter
--
   type Counter (Widget : not null access Gtk_Label_Record'Class) is
      new Gtk_Indicator_Object with private;
   type Counter_Ptr is access Counter'Class;
--
-- Draw -- Implements Draw of Gtk_Indicator
--
   procedure Draw (Viewer : in out Bar);
   procedure Draw (Viewer : in out Timed_Bar);
   procedure Draw (Viewer : in out Counter);

private
   type Gtk_Indicator_Object is
      abstract new Indicator.Advance.Progress_Object with
   record
      Cancelled : Boolean := False;
   end record;

   type Bar (Widget : not null access Gtk_Progress_Bar_Record'Class) is
      new Gtk_Indicator_Object with null record;

   type Timed_Bar
        (  Widget : not null access Gtk_Progress_Bar_Record'Class
        )  is new Bar (Widget) with null record;

   type Counter (Widget : not null access Gtk_Label_Record'Class) is
      new Gtk_Indicator_Object with null record;

end Indicator.Gtk_IO;
