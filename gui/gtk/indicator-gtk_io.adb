--                                                                    --
--  package Indicators.Gtk_IO       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spriong, 2006      --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.IO_Exceptions;         use Ada.IO_Exceptions;
with Glib;                      use Glib;
with GLib.Object;               use GLib.Object;
with GLib.Properties.Creation;  use GLib.Properties.Creation;
with GLib.Types;                use GLib.Types;
with Gtk.Main.Router;           use Gtk.Main.Router;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Widget.Styles;         use Gtk.Widget.Styles;
with Strings_Edit.Floats;       use Strings_Edit.Floats;

package body Indicator.Gtk_IO is
   use Indicator.Advance;

   Timed_Progress_Bar_Class_Record : aliased Ada_GObject_Class :=
                                             Uninitialized_Class;
   function Get_Type return Gtk_Type is
   begin
      if Initialize_Class_Record
         (  Ancestor     => Gtk.Progress_Bar.Get_Type,
            Class_Record => Timed_Progress_Bar_Class_Record'Access,
            Type_Name    => Timed_Progress_Bar_Class_Name
         )
      then
         Install_Style_Property
         (  Class_Ref (Timed_Progress_Bar_Class_Record.The_Type),
            Gnew_String
            (  Name    => "years",
               Nick    => "Years",
               Blurb   => "The duration in years",
               Default => "years"
         )  );
         Install_Style_Property
         (  Class_Ref (Timed_Progress_Bar_Class_Record.The_Type),
            Gnew_String
            (  Name    => "days",
               Nick    => "Days",
               Blurb   => "The duration in days",
               Default => "days"
         )  );
         Install_Style_Property
         (  Class_Ref (Timed_Progress_Bar_Class_Record.The_Type),
            Gnew_String
            (  Name    => "hours",
               Nick    => "Hours",
               Blurb   => "The duration in hours",
               Default => "h"
         )  );
         Install_Style_Property
         (  Class_Ref (Timed_Progress_Bar_Class_Record.The_Type),
            Gnew_String
            (  Name    => "minutes",
               Nick    => "Minutes",
               Blurb   => "The duration in minutes",
               Default => "min"
         )  );
         Install_Style_Property
         (  Class_Ref (Timed_Progress_Bar_Class_Record.The_Type),
            Gnew_String
            (  Name    => "seconds",
               Nick    => "Seconds",
               Blurb   => "The duration in seconds",
               Default => "s"
         )  );
      end if;
      return Timed_Progress_Bar_Class_Record.The_Type;
   end Get_Type;

   procedure Gtk_New (Widget : out Gtk_Timed_Progress_Bar) is
   begin
      Widget := new Gtk_Timed_Progress_Bar_Record;
      Indicator.Gtk_IO.Initialize (Widget);
   end Gtk_New;

   procedure Initialize
             (  Widget : not null access
                         Gtk_Timed_Progress_Bar_Record'Class
             )  is
   begin
      G_New (Widget, Get_Type);
      Gtk.Progress_Bar.Initialize (Widget);
   end Initialize;

   type Progress_Data (Viewer : access Gtk_Indicator_Object'Class) is
      new Request_Data with null record;

   procedure Service (Data : in out Progress_Data);

   procedure Service (Data : in out Progress_Data) is
   begin
      if not Data.Viewer.Cancelled then
         Draw (Data.Viewer.all);
      end if;
   end Service;

   procedure Cancel (Viewer : in out Gtk_Indicator_Object) is
   begin
      Viewer.Cancelled := True;
   end Cancel;

   procedure Check (Viewer : in out Gtk_Indicator_Object) is
      Update : Boolean;
   begin
      if Viewer.Cancelled then
         Viewer.Cancelled := False;
         raise End_Error;
      end if;
      On_Check (Viewer, Update);
      if Update then
         declare
            Data : Progress_Data (Viewer'Access);
         begin
            Request (Data);
         end;
      end if;
   end Check;

   procedure Done (Viewer : in out Gtk_Indicator_Object) is
      Update : Boolean;
   begin
      if Viewer.Cancelled then
         Viewer.Cancelled := False;
         raise End_Error;
      end if;
      On_Done (Viewer, Update);
      if Update then
         declare
            Data : Progress_Data (Viewer'Access);
         begin
            Request (Data);
         end;
      end if;
   end Done;

   procedure Reset
             (  Viewer : in out Gtk_Indicator_Object;
                Total  : Natural := 0
             )  is
   begin
      if Viewer.Cancelled then
         Viewer.Cancelled := False;
         raise End_Error;
      end if;
      Reset (Progress_Object (Viewer), Total);
   end Reset;

   function Get_Text (Viewer : Bar'Class) return String is
      Count : constant Float := Get (Viewer);
   begin
      if Count < 1.0 then
         return Image (Value => Count * 100.0, AbsSmall => -2) & '%';
      else
         return "100%";
      end if;
   end Get_Text;

   procedure Draw (Viewer : in out Bar) is
      Active : Boolean;
   begin
      if Viewer.Cancelled then
         return;
      end if;
      Set_Text (Viewer.Widget, Get_Text (Viewer));
      Set_Fraction (Viewer.Widget, Gdouble (Float'(Get (Viewer))));
      while Gtk.Main.Events_Pending loop
         Active := Gtk.Main.Main_Iteration;
      end loop;
   end Draw;

   procedure Draw (Viewer : in out Timed_Bar) is
      Active : Boolean;
      Count  : constant Float := Get (Viewer);
      Minute : constant Float := 60.0;
      Hour   : constant Float := 60.0  * Minute;
      Day    : constant Float := 24.0  * Hour;
      Year   : constant Float := 365.0 * Day;
      Prefix : String renames Get_Text (Viewer);
   begin
      if Count > 0.0 and then Count < 1.0 then
         declare
            Seconds : constant Float :=
                         (  Float (Clock - Get (Viewer))
                         *  (1.0 - Count)
                         /  Count
                         );
         begin
            if Seconds > Year then
               Set_Text
               (  Viewer.Widget,
                  (  Prefix
                  &  " +"
                  &  Image (Seconds / Year, AbsSmall=>-1)
                  &  Style_Get (Viewer.Widget, "years")
               )  );
            elsif Seconds > Day then
               Set_Text
               (  Viewer.Widget,
                  (  Prefix
                  &  " +"
                  &  Image (Seconds / Day, AbsSmall=>-1)
                  &  Style_Get (Viewer.Widget, "days")
               )  );
            elsif Seconds > Hour then
               Set_Text
               (  Viewer.Widget,
                  (  Prefix
                  &  " +"
                  &  Image (Seconds / Hour, AbsSmall=>-1)
                  &  Style_Get (Viewer.Widget, "hours")
               )  );
            elsif Seconds > Minute then
               Set_Text
               (  Viewer.Widget,
                  (  Prefix
                  &  " +"
                  &  Image (Seconds / Minute, AbsSmall=>-1)
                  &  Style_Get (Viewer.Widget, "minutes")
               )  );
            elsif Seconds > 0.5 then
               Set_Text
               (  Viewer.Widget,
                  (  Prefix
                  &  " +"
                  &  Image (Seconds, AbsSmall=>0)
                  &  Style_Get (Viewer.Widget, "seconds")
               )  );
            end if;
         end;
      else
         Set_Text (Viewer.Widget, Prefix);
      end if;
      Set_Fraction
      (  Viewer.Widget,
         Gdouble'Min (1.0, Gdouble (Float'(Get (Viewer))))
      );
      while Gtk.Main.Events_Pending loop
         Active := Gtk.Main.Main_Iteration;
      end loop;
   end Draw;

   procedure Draw (Viewer : in out Counter) is
      Active : Boolean;
   begin
      Set_Text (Viewer.Widget, Image (Get (Viewer), AbsSmall=>0));
      while Gtk.Main.Events_Pending loop
         Active := Gtk.Main.Main_Iteration;
      end loop;
   end Draw;

end Indicator.Gtk_IO;
