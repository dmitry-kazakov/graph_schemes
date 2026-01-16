--                                                                    --
--  package Indicator.Text_IO       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Strings_Edit.Floats;     use Strings_Edit.Floats;

package body Indicator.Text_IO is

   procedure Show
             (  Viewer : in out Text_Indicator_Object'Class;
                Text   : String
             )  is
      Excess : constant Integer := Viewer.Written - Text'Length;
   begin
      if Excess > 0 then
         Put (Viewer.Written * BS & Text & Excess * ' ');
      else
         Put (Viewer.Written * BS & Text);
         Viewer.Written := Text'Length;
      end if;
   end Show;

   procedure Check (Viewer : in out Bar) is
      Update : Boolean;
   begin
      On_Check (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Check;

   procedure Check (Viewer : in out Timed_Bar) is
      Update : Boolean;
   begin
      On_Check (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Check;

   procedure Check (Viewer : in out Counter) is
      Update : Boolean;
   begin
      On_Check (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Check;

   procedure Done (Viewer : in out Bar) is
      Update : Boolean;
   begin
      On_Done (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Done;

   procedure Done (Viewer : in out Timed_Bar) is
      Update : Boolean;
   begin
      On_Done (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Done;

   procedure Done (Viewer : in out Counter) is
      Update : Boolean;
   begin
      On_Done (Viewer, Update);
      if Update then
         Show (Viewer, Get_Text (Viewer));
      end if;
   end Done;

   function Get_Text (Viewer : Bar) return String is
      State : constant Float := Get (Viewer);
   begin
      if State < 1.0 then
         declare
            Width : constant Natural :=
                             Natural (State * Float (Viewer.Size * 2));
            Count : constant Natural := Width / 2;
         begin
            if (Width mod 2) = 0 then
               return
               (  '|'
               &  Count * 'H'
               &  (Viewer.Size - Count) * ' '
               &  "| "
               &  Image (Value => State * 100.0, AbsSmall => -2)
               &  '%'
               );
            else
               return
               (  '|'
               &  Count * 'H'
               &  'I'
               &  (Viewer.Size - Count - 1) * ' '
               &  "| "
               &  Image (Value => State * 100.0, AbsSmall => -2)
               &  '%'
               );
            end if;
         end;
      else
         return '|' & Viewer.Size * 'H' & "| 100%";
      end if;
   end Get_Text;

   function Get_Text (Viewer : Timed_Bar) return String is
      Count  : constant Float := Get (Viewer);
      Minute : constant Float := 60.0;
      Hour   : constant Float := 60.0  * Minute;
      Day    : constant Float := 24.0  * Hour;
      Year   : constant Float := 365.0 * Day;
      Prefix : String renames Get_Text (Bar (Viewer));
   begin
      if Count > 0.0 then
         declare
            Seconds : constant Float :=
                         (  Float (Clock - Get (Viewer))
                         *  (1.0 - Count)
                         /  Count
                         );
         begin
            if Seconds > Year then
               return
               (  Prefix
               &  " +"
               &  Image (Seconds / Year, AbsSmall=>-1)
               &  " years"
               );
            elsif Seconds > Day then
               return
               (  Prefix
               &  " +"
               &  Image (Seconds / Day, AbsSmall=>-1)
               &  " days"
               );
            elsif Seconds > Hour then
               return
               (  Prefix
               &  " +"
               &  Image (Seconds / Hour, AbsSmall=>-1)
               &  "h"
               );
            elsif Seconds > Minute then
               return
               (  Prefix
               &  " +"
               &  Image (Seconds / Minute, AbsSmall=>-1)
               &  "min"
               );
            elsif Seconds > 0.5 then
               return
               (  Prefix
               &  " +"
               &  Image (Seconds, AbsSmall=>0)
               &  "s"
               );
            end if;
         end;
      end if;
      return Prefix;
   end Get_Text;

   function Get_Text (Viewer : Counter) return String is
   begin
      return Image (Get (Viewer), AbsSmall=>0) & Viewer.Suffix.all;
   end Get_Text;

end Indicator.Text_IO;
