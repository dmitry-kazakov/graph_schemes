--                                                                    --
--  package Indicators.Advance      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Indicator.Advance is

   function Get_Refresh (Viewer : Progress_Object) return Duration is
   begin
      return Viewer.Refresh;
   end Get_Refresh;

   procedure Check_Update
             (  Viewer : in out Progress_Object;
                Update : in out Boolean
             )  is
   begin
      if Update or else Clock - Viewer.That_Time >= Viewer.Refresh then
         Update := True;
         Viewer.That_Time := Clock;
      end if;
   end Check_Update;

   function Get (Viewer : Progress_Object) return Float is
   begin
      return Viewer.Now.Count;
   end Get;

   function Get (Viewer : Progress_Object) return Time is
   begin
      return Viewer.Start;
   end Get;

   procedure On_Check
             (  Viewer : in out Progress_Object;
                Update : out Boolean
             )  is
   begin
      Update := False;
      if not Viewer.Empty then
         Viewer.Now.Count := Viewer.Now.Count + Viewer.Now.Step;
         Check_Update (Viewer, Update);
      end if;
   end On_Check;

   procedure On_Done
             (  Viewer : in out Progress_Object;
                Update : out Boolean
             )  is
   begin
      Update := False;
      if not Viewer.Empty then
         if Is_Empty (Viewer.History) then
            Viewer.Empty := True;
            if Viewer.Now.Step < 1.0 then
               Viewer.Now.Count := 1.0;
            else
               Viewer.Now.Count := Viewer.Now.Count + Viewer.Now.Step;
            end if;
            Update := True;
            Check_Update (Viewer, Update);
         else
            Viewer.Now := Top (Viewer.History);
            Pop (Viewer.History);         
            Viewer.Now.Count := Viewer.Now.Count + Viewer.Now.Step;
            Check_Update (Viewer, Update);
         end if;
      end if;
   end On_Done;

   procedure Reset
             (  Viewer : in out Progress_Object;
                Total  : Natural := 0
             )  is
   begin
      if Viewer.Empty then
         Viewer.Empty     := False;
         Viewer.Start     := Clock;
         Viewer.That_Time := Viewer.Start - Viewer.Refresh;         
      else
         Push (Viewer.History, Viewer.Now);
      end if;
      if Total /= 0 then
         Viewer.Now.Step := Viewer.Now.Step / Float (Total);
      end if;
   end Reset;

   procedure Set_Refresh
             (  Viewer  : in out Progress_Object;
                Refresh : Duration
             )  is
   begin
      Viewer.Refresh := Refresh;
   end Set_Refresh;

end Indicator.Advance;
