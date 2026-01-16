--                                                                    --
--  package Indicator               Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2003       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

package body Indicator is

   procedure Cancel (Viewer : in out Indicator_Object) is
   begin
      null;
   end Cancel;

   procedure Check (Viewer : in out Indicator_Object) is
   begin
      null;
   end Check;

   procedure Done (Viewer : in out Indicator_Object) is
   begin
      null;
   end Done;

   procedure Reset
             (  Viewer : in out Indicator_Object;
                Total  : Natural := 0
             )  is
   begin
      null;
   end Reset;

   procedure Set_Data
             (  Viewer : in out Indicator_Object;
                Data   : in out Indicator_Data'Class
             )  is
   begin
      null;
   end Set_Data;

   procedure Set_Data (Viewer : in out Indicator_Object) is
   begin
      null;
   end Set_Data;

end Indicator;
