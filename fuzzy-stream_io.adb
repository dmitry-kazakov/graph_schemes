--                                                                    --
--  package Fuzzy.Stream_IO         Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2010       --
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

package body Fuzzy.Stream_IO is
   pragma Assert (Stream_Element'Pos (Stream_Element'Last) >= 255);

   function Image (Data : Set) return Stream_Element_Array is
      Result : Stream_Element_Array (1..Data'Length);
   begin
      for Index in Data'Range loop
         Result (Stream_Element_Offset (Index - Data'First + 1)) :=
            Stream_Element (Data (Index) * 256.0);
      end loop;
      return Result;
   end Image;

   function Value (Data : Stream_Element_Array) return Set is
      Result : Set (1..Data'Length);
   begin
      for Index in Result'Range loop
         Result (Index) :=
            (  Integer_Confidence
               (  Data
                  (  Stream_Element_Offset (Index)
                  +  Data'First
                  -  1
               )  )
            /  256.0
            );
      end loop;
      return Result;
   end Value;

end Fuzzy.Stream_IO;
