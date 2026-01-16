--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.General                       Luebeck            --
--  Interface                                      Winter, 2002       --
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
--
--  This  package  provides  a  fuzzy  training  set type which may hold
--  training examples of any type. The whole set is memory allocated. 
--
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Memory_Resident;

package Fuzzy.Lecture.General is
   pragma Elaborate_Body (Fuzzy.Lecture.General);
--
-- Class -- Name of the class of memory-mapped training sets
--
   Class : String renames Fuzzy.Lecture.Memory_Resident.Class;
--
-- Create -- A memory-resident training set
--
-- Returns :
--
--    Handle to the set
--
   function Create return Lecture_Handle;

end Fuzzy.Lecture.General;
