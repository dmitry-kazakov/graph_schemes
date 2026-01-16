--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.General.Text_Dialogue         Luebeck            --
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

with Fuzzy.Lecture.Memory_Resident;

package Fuzzy.Lecture.General.Text_Dialogue is
   pragma Elaborate_Body (Fuzzy.Lecture.General.Text_Dialogue);
--
-- Class -- Name of the class of dialogue memory-mapped training sets
--
   Class : constant String :=
              Fuzzy.Lecture.General.Class & ".Text_Dialogue";
--
-- Create -- A dialogue memory-mapped training set
--
-- Returns :
--
--    Handle to the set
--
   function Create return Lecture_Handle;

private
   type Dialogue_Lecture_Object is
      new Fuzzy.Lecture.Memory_Resident.General_Lecture_Object with
         null record;
--
-- Query -- Overrides Fuzzy.Lecture...
--
   procedure Query
             (  Lesson  : in out Dialogue_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
end Fuzzy.Lecture.General.Text_Dialogue;
