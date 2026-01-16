--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.Bulk                   Luebeck            --
--  Interface                                      Winter, 2010       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Ada.Finalization;

package Fuzzy.Lecture.Handle.Bulk is
--
-- Lecture_Update -- Training set bulk update
--
--    Lesson -- The training set (pointer to)
--
-- When  the  object  is  created  Begin_Bulk_Update  is  called.   Upon
-- finalization End_Bulk_Update is called.
--
   type Lecture_Update
        (  Lesson : not null access Lecture_Object'Class
        )  is new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Object destruction
--
--   Transaction - The object
--
   procedure Finalize (Transaction : in out Lecture_Update);
--
-- Initialize -- Object construction
--
--   Transaction - The object
--
   procedure Initialize (Transaction : in out Lecture_Update);

private
   type Lecture_Update
        (  Lesson : not null access Lecture_Object'Class
        )  is new Ada.Finalization.Limited_Controlled with null record;

end Fuzzy.Lecture.Handle.Bulk;
