--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.HTML                   Luebeck            --
--  Interface                                      Spring, 2002       --
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
--
--  This package provides HTML output for fuzzy teaching sets.
--
with Ada.Text_IO;                use Ada.Text_IO;
with Fuzzy.Feature.Handle.HTML;  use Fuzzy.Feature.Handle.HTML; 
with Fuzzy.Feature.HTML;         use Fuzzy.Feature.HTML; 

package Fuzzy.Lecture.Handle.HTML is
   pragma Elaborate_Body (Fuzzy.Lecture.Handle.HTML);
--
-- Put -- Output a training set
--
--  [ File ]     - The output file
--    Lesson     - The handle of the teaching set
--    From       - The first example to put
--    To         - The last example to put
--    Parameters - The output parameters
--
-- This procedure writes the training set Lesson into the file  File  in
-- the  HTML  format.  The  parameters  From,  To determine the range of
-- examples to output. Parameters control the  output.  Additionally  to
-- their  fields  that  control  the  output  of  the  feature   values:
-- Parameters.Title is the background color of the table header. A  unit
-- specification  appears  after the feature name in the table header in
-- all  cases it is omitted after a value. In this case Parameters.Mode,
-- Parameters.Use_SI    etc    control   its   appearance.   Note   that
-- Parameters.Mode  controls  all text including the feature names. Make
-- sure that the character set of the feature names and their scales are
-- conform to the value Parameters.Mode.
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle
--    I/O exceptions
--
   procedure Put
             (  File       : File_Type;
                Lesson     : Lecture_Handle;
                From       : Positive        := 1;
                To         : Positive        := Positive'Last;
                Parameters : HTML_Parameters := HTML_Defaults
             );
   procedure Put
             (  Lesson     : Lecture_Handle;
                From       : Positive        := 1;
                To         : Positive        := Positive'Last;
                Parameters : HTML_Parameters := HTML_Defaults
            );
end Fuzzy.Lecture.Handle.HTML;
