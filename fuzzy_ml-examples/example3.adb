--                                                                    --
--  procedure Example3              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2002       --
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Confidence_Factors;            use Confidence_Factors;
with Fuzzy.Classifier.Handle;       use Fuzzy.Classifier.Handle;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.HTML;     use Fuzzy.Lecture.Handle.HTML;
with Fuzzy.Lecture.Handle.Text_IO;  use Fuzzy.Lecture.Handle.Text_IO;

with Fuzzy.Classifier.Handle.Factory;
use  Fuzzy.Classifier.Handle.Factory;

procedure Example3 is
   File   : File_Type;
   Lesson : Lecture_Handle;
   X      : constant Feature_Handle := Create_Integer ("X", 1, 7);
   Y      : constant Feature_Handle := Create_Integer ("Y", 1, 7);
   Class  : constant Feature_Handle :=
               Create_Discrete ("Class", "First, Second");
   procedure Report (Scheme : Classifier_Handle) is separate;
begin
   --
   -- Create an output file
   --
   Create (File, Out_File, "Example3.htm");
   --
   -- Read a training set from a file
   --
   Lesson := Read ("Example3.txt", (X, Y, Class));
   --
   -- Write the training set into the output file
   --
   Put_Line (File, "The training set:");
   Put (File, Lesson);
   --
   -- Learn using X and Y
   --
   Report
   (  Learn
      (  Lesson, (X, Y),
         Class,
         Equivalence => Confidence'Last
   )  );
   --
   -- Close the file
   --
   Close (File);
end Example3;
