--                                                                    --
--  procedure Example3.Report       Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
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

with Confidence_Factors.Edit;       use Confidence_Factors.Edit;
with Fuzzy.Classifier.Handle.HTML;  use Fuzzy.Classifier.Handle.HTML;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Integer_Intervals;             use Integer_Intervals;
with HTML;                          use HTML;

separate (Example3) procedure Report (Scheme : Classifier_Handle) is
   Test    : Lecture_Handle;
   Example : Integer := 0;
begin
   Put_Line (File, "<p>The result of training:</p>");
   --
   -- Write the result of learning into the file
   --
   Put (File, Scheme);
   --
   -- Let's see how it classifies the singletons
   --
   for X_Index in 1..Get_Cardinality (X) loop
      for Y_Index in 1..Get_Cardinality (Y) loop
         Example := Example + 1;
         Put (Test, Example, X, Value (Integer'Image (X_Index), X));
         Put (Test, Example, Y, Value (Integer'Image (Y_Index), Y));
         Put (Test, Example, Class, Classify (Scheme, Test, Example));
      end loop;
   end loop;
   --
   -- Images of the classes:
   --
   for Class_No in 1..Get_Cardinality (Class) loop
      Put_Line
      (  File,
         (  Break
         &  "Image of "
         &  Image
            (  Class,
               Interval'(Class_No, Class_No)
      )  )  );
      Example := 1;
      for X_Index in 1..Get_Cardinality (X) loop
         Put_Line (File, Break);
         for Y_Index in 1..Get_Cardinality (Y) loop
            Put_Line
            (  File,
               Image
               (  Classify
                  (  Scheme, Test, Example
                  ) .Possibility (Class_No)
            )  );
            Example := Example + 1;
         end loop;
      end loop;
   end loop;
end Report;
