--                                                                    --
--  procedure Test_Graphs.Test_12   Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
--                                                 Summer, 2004       --
--                                                                    --
--                                Last revision :  18:58 25 Jul 2018  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Confidence_Factors;  use Confidence_Factors;
with Fuzzy.Graph.Scheme;  use Fuzzy.Graph.Scheme;
with Integer_Intervals;   use Integer_Intervals;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_12 is
   Lesson  : Lecture_Handle := Create_Memory_Resident;
   X       : constant Feature_Handle := Create_Discrete ("X", "x1, x2");
   Y       : constant Feature_Handle := Create_Discrete ("Y", "y1, y2");
   Z       : constant Feature_Handle := Create_Discrete ("Z", "z1, z2");
   Classes : constant Feature_Handle := Create_Integer  ("Class", 1, 3);
   Tree    : Classifier_Handle;
begin
   Put_Line (File, "<hr><p><b>Test 12</b></p>");
   Put (Lesson, 1, X, Has_In, Value ("x1", X));
   Put (Lesson, 1, Y, Has_In, Value ("y1", Y));
   Put (Lesson, 1, Z, Has_In, Value ("z1", Z));
   Put (Lesson, 1, Classes, Has_In,  Value ("1", Classes));

   Put (Lesson, 2, X, Has_In, Value ("x2", X));
   Put (Lesson, 2, Y, Has_In, Value ("y1", Y));
   Put (Lesson, 2, Z, Has_In, Value ("z1", Z));
   Put (Lesson, 2, Classes, Has_In,  Value ("1", Classes));
   
   Put (Lesson, 3, X, Has_In, Value ("x1", X));
   Put (Lesson, 3, Y, Has_In, Value ("y2", Y));
   Put (Lesson, 3, Z, Has_In, Value ("z1", Z));
   Put (Lesson, 3, Classes, Has_In,  Value ("2", Classes));

   Put (Lesson, 4, X, Has_In, Value ("x2", X));
   Put (Lesson, 4, Y, Has_In, Value ("y2", Y));
   Put (Lesson, 4, Z, Has_In, Value ("z1", Z));
   Put (Lesson, 4, Classes, Has_In,  Value ("2", Classes));

   Put (Lesson, 5, X, Has_In, Value ("x1", X));
   Put (Lesson, 5, Y, Has_In, Value ("y1", Y));
   Put (Lesson, 5, Z, Has_In, Value ("z2", Z));
   Put (Lesson, 5, Classes, Has_In,  Value ("3", Classes));

   Put (Lesson, 6, X, Has_In, Value ("x2", X));
   Put (Lesson, 6, Y, Has_In, Value ("y1", Y));
   Put (Lesson, 6, Z, Has_In, Value ("z2", Z));
   Put (Lesson, 6, Classes, Has_In,  Value ("3", Classes));

   Put (Lesson, 7, X, Has_In, Value ("x1", X));
   Put (Lesson, 7, Y, Has_In, Value ("y2", Y));
   Put (Lesson, 7, Z, Has_In, Value ("z2", Z));
   Put (Lesson, 7, Classes, Has_In,  Value ("3", Classes));

   Put (File, Lesson);
   Tree := Learn (Lesson, (X, Y, Z), Classes);
   Put (File, Tree);
end Test_12;
