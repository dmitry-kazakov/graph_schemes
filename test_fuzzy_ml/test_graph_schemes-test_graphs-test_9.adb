--                                                                    --
--  procedure Test_Graphs.Test_9    Copyright (c)  Dmitry A. Kazakov  --
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

with Confidence_Factors;       use Confidence_Factors;
with Fuzzy.Graph.Scheme;       use Fuzzy.Graph.Scheme;
with Integer_Intervals;        use Integer_Intervals;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_9 is
   Lesson  : Lecture_Handle := Create_Memory_Resident;
   X       : constant Feature_Handle := Create_Integer ("X", 0, 3);
   Xi      : constant Bounded_Array  := Create_Binary (X);
   Classes : constant Feature_Handle := Create_Tone_Feature ("tone");
   Tree    : Classifier_Handle;
begin
   Put_Line (File, "<hr><p><b>Test 9</b></p>");
   Put (Lesson, 1, X, Has_In, Value ("1..2", X)); 
   Put (Lesson, 1, Classes, Has_In,  Value ("A", Classes)); 
   Put (File, Lesson);
   Tree := Learn (Lesson, (Ref (Xi, 1), Ref (Xi, 2)), Classes);
   Put (File, Tree);
   if (  "X.1(0 => X.2(1 => tone = A:1:0); 1 => X.2(0 => tone = A:1:0))"
      /= Image (Get_Graph (Tree, Has_In))
      )
   then
      Raise_Exception
      (  Failed'Identity,
         (  "Error in Test 9 (got:"
         &  Image (Get_Graph (Tree, Has_In))
         &  ")"
      )  );
   end if;
end Test_9;
