--                                                                    --
--  procedure Test_Graphs.Test_2    Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
--                                                 Spring, 2002       --
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
with Integer_Intervals;   use Integer_Intervals;

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_2 is
   Lesson  : Lecture_Handle := Create_Memory_Resident;
   Tree    : Classifier_Handle;
   X1      : constant Feature_Handle :=
                      Create_Discrete
                      (  "color",
                         (  "Red, Green, Blue, Yellow, Pink, Cyan, "
                         &  "Brown, Black, White"
                      )  );
   X2      : constant Feature_Handle :=
                      Create_Day_Of_Week_Feature ("day");
   Classes : constant Feature_Handle := Create_Tone_Feature ("tone");
begin
   Put_Line (File, "<hr><p><b>Test 2</b></p>");
   Put
   (  Lesson,
      1,
      X2,
      Has_Not,
      Value ("Mo..Fr, Sa:0.7", X2)
   );
   Put
   (  Lesson,
      1,
      Classes,
      Has_In,
      Value ("A", Classes)
   );
   Put (File, Lesson);
   Tree := Learn (Lesson, (X1, X2), Classes);
   Put (File, Tree);
   for Index in 1..Get_Cardinality (X2) loop
      Put_Line
      (  File,
         (  "<p>"
         &  Image (X2, Interval'(Index, Index))
         &  " is classified as:</p>"
      )  );
      declare
         Test  : Lecture_Handle := Create_Memory_Resident;
         Point : Set (1..Get_Cardinality (X2)) :=
                    (others => Confidence'Last);
      begin
         Point (Index) := Confidence'First;
         Put
         (  Test,
            1,
            X2,
            Has_Not,
            Point
         );         
         Put (File, Classes, Classify (Tree, Test, 1));
      end;
   end loop;
end Test_2;
