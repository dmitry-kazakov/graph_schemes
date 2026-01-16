--                                                                    --
--  procedure Test_Graphs.Test_11   Copyright (c)  Dmitry A. Kazakov  --
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

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_11 is
   Lesson : Lecture_Handle := Create_Memory_Resident;
   X1 : constant Feature_Handle :=
        Create_Discrete
        (  "color",
           "Red, Green, Blue, Yellow, Pink, Cyan, Brown, Black, White"
        );
   X2      : constant Feature_Handle :=
                      Create_Day_Of_Week_Feature ("day");
   Classes : constant Feature_Handle := Create_Tone_Feature ("tone");
   Tree    : Classifier_Handle;
begin
   Put_Line (File, "<hr><p><b>Test 11</b></p>");
   Put_Line
   (  File,
      (  "<p>A fuzzy set over the domain of the feature "
      &  Get_Name (X1)
      &  ":</p>"
   )  );
   Put (File, X1, Set'(0.2, 0.4, 0.7, 0.9, 0.5, 0.1, 0.0, 0.0, 0.1));
   Put_Line
   (  File,
      (  "<p>A value of the feature "
      &  Get_Name (X1)
      &  ". It has both possibilities and necessities:</p>"
   )  );
   Put
   (  File,
      X1,
      Fuzzy.Intuitionistic.Classification'
      (  Cardinality => 9,
         Necessity   => (0.1, 0.0, 0.7, 0.5, 0.2, 0.0, 0.0, 0.0, 0.0),
         Possibility => (0.2, 0.4, 0.7, 0.9, 0.5, 0.1, 0.0, 0.0, 0.1)
   )  );
   Put (Lesson, 1, X1,      Has_In, 1);
   Put (Lesson, 1, X2,      Has_In, 1);
   Put (Lesson, 1, Classes, Has_In, 1);
   Put (Lesson, 2, X1,      Has_In, 1);
   Put (Lesson, 2, X2,      Has_In, 2);
   Put (Lesson, 2, Classes, Has_In, 2);
   Put (Lesson, 3, X1,      Has_In, (1=>0.9, 2=>0.9, 3..9=>0.0));
   Put (Lesson, 3, X2,      Has_In, 2);
   Put (Lesson, 3, Classes, Has_In, 3);
   Put_Line (File, "<p>A set of examples to learn from:</p>");
   Put (File, Lesson);
   Tree := Learn (Lesson, (X1, X2), Classes);
   Put_Line (File, "<p>The result of learning:</p>");
   Put (File, Tree);
   Put_Line (File, "<p>The teaching set extracted from the graph,");
   Put_Line (File, "note that the features go in reverse order:</p>");
   declare
      Lesson : Lecture_Handle := Create_Memory_Resident;
   begin
      Get_Examples (Tree, Lesson);
      Put (File, Lesson);
   end;
   declare
      Lesson : Lecture_Handle := Create_Memory_Resident;
   begin
      Put (Lesson, 1, X1, Has_In, (2..5=>0.0, 1|6..9=>0.8));
      Put (Lesson, 1, X2, Has_In, 2);
      Put (Lesson, 1, Classes, Has_In, 4);
      Put_Line (File, "<p>A set to incrementally learn from:</p>");
      Put (File, Lesson);
      Learn (Tree, Lesson, (X1, X2));
      Put_Line (File, "<p>The result of learning:</p>");
      Put (File, Tree);
      Put_Line (File, "<p>Some statistics here:<ul>");
      Put_Line
      (  File,
         (  "<li>Number of features: "
         &  Image (Get_Number_Of_Features)
         &  "</li>"
      )  );
      Put_Line
      (  File,
         (  "<li>Number of nodes: "
         &  Image (Get_Number_Of_Nodes)
         &  "</li>"
      )  );
      Put_Line
      (  File,
         (  "<li>Number of lectures: "
         &  Image (Get_Number_Of_Lectures)
         &  "</li>"
      )  );
      Put_Line (File, "</ul></p>");
   end;
end Test_11;
