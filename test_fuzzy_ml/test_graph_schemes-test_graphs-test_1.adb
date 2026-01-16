--                                                                    --
--  procedure Test_Graphs.Test_1    Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
--                                                 Winter, 2003       --
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

with Fuzzy.Graph.Handle.HTML;      use Fuzzy.Graph.Handle.HTML;
with Fuzzy.Graph.Scheme;           use Fuzzy.Graph.Scheme;
with Fuzzy.Feature.Handle.Tables;  use Fuzzy.Feature.Handle.Tables;

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_1 is
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
   Put_Line (File, "<hr><p><b>Test 1</b></p>");
   declare
      Node     : Node_Handle;
      Features : Table;
   begin
      Add (Features, "color", X1);
      Add (Features, "day",   X2);
      Add (Features, "tone",  Classes);
      Node :=
         Value
         (  (  "color"
            &  "(  Red..Blue, Pink => "
            &  "      day "
            &  "      (  MO, TH..SU => tone = C..E; "
            &  "         WE         => tone = A"
            &  "      );" 
            &  "   White =>"
            &  "      day "
            &  "      (  WE, TH, FR => tone = C,D "
            &  "      )" 
            &  ")"
            ),
            Features
         );
      Put_Line (File, "<p>Node read from a string:</p>");
      Put (File, Node);
   end;   
   Put (Lesson, 1, Classes, Has_In, Value ("D", Classes));
   Put (Lesson, 2, X2, Has_In, Value ("We", X2));
   Put (Lesson, 2, Classes, Has_In, Value ("A", Classes));
   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   Put_Line (File, "<p>Learnt from the example 1:</p>");
   Tree := Learn (Lesson, (X1, X2), Classes, 1, 1);
   Put (File, Tree);

   Put_Line (File, "<p>Learnt incrementally from example 2:</p>");
   Learn (Tree, Lesson, (X1, X2), 2, 2);
   Put (File, Tree);

   Put_Line (File, "<p>Learnt from the set:</p>");
   Tree := Learn (Lesson, (X1, X2), Classes);
   Put (File, Tree);
   if (  Image (Get_Graph (Tree, Has_In))
      /= (  "day(MO..TU, TH..SU => tone = D:1:0; "
         &  "WE => tone = D:1:0, A:1:0)"
      )  )
   then
      Raise_Exception
      (  Failed'Identity,
         "Wrong graph generated in Test 1"
      );
   end if;
end Test_1;
