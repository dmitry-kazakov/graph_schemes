--                                                                    --
--  procedure Test_Graphs.Test_13   Copyright (c)  Dmitry A. Kazakov  --
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

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_13 is
   Lesson  : Lecture_Handle;
   T1, T2  : Classifier_Handle;
   X1      : constant Feature_Handle := Create_Discrete ("X1", "A, B");
   X2      : constant Feature_Handle := Create_Discrete ("X2", "C, D");
   Classes : constant Feature_Handle :=
                      Create_Discrete ("Classes", "E, F");
begin
   Put_Line (File, "<hr><p><b>Test 13</b></p>");

   Put (Lesson, 1, X1,      Has_In, Value ("A", X1));
   Put (Lesson, 1, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 1, Classes, Has_In, Value ("E", Classes));
   Put (Lesson, 2, X1,      Has_In, Value ("B", X1));
   Put (Lesson, 2, X2,      Has_In, Value ("D", X2));
   Put (Lesson, 2, Classes, Has_In, Value ("F", Classes));
   Put (Lesson, 3, X1,      Has_In, Value ("B", X1));
   Put (Lesson, 3, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 3, Classes, Has_In, Value ("E", Classes));
   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   T2 := Learn (Lesson, (X1, X2), Classes);
   Put_Line (File, "<p>Learnt from the examples:</p>");
   Put (File, T2);
   if (  Image (Get_Graph (T2, Has_In))
      /= (  "X2(C => X1(A..B => Classes = E:1:0); "
         &  "D => X1(B => Classes = F:1:0))"
      )  )
   then
      Put (File, Image (Get_Graph (T2, Has_In)));
      Raise_Exception
      (  Failed'Identity,
         "Failed to select the feature X2 in Test 13"
      );
   end if;

   Invalidate (Lesson);

   Put (Lesson, 1, X1,      Has_In, Value ("A", X1));
   Put (Lesson, 1, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 1, Classes, Has_In, Value ("E", Classes));
   Put (Lesson, 2, X1,      Has_In, Value ("B", X1));
   Put (Lesson, 2, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 2, Classes, Has_In, Value ("E", Classes));
   Put (Lesson, 3, X1,      Has_In, Value ("B", X1));
   Put (Lesson, 3, X2,      Has_In, Value ("D", X2));
   Put (Lesson, 3, Classes, Has_In, Value ("F", Classes));
   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   T1 := Learn (Lesson, (X1, X2), Classes);
   Put_Line (File, "<p>Learnt from the examples:</p>");
   Put (File, T1);

   if (  Image (Get_Graph (T1, Has_In))
      /= Image (Get_Graph (T2, Has_In))
      )
   then
      Raise_Exception (Failed'Identity, "Wrong graph in Test 13");
   end if;
end Test_13;
