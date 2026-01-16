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

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_14 is
   Lesson  : Lecture_Handle;
   T1      : Classifier_Handle;
   X1      : constant Feature_Handle := Create_Discrete ("X1", "A, B");
   X2      : constant Feature_Handle := Create_Discrete ("X2", "C, D");
   X3      : constant Feature_Handle := Create_Discrete ("X3", "E, F");
   Classes : constant Feature_Handle :=
                      Create_Discrete ("Classes", "G, H");
begin
   Put_Line (File, "<hr><p><b>Test 14</b></p>");
   Put (Lesson, 1, X1,      Has_In, Value ("A", X1));
   Put (Lesson, 1, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 1, X3,      Has_In, Value ("E", X3));
   Put (Lesson, 1, Classes, Has_In, Value ("G", Classes));
   Put (Lesson, 2, X1,      Has_In, Value ("B", X1));
   Put (Lesson, 2, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 2, X3,      Has_In, Value ("E", X3));
   Put (Lesson, 2, Classes, Has_In, Value ("G", Classes));
   Put (Lesson, 3, X1,      Has_In, Value ("A", X1));
   Put (Lesson, 3, X2,      Has_In, Value ("C", X2));
   Put (Lesson, 3, X3,      Has_In, Value ("F", X3));
   Put (Lesson, 3, Classes, Has_In, Value ("H", Classes));
   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   Put_Line (File, "<p>Learnt from the examples:</p>");
   T1 := Learn (Lesson, (X1, X2, X3), Classes);
   Put (File, T1);

   if (  Image (Get_Graph (T1, Has_In))
      /= (  "X3(E => X1(A..B => X2(C => Classes = G:1:0)); "
         &  "F => X1(A => X2(C => Classes = H:1:0)))"
      )  )
   then
      Raise_Exception
      (  Failed'Identity,
         "Wrong graph generated in Test 14"
      );
   end if;
end Test_14;
