--                                                                    --
--  procedure Test_Graphs.Test_15   Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
--                                                 Autumn, 2006       --
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

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_15 is
   Lesson  : Lecture_Handle;
   T1      : Classifier_Handle;
   X1      : constant Feature_Handle := Create_Discrete ("X1", "A, B");
   Classes : constant Feature_Handle :=
                Create_Discrete ("Classes", "C1, C2");
   X2      : constant Feature_Handle :=
                Create_Clustering ("Frequency", Identity (Classes));
begin
   Put_Line (File, "<hr><p><b>Test 15</b></p>");
   Put (Lesson, 1, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 1, Classes, Has_In, Value ("C1", Classes));
   Put (Lesson, 2, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 2, Classes, Has_In, Value ("C2", Classes));
   Put (Lesson, 3, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 3, Classes, Has_In, Value ("C1", Classes));
   Put (Lesson, 4, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 4, Classes, Has_In, Value ("C2", Classes));
   Put (Lesson, 5, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 5, Classes, Has_In, Value ("C2", Classes));
   Put (Lesson, 6, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 6, Classes, Has_In, Value ("C2", Classes));
   Put (Lesson, 7, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 7, Classes, Has_In, Value ("C2", Classes));
   Put (Lesson, 8, X1,      Has_In, Value ("A",  X1));
   Put (Lesson, 8, Classes, Has_In, Value ("C2", Classes));   
   Put (Lesson, 9, X1,      Has_In, Value ("B",  X1));
   Put (Lesson, 9, Classes, Has_In, Value ("C1", Classes));

   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   Put_Line (File, "<p>Learnt from the examples:</p>");
   T1 := Learn (Lesson, (X2, X1), Classes);   
   Put (File, T1);

   if (  Image (Get_Graph (T1, Has_In))
      /= (  "X1(A => Frequency(C1:0.251 => Classes = C1:1:0; "
         &  "C2 => Classes = C2:1:0); "
         &  "B => Frequency(C1 => Classes = C1:1:0))"
      )  )
   then
      Raise_Exception
      (  Failed'Identity,
         "Wrong graph generated in Test 15"
      );
   end if;
   declare
      Features : Table;
   begin
      Add (Features, "X1", X1);
      Add (Features, "Frequency", X2);
      Add (Features, "Classes", Classes);
      if (  Image (Get_Graph (T1, Has_In))
         /= Image
            (  Value
               (  Image (Get_Graph (T1, Has_In)),
                  Features
         )  )  )
      then
         Put_Line (File, "Offending images:<BR>");
         Put_Line (File, Image (Get_Graph (T1, Has_In)) & "<BR>");
         Put_Line
         (  File,
            (  Image
               (  Value
                  (  Image (Get_Graph (T1, Has_In)),
                     Features
               )  )
            &  "<BR>"
         )  );
         Raise_Exception
         (  Failed'Identity,
            "Image->Value->Image failed in Test 15"
         );
      end if;
   end;
end Test_15;
