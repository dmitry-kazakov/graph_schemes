--                                                                    --
--  procedure Test_Graphs.Test_16   Copyright (c)  Dmitry A. Kazakov  --
--  Separate body implementation                   Luebeck            --
--                                                 Autumn, 2010       --
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

with Confidence_Factors;           use Confidence_Factors;
with Fuzzy.Graph.Handle.HTML;      use Fuzzy.Graph.Handle.HTML;
with Fuzzy.Graph.Scheme;           use Fuzzy.Graph.Scheme;
with Fuzzy.Feature.Handle.Tables;  use Fuzzy.Feature.Handle.Tables;

separate (Test_Graph_Schemes.Test_Graphs) procedure Test_16 is
   Solver   : Classifier_Handle;
   Lesson   : Lecture_Handle;
   Height   : constant Feature_Handle :=
                 Create_Linguistic
                 (  "Height",
                    (  "very short (->5->5.3:0),"
                    &  "short (4:0->5->5.3->5.7:0),"
                    &  "normal (5:0->5.3->6->6.3:0),"
                    &  "tall (5.7:0->6->),"
                    &  "very tall (6:0->6.6->)"
                    ),
                    "foot"
                 );
   Weight   : constant Feature_Handle :=
                 Create_Isosceles_Trapezoids
                 (  "Weight",
                    4,
                    124.0,
                    203.0,
                    3.0,
                    "lb"
                 );
   Physique : constant Feature_Handle :=
                 Create_Discrete
                 (  "Physique",
                    "Underweight, Healthy weight, Overweight, Obese"
                 );
begin
   Put_Line (File, "<hr><p><b>Test 16</b></p>");

   Put (Lesson, 1, Height,   Value ("4.5",            Height));
   Put (Lesson, 1, Weight,   Value ("250..300",       Weight));
   Put (Lesson, 1, Physique, Value ("Obese",          Physique));

   Put (Lesson, 2, Height,   Value ("1.8 m",          Height));
   Put (Lesson, 2, Weight,   Value ("50 kg",          Weight));
   Put (Lesson, 2, Physique, Value ("Underweight",    Physique));

   Put (Lesson, 3, Height,   Value ("1.9 m",          Height));
   Put (Lesson, 3, Weight,   Value ("170 kg",         Weight));
   Put (Lesson, 3, Physique, Value ("Healthy weight", Physique));

   Put (Lesson, 4, Height,   Value ("1.5..1.8 m",     Height));
   Put (Lesson, 4, Weight,   Value ("240..350",       Weight));
   Put (Lesson, 4, Physique, Value ("Obese",          Physique));

   Put (Lesson, 5, Height,   Value ("1.6 m",          Height));
   Put (Lesson, 5, Weight,   Value ("50..65 kg",      Weight));
   Put (Lesson, 5, Physique, Value ("Healthy weight", Physique));

   Put (Lesson, 6, Height,   Value ("1.7..1.9 m",     Height));
   Put (Lesson, 6, Weight,   Value ("70 kg",          Weight));
   Put (Lesson, 6, Physique, Value ("Healthy weight", Physique));

   Put (Lesson, 7, Height,   Value ("1.7 m",          Height));
   Put (Lesson, 7, Weight,   Value ("90 kg",          Weight));
   Put (Lesson, 7, Physique, Value ("Overweight",     Physique));

   Put_Line (File, "<p>The training set:</p>");
   Put (File, Lesson);

   Put_Line (File, "<p>Learnt from the examples:</p>");
   Solver := Learn (Lesson, (Height, Weight), Physique);
   Put (File, Solver);

   declare
      Space : Lecture_Handle;
      No    : Positive := 1;
      H     : Fuzzy.Intuitionistic.Classification :=
              (  Cardinality => Get_Cardinality (Height),
                 Possibility => (others => Confidence'First),
                 Necessity   => (others => Confidence'First)
              );
      W     : Fuzzy.Intuitionistic.Classification :=
              (  Cardinality => Get_Cardinality (Weight),
                 Possibility => (others => Confidence'First),
                 Necessity   => (others => Confidence'First)
              );
   begin
      for I in 1..Get_Cardinality (Height) loop
         H.Possibility (I) := Confidence'Last;
         H.Necessity   (I) := Confidence'Last;
         for J in 1..Get_Cardinality (Weight) loop
            W.Possibility (J) := Confidence'Last;
            W.Necessity   (J) := Confidence'Last;
            Put (Space, No, Height, H);
            Put (Space, No, Weight, W);
            Put (Space, No, Physique, Classify (Solver, Space, No));
            No := No + 1;
            W.Possibility (J) := Confidence'First;
            W.Necessity   (J) := Confidence'First;
         end loop;
         H.Possibility (I) := Confidence'First;
         H.Necessity   (I) := Confidence'First;
      end loop;
      Put_Line
      (  File,
         "<p>Classification of the feature space points:</p>"
      );
      Put (File, Space);
   end;

end Test_16;
