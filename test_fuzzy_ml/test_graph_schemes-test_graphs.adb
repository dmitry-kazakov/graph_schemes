--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Graphs              Luebeck            --
--  Separate body implementation                   Spring, 2002       --
--                                                                    --
--                                Last revision :  13:11 20 Apr 2010  --
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
--
--  This is a test procedure for fuzzy graphs.
--
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;
with Fuzzy;                         use Fuzzy;
with Fuzzy.Classifier.Handle;       use Fuzzy.Classifier.Handle;
with Fuzzy.Feature;                 use Fuzzy.Feature;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Feature.Handle.HTML;     use Fuzzy.Feature.Handle.HTML;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Factory;  use Fuzzy.Lecture.Handle.Factory;
with Fuzzy.Lecture.Handle.HTML;     use Fuzzy.Lecture.Handle.HTML;
with Fuzzy.Graph.Handle;            use Fuzzy.Graph.Handle;
with Fuzzy.Graph.Handle.Edit;       use Fuzzy.Graph.Handle.Edit;
with Fuzzy.Classifier.Handle.HTML;  use Fuzzy.Classifier.Handle.HTML;
with Strings_Edit.Integers;         use Strings_Edit.Integers;
with Test_Tone_Feature;             use Test_Tone_Feature;
with Test_Day_Of_Week_Feature;      use Test_Day_Of_Week_Feature;

with Fuzzy.Intuitionistic;
with Fuzzy.Classifier.Handle.Factory;
use  Fuzzy.Classifier.Handle.Factory;

separate (Test_Graph_Schemes)
   procedure Test_Graphs (File : in out File_Type) is
   procedure Test_1  is separate;
   procedure Test_2  is separate;
   procedure Test_3  is separate;
   procedure Test_4  is separate;
   procedure Test_5  is separate;
   procedure Test_6  is separate;
   procedure Test_7  is separate;
   procedure Test_8  is separate;
   procedure Test_9  is separate;
   procedure Test_10 is separate;
   procedure Test_11 is separate;
   procedure Test_12 is separate;
   procedure Test_13 is separate;
   procedure Test_14 is separate;
   procedure Test_15 is separate;
   procedure Test_16 is separate;
begin
   Test_1;
   Test_2;
   Test_3;
   Test_4;
   Test_5;
   Test_6;
   Test_7;
   Test_8;
   Test_9;
   Test_10;
   Test_11;
   Test_12;
   Test_13;
   Test_14;
   Test_15;
   Test_16;
end Test_Graphs;
