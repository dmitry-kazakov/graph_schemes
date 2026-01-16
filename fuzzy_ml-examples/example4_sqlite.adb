--                                                                    --
--  procedure Example4_SQLite       Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2010       --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of the GNU General Public  License  as  --
--  published by the Free Software Foundation; either version  2  of  --
--  the License, or (at your option) any later version. This library  --
--  is distributed in the hope that it will be useful,  but  WITHOUT  --
--  ANY   WARRANTY;   without   even   the   implied   warranty   of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  General  Public  License  for  more  details.  You  should  have  --
--  received  a  copy  of  the GNU General Public License along with  --
--  this library; if not, write to  the  Free  Software  Foundation,  --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                    --
--  As a special exception, if other files instantiate generics from  --
--  this unit, or you link this unit with other files to produce  an  --
--  executable, this unit does not by  itself  cause  the  resulting  --
--  executable to be covered by the GNU General Public License. This  --
--  exception  does not however invalidate any other reasons why the  --
--  executable file might be covered by the GNU Public License.       --
--____________________________________________________________________--

with Ada.Characters.Handling;       use Ada.Characters.Handling;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Text_IO;                   use Ada.Text_IO;

with Confidence_Factors.Edit;       use Confidence_Factors.Edit;
with Fuzzy.Classifier.Handle;       use Fuzzy.Classifier.Handle;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Intuitionistic;          use Fuzzy.Intuitionistic;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.Bulk;     use Fuzzy.Lecture.Handle.Bulk;
with Fuzzy.Lecture.Handle.Text_IO;  use Fuzzy.Lecture.Handle.Text_IO;
with Fuzzy.Persistence;             use Fuzzy.Persistence;
with Indicator.Text_IO;             use Indicator.Text_IO;
with Integer_Intervals;             use Integer_Intervals;
with Persistent.Handle;             use Persistent.Handle;
with Persistent.SQLite;             use Persistent.SQLite;
with Persistent.SQLite.Lectures;    use Persistent.SQLite.Lectures;
with Strings_Edit.UTF8.Maps;        use Strings_Edit.UTF8.Maps;
with Name_Tables;

with Fuzzy.Classifier.Handle.Factory;
use  Fuzzy.Classifier.Handle.Factory;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

procedure Example4_SQLite is
begin
   --
   -- First  we  should  make  minus  ('-')  legal for names of discrete
   -- feature  values,  because  the  teaching   set   contains   values
   -- containing minus (like in 'mod-stable').
   --
   Name_Tables.Name_Body := Name_Tables.Name_Body or To_Set ("-");
   ---------------------------------------------------------------------
   -- The  first session with the data base. Here we create the training
   -- set, classes and the features we will use.
   --
   declare
      Data_Base    : Storage_Handle;
      Training_Set : Lecture_Handle;
      --
      -- Features is the list of all features the teaching set  defines.
      -- The last feature is the classes (the class-feature).
      --
      Features : Feature_Array :=
      (  Create_Discrete ("L-CORE", "high, mid, low"),
         Create_Discrete ("L-SURF", "high, mid, low"),
         Create_Discrete ("L-O2", "excellent, good, fair, poor"),
         Create_Discrete ("L-BP", "high, mid, low"),
         Create_Discrete ("SURF-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("CORE-STBL", "stable, mod-stable, unstable"),
         Create_Discrete ("BP-STBL", "stable, mod-stable, unstable"),
         Create_Float    ("COMFORT", 5, 0.0, 20.0),
         Create_Discrete ("ADM-DECS", "I, S, A")
      );
   begin
      Put_Line ("Creating a SQLite data base in sqlite.db");
      Data_Base := Create ("sqlite.db", True);
      Put ("Session one. Creating the training set ");
      --
      -- Storing  all features into the data base. The name of a feature
      -- in the data base will be identical to the feature name.
      --
      for Index in Features'Range loop
         Put
         (  Data_Base,
            Features (Index),
            Get_Name (Features (Index))
         );
      end loop;
      --
      -- Creating a training set in the data base
      --
      Training_Set := Create (Data_Base, "Training set");
      --
      -- Reading training examples from a file into the set
      --
      declare
         Suffix      : aliased String := " examples read";
         Progress    : aliased Counter (Suffix'Access);
         Transaction : Lecture_Update (Ptr (Training_Set));
      begin
         Read
         (  File_Name => "example4.txt",
            Lesson    => Training_Set,
            Features  => Features,
            Viewer    => Progress'Access
         );
         New_Line;
      end;
   end;
   ---------------------------------------------------------------------
   -- The  second  session.  Here we use the training set for creating a
   -- graph-scheme. Then the graph-scheme is also  stored  in  the  data
   -- base.
   --
   declare
      Data_Base    : Storage_Handle;
      Training_Set : Lecture_Handle;
      Classifier   : Classifier_Handle;
      Features     : Feature_Array (1..8);
      Classes      : Feature_Handle;
   begin
      Data_Base := Create ("sqlite.db");
      Put ("Session two. Learning ");
      --
      -- Getting a handle to the training set in the data base
      --
      Training_Set := Get (Data_Base, "Training set");
      --
      -- Getting the features we want to use for learning
      --
      Features (1) := Get (Data_Base, "L-CORE");
      Features (2) := Get (Data_Base, "L-SURF");
      Features (3) := Get (Data_Base, "L-O2");
      Features (4) := Get (Data_Base, "L-BP");
      Features (5) := Get (Data_Base, "SURF-STBL");
      Features (6) := Get (Data_Base, "CORE-STBL");
      Features (7) := Get (Data_Base, "BP-STBL");
      Features (8) := Get (Data_Base, "COMFORT");
      --
      -- Get the class-feature
      --
      Classes := Get (Data_Base, "ADM-DECS");
      --
      -- Learn from the first 3/4 of the reaching  set.  The  result  of
      -- training, a classifier, is stored into the data base.
      --
      declare
         Progress : aliased Timed_Bar (20);
      begin
         Classifier :=
            Learn
            (  Lesson   => Training_Set,
               Features => Features,
               Classes  => Classes,
               To       => (Get_Examples_Number (Training_Set) * 3) / 4,
               Viewer   => Progress'Access
            );
         New_Line;
      end;
      Classifier := Separator (Classifier);
      Put (Data_Base, Classifier, "Classifier1");
      --
      -- Here we create another  classifier  by  splitting  the  feature
      -- COMFORT  into  several binary features and distributing them in
      -- the  feature  list.  The  obtained  classifier  is stored under
      -- another name.
      --
      declare
         Binary   : constant Bounded_Array :=
                             Create_Binary (Features (8));
         Progress : aliased Timed_Bar (20);
      begin
         Put ("Learning on binary features ");
         Classifier :=
            Learn
            (  Lesson   => Training_Set,
               Features =>
               (  Ref (Binary, 1), -- The MSB of comfort
                  Ref (Binary, 2), -- The second bit of comfort
                  Ref (Binary, 3), -- The last bit of comfort
                  Features (1),    -- Internal temperature
                  Features (2),    -- Surface temperature
                  Features (3),    -- Oxygen saturation
                  Features (4),    -- Last blood pressure
                  Features (5),    -- Stability of surface temperature
                  Features (6),    -- Stability of core temperature
                  Features (7)     -- Stability of blood pressure
               ),
               Classes => Classes,
               To      => (Get_Examples_Number (Training_Set) * 3) / 4,
               Viewer  => Progress'Access
            );
         New_Line;
         Classifier := Separator (Classifier);
         Put (Data_Base, Classifier, "Classifier2");
      end;
   end;
   ---------------------------------------------------------------------
   -- The third session. Here we use the classifier and the rest of  the
   -- training set to verify the quality of learning.
   --
   declare
      Data_Base    : Storage_Handle;
      Training_Set : Lecture_Handle;
      Classifier   : Classifier_Handle;
      Result       : Fuzzy.Intuitionistic.Set (3);
   begin
      Data_Base := Create ("sqlite.db");
      Put_Line ("Session three. Testing the classifier");
      --
      -- Getting a handle to the training set and the classifier  stored
      -- in the data base
      --
      Training_Set := Get (Data_Base, "Training set");
      Classifier   := Get (Data_Base, "Classifier1");
      --
      -- Classify the rest 1/4 of the training set
      --
      Result :=
         Verify
         (  Classifier,
            Training_Set,
            From => (Get_Examples_Number (Training_Set) * 3) / 4 + 1
         );
      Put_Line ("The classification error means:");
      for Index in 1..Result.Cardinality loop
         Put_Line
         (  Image (Get_Classes (Classifier), Interval'(Index, Index))
         &  ": ["
         &  Image (Result.Possibility (Index))
         &  ".."
         &  Image (Result.Necessity (Index))
         &  "]"
         );
      end loop;
      --
      -- Here we repeat that for the second classifier
      --
      Classifier := Get (Data_Base, "Classifier2");
      Result :=
         Verify
         (  Classifier,
            Training_Set,
            From => (Get_Examples_Number (Training_Set) * 3) / 4 + 1
         );
      Put_Line ("The error means with binary features:");
      for Index in 1..Result.Cardinality loop
         Put_Line
         (  Image (Get_Classes (Classifier), Interval'(Index, Index))
         &  ": ["
         &  Image (Result.Possibility (Index))
         &  ".."
         &  Image (Result.Necessity (Index))
         &  "]"
         );
      end loop;
   end;
exception
   when Error : others =>
      Put_Line (Exception_Information (Error));
end Example4_SQLite;
