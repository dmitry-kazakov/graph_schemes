--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_SQLite.Test_2       Luebeck            --
--  Separate body implementation                   Aututmn, 2010      --
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

with Fuzzy.Feature.Handle.Edit;      use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;   use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle.HTML;      use Fuzzy.Lecture.Handle.HTML;
with Fuzzy.Feature.Domain_Integers;  use Fuzzy.Feature.Domain_Integers;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

separate (Test_Graph_Schemes.Test_SQLite) procedure Test_2 is
   Name    : constant String := "test";
   Storage : Storage_Handle;
   Lesson  : Lecture_Handle;
begin
   Put_Line (File, "<hr><p><b>SQLite Test 2 (lectures)</b></p>");
   Storage := Create ("test.db");
   Dir (File, Storage);
   --
   -- Restoring the training set from the data base
   --
   Lesson := Get (Storage, Name);
   Put_Line (File, "<p>The training set " & Name & " restored:</p>");
   Put (File, Lesson);
   --
   -- Deleting a feature from the set
   --
   declare
      X2 : Feature_Handle := Get_Feature (Lesson, 2);
   begin
      Put_Line (File, "<p>Deleting the second feature:</p>");
      Delete (X2);
      Put (File, Lesson);
   end;
   --
   -- Name the 1st feature
   --
   declare
      X1 : Feature_Handle := Get_Feature (Lesson, 1);
   begin
      Rename (Storage, X1, "X1");
      Put_Line (File, "<p>Naming the first feature...</p>");
      Dir (File, Storage);
   end;
   --
   -- Create different features
   --
   declare
      X : Feature_Handle;
   begin
      Put_Line (File, "<p>Creating various features...</p>");
      X := Create_Integer ("Int", -100, 200);
      Put (Storage, X, String'("Small"));
      X := Create_Float ("Speed", 20, 0.0, 250.0);
      Put (Storage, X, String'("Speed km/h"));
      declare
         List : constant Bounded_Array := Create_Binary (X);
      begin
         for Index in List.First..List.Last loop
           X := Ref (List, Index);
           Put (Storage, X, Get_Name (X));
         end loop;
      end;
      X :=
         Create_Discrete
         (  "Day",
            (  "Sunday, Monday, Tuesday, Wednesday, "
            &  "Thursday, Friday, Saturday"
         )  );
      Put (Storage, X, String'("Day of week"));
      X :=
         Create_Linguistic
         (  """Temperature""",
            (  "chill (->-10->-4:0),"
            &  "cold (->11->17:0),"
            &  "tepid (3:0->8..21->28:0),"
            &  "warm (15:0->21->29->35:0),"
            &  "hot (25:0->28->) "
         )  );
      Put (Storage, X, String'("Temperature"));
   end;
   Dir (File, Storage);
   Invalidate (Lesson);
   Invalidate (Storage); -- Close connection
   --
   -- Re-opening and loading the features back
   Storage := Create ("test.db");
   Lesson  := Get (Storage, Name);
   declare
      Small  : Feature_Handle := Get (Storage, String'("Small"));
      Speed  : Feature_Handle := Get (Storage, String'("Speed km/h"));
      Binary : Feature_Handle := Get (Storage, String'("Speed km/h.2"));
      Day    : constant Feature_Handle :=
                        Get (Storage, String'("Day of week"));
      T      : Feature_Handle := Get (Storage, String'("Temperature"));
   begin
      Put (Lesson, 1, Day, Value ("Monday:1:0.5,Tuesday:0.5:0.3", Day));
      Put_Line (File, "<p>The training set:</p>");
      Put (File, Lesson);
      declare
         To_Remove : Feature_Handle :=
            Get (Storage, String'("Speed km/h.5"));
      begin
         Unname (Storage, To_Remove); -- This removes it
      end;
   end;
   Put_Line (File, "<p>The training set is now anonymous:</p>");
   Unname (Storage, Lesson);
   Put (File, Lesson);
   Invalidate (Lesson);
   Put_Line (File, "<p>No more references to the training set</p>");
   Dir (File, Storage);
end Test_2;
