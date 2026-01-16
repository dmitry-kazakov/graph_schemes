--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_SQLite.Test_1       Luebeck            --
--  Separate body implementation                   Autumn, 2010       --
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
--
--  This is a test procedure for fuzzy lectures
--
with Confidence_Factors;            use Confidence_Factors;
with Fuzzy;                         use Fuzzy;
with Fuzzy.Feature;                 use Fuzzy.Feature;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle.Factory;  use Fuzzy.Lecture.Handle.Factory;

separate (Test_Graph_Schemes.Test_SQLite) procedure Test_1 is
   Storage : constant Storage_Handle := Create ("test.db", True);
   Domain  : constant String :=
      "Red, Green, Blue, Yellow, Pink, Cyan, Brown, Black, White";
   Lesson  : Lecture_Handle := Create_Persistent (Storage, "test");
   X1      : constant Feature_Handle := Create_Discrete ("x1", Domain);
   X2      : constant Feature_Handle := Create_Discrete ("x2", Domain);
   X3      : constant Feature_Handle := Create_Discrete ("x3", Domain);
   function Check (X : Fuzzy.Set; E : Natural) return Boolean is
      R : Natural := E;
   begin
      for Index in X'Range loop
         if X (Index) = Confidence'Last then
            if 0 = R rem 2 then
               return True;
            end if;
         else
            if 0 /= R rem 2 then
               return True;
            end if;
         end if;
         R := R / 2;
      end loop;
      return False;
   end Check;
begin
   Put_Line (File, "<hr><p><b>SQLite Test 1 (lectures)</b></p>");
   Dir (File, Storage);
   if Is_Defined (Lesson, 1, X1, Has_In) then
      Raise_Exception
      (  Failed'Identity,
         "Error in empty set"
      );
   end if;
   --
   -- Possibility (1,X1) = 1
   --
   Put (Lesson, 1, X1, Has_In, 1);
   if not Is_Defined (Lesson, 1, X1, Has_In) then
      Raise_Exception
      (  Failed'Identity,
         "Error in Is_Possibility_Defined"
      );
   end if;
   if Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#) then
      Raise_Exception
      (  Constraint_Error'Identity,
         "Error in Get_Possibility [set]"
      );
   end if;
   if (  Possibility
         (  Fuzzy.Set'
            (  2..9  => Confidence'Last,
               1     => Confidence'First
            ),
            Get (Lesson, 1, X1, Has_In)
         )
      /= Confidence'First
      )
   then
      Raise_Exception
      (  Failed'Identity,
         "Error in Possibility (Get_Possibility)"
      );
   end if;
   if Check (Get (Lesson, 1, X2, Has_In), 2#1_1111_1111#) then
      Raise_Exception (Failed'Identity, "Error in Get_Possibility");
   end if;
   if (  Check (not Get (Lesson, 1, X1, Has_Out), 2#0_0000_0000#)
      or Check (not Get (Lesson, 2, X1, Has_Out), 2#0_0000_0000#)
      or Check (not Get (Lesson, 1, X2, Has_Out), 2#0_0000_0000#)
      or Check (not Get (Lesson, 2, X2, Has_Out), 2#0_0000_0000#)
      )
   then
      Raise_Exception (Failed'Identity, "Error in Get_Necessity");
   end if;
   --
   -- Possibility (2,X2) = 2
   --
   Put (Lesson, 2, X2, Has_In, 2);
   if (  Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#)
      or (  Possibility
            (  Fuzzy.Set'
               (  2..9 => Confidence'Last,
                  1    => Confidence'First
               ),
               Get (Lesson, 1, X1, Has_In)
            )
         /= Confidence'First
         )
      or Check (Get (Lesson, 1, X2, Has_In), 2#1_1111_1111#)
      or Check (Get (Lesson, 2, X2, Has_In), 2#0_0000_0010#)
      )
   then
      Raise_Exception (Failed'Identity, "Error in Get_Possibility");
   end if;
   --
   -- Possibility (3,X3) = 3
   --
   Put (Lesson, 3, X3, Has_In, 3);
   if (  Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#)
      or (  Possibility
            (  Fuzzy.Set'
               (  2..9 => Confidence'Last,
                  1    => Confidence'First
               ),
               Get (Lesson, 1, X1, Has_In)
            )
         /= Confidence'First
         )
      or Check (Get (Lesson, 1, X2, Has_In), 2#1_1111_1111#)
      or Check (Get (Lesson, 2, X2, Has_In), 2#0_0000_0010#)
      or Check (Get (Lesson, 3, X3, Has_In), 2#0_0000_0100#)
      )
   then
      Raise_Exception (Failed'Identity, "Error in Get_Possibility");
   end if;
end Test_1;
