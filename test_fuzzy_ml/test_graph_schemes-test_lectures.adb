--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Lectures            Luebeck            --
--  Separate body implementation                   Spring, 2002       --
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
with Ada.Exceptions;                use Ada.Exceptions;
with Confidence_Factors;            use Confidence_Factors;
with Fuzzy;                         use Fuzzy;
with Fuzzy.Feature;                 use Fuzzy.Feature;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.HTML;     use Fuzzy.Lecture.Handle.HTML;

with Fuzzy.Lecture.General;
with Test_Caching_Lecture;

separate (Test_Graph_Schemes)
   procedure Test_Lectures (File : in out File_Type) is
   Domain : constant String :=
      "Red, Green, Blue, Yellow, Pink, Cyan, Brown, Black, White";

   function Check (X : Set; E : Natural) return Boolean is
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
   Put_Line (File, "<hr><p><b>Test memory resident lectures</b></p>");
   declare
      Lesson : Lecture_Handle := Fuzzy.Lecture.General.Create;
      X1 : constant Feature_Handle := Create_Discrete ("x1", Domain);
      X2 : Feature_Handle := Create_Discrete ("x2", Domain);
      X3 : constant Feature_Handle := Create_Discrete ("x3", Domain);
   begin
      if Is_Defined (Lesson, 1, X1, Has_In) then
         Raise_Exception (Failed'Identity, "Error in empty set");
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
         (  Failed'Identity,
            "Error in Get_Possibility [set]"
         );
      end if;
      if (  Possibility
            (  Set'
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
               (  Set'
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
               (  Set'
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
      Put (File, Lesson);
      Put_Line (File, "<p>Deleting the second feature:</p>");
      Delete (X2);
      Put (File, Lesson);
   end;
   Put_Line (File, "<hr><p><b>Test caching lectures</b></p>");
   declare
      use Test_Caching_Lecture;
      Lesson : Lecture_Handle := Create (2);
      X1 : constant Feature_Handle := Create_Discrete ("x1", Domain);
      X2 : Feature_Handle := Create_Discrete ("x2", Domain);
      X3 : constant Feature_Handle := Create_Discrete ("x3", Domain);
   begin
      if Is_Defined (Lesson, 1, X1, Has_In) then
         Raise_Exception (Failed'Identity, "Error in empty set");
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
         (  Failed'Identity,
            "Error in Get_Possibility [set]"
         );
      end if;
      if (  Possibility
            (  Set'
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
      if Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#) then
         Raise_Exception
         (  Failed'Identity,
            "Error in Get_Possibility [get]"
         );
      end if;
      if Check (not Get (Lesson, 1, X1, Has_Out), 2#0_0000_0000#) then
         Raise_Exception (Failed'Identity, "Error in Get_Necessity");
      end if;
      if Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#) then
         Raise_Exception
         (  Failed'Identity,
            "Error in Get_Possibility [read]"
         );
      end if;
      if (  Check (not Get (Lesson, 2, X1, Has_Out), 2#0_0000_0000#)
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
      if Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#) then
         Raise_Exception (Failed'Identity, "Error in Get_Possibility");
      end if;
      if (  (  Possibility
               (  Set'
                  (  2..9 => Confidence'Last,
                     1    => Confidence'First
                  ),
                  Get (Lesson, 1, X1, Has_In)
               )
            /= Confidence'First
            )
         or Check (Get (Lesson, 1, X2, Has_In), 2#1_1111_1111#)
         or Check (Get (Lesson, 2, X2, Has_In), 2#0_0000_0010#
         )  )
      then
         Raise_Exception (Failed'Identity, "Error in Get_Possibility");
      end if;
      --
      -- Possibility (3,X3) = 3
      --
      Put (Lesson, 3, X3, Has_In, 3);
      if (  Check (Get (Lesson, 1, X1, Has_In), 2#0_0000_0001#)
         or (  Possibility
               (  Set'
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
      Put (File, Lesson);
      Put_Line (File, "<p>Deleting the second feature:</p>");
      Delete (X2);
      Put (File, Lesson);
      Write (Lesson);
   exception
      when others =>
         Drop (Lesson);
         raise;
   end;
end Test_Lectures;
