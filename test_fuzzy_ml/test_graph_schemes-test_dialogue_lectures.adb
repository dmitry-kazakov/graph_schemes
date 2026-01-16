--                                                                    --
--  procedure                       Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Dialogue_Lectures   Luebeck            --
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
--  This is a test procedure for dialogue fuzzy lectures
--
with Ada.Text_IO;                   use Ada.Text_IO;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;

with Fuzzy.Lecture.General.Text_Dialogue;

separate (Test_Graph_Schemes) procedure Test_Dialogue_Lectures is
   Color  : constant Feature_Handle :=
                     Create_Discrete ("Color", "R, G, B");
   Number : constant Feature_Handle :=
                     Create_Float ("X 0..1", 10, 0.0, 1.0);
   Lesson : constant Lecture_Handle :=
                     Fuzzy.Lecture.General.Text_Dialogue.Create;
begin
   Put_Line ("You typed " & Image (Color,  Get (Lesson, 1, Color)));
   Put_Line ("You typed " & Image (Number, Get (Lesson, 1, Number)));
end Test_Dialogue_Lectures;
