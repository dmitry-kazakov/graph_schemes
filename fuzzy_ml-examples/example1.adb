--                                                                    --
--  procedure Example1              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2002       --
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

with Ada.Text_IO;                   use Ada.Text_IO;
with Fuzzy.Feature.Domain_Floats;   use Fuzzy.Feature.Domain_Floats;
with Fuzzy.Feature.Handle;          use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;     use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Feature.Handle.Factory;  use Fuzzy.Feature.Handle.Factory;
with Fuzzy.Lecture.Handle;          use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.HTML;     use Fuzzy.Lecture.Handle.HTML;
with Fuzzy.Floats.Edit;          -- I/O for fuzzy numbers
with Fuzzy.Feature.Domain_Float_Handle;

procedure Example1 is
   package Fuzzy_Edit is new Fuzzy_Floats.Edit (Float_Edit);
   use Fuzzy_Edit;
   use Fuzzy_Measures;
   use Fuzzy.Feature.Domain_Float_Handle;
   use Irregular_Measures;

   File   : File_Type;
   Lesson : Lecture_Handle;
   Color  : constant Feature_Handle :=
               Create_Discrete ("Color", "Red, Green, Blue");
   Height : constant Feature_Handle :=
               Create_Float ("Height", 10, 120.0, 200.0, "foot");
begin
   --
   -- Create an output file
   --
   Create (File, Out_File, "Example1.htm");
   --
   -- Set  the  first  example,  for  the  feature  color.  The value is
   -- specified as a string which is converted to a classification using
   -- Fuzzy.Feature.Handle.Edit.Value.
   --
   Put (Lesson, 1, Color, Value ("Red:1:1, Green:0.5:0.2", Color));
   --
   -- Set  the  first  example, for the feature height. The value of the
   -- feature  is converted from a string to a fuzzy float and then to a
   -- classification of the feature.
   --
   Put
   (  Lesson,
      1,
      Height,
      Classify
      (  Height,
         Value ("152..180:0.2,159..175:0.5,165..170:0.8,166..167:1")*ft
   )  );
   --
   -- Set the second example, for the feature height. Here we again
   -- use Value from Fuzzy.Feature.Handle.Edit.
   --
   Put (Lesson, 2, Height, Value ("47..50m:1:1", Height));
   --
   -- Write the training set into the file
   --
   Put (File, Lesson);
   --
   -- Close the file
   --
   Close (File);
end Example1;
