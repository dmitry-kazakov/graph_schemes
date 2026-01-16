--                                                                    --
--  procedure Test_FCL_Compiler     Copyright (c)  Dmitry A. Kazakov  --
--  Test                                           Luebeck            --
--                                                 Winter, 2005       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;                use Ada.Text_IO;
with Fuzzy.Lecture.Handle;       use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Handle.HTML;  use Fuzzy.Lecture.Handle.HTML;
with Parsers.FCL.Compiler;       use Parsers.FCL.Compiler;
with Parsers.Multiline_Source;   use Parsers.Multiline_Source;

with Fuzzy.Feature.HTML;
with Fuzzy.Feature.Handle.HTML;
with Parsers.Multiline_Source.Text_IO;
with Units;

procedure Test_FCL_Compiler is
   use Parsers.FCL.Compiler.Rules_Lists;
   File   : aliased File_Type;
   Result : Program;
   Parameters : Fuzzy.Feature.HTML.HTML_Parameters :=
                   Fuzzy.Feature.Handle.HTML.HTML_Defaults;
begin
   Parameters.Mode := Units.Latin1_Set;
   Open (File, In_File, "test_fcl_compiler.fcl");
   declare
      Code : aliased Parsers.Multiline_Source.Text_IO.Source
                       (File'Access);
   begin
      Compile (Result, Code);
   end;
   Close (File);
   Create (File, Out_File, "test.htm");
   for Index in 1..GetSize (Result.Rules) loop
      Put_Line
      (  File,
         (  "<p>"
         &  GetName (Result.Rules, Index)
         &  " declared at "
         &  Image (GetTag (Result.Rules, Index).Location)
         &  "</p>"
      )  );
      Put
      (  File       => File,
         Lesson     => GetTag (Result.Rules, Index).Lesson,
         Parameters => Parameters
      );
   end loop;
   Close (File);
exception
   when Error : others =>
      Put ("Error :");
      Put_Line (Exception_Information (Error));
end Test_FCL_Compiler;
