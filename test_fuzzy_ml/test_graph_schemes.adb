--                                                                    --
--  procedure Test_Graph_Schemes    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
--                                                                    --
--                                Last revision :  10:08 22 Nov 2014  --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Text_IO;           use Ada.Text_IO;
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Handle;    use Fuzzy.Graph.Handle;
with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;

procedure Test_Graph_Schemes is
   Failed : exception;

   procedure Test_Dialogue_Lectures is separate;
   procedure Test_Binary            is separate;
   procedure Test_Indicators        is separate;
   procedure Test_Lectures    (File : in out File_Type) is separate;
   procedure Test_Graphs      (File : in out File_Type) is separate;
   procedure Test_ODBC        (File : in out File_Type) is separate;
   procedure Test_Single_File (File : in out File_Type) is separate;
   procedure Test_SQLite      (File : in out File_Type) is separate;

   File : File_Type;
begin
   Create (File, Out_File, "test.htm");
   Put_Line (File, "<body>");
   Put_Line ("Testing lectures ...");
   Test_Lectures (File);
   Put_Line ("Testing Single Files ...");
   Test_Single_File (File);
   Put_Line ("Testing SQLite ...");
   Test_SQLite (File);
   Put_Line ("Testing ODBC ...");
   Test_ODBC (File);
   Put_Line ("Testing dialogue lectures ...");
   Test_Dialogue_Lectures;
   Put_Line ("Testing derived binary features ...");
   Test_Binary;
   Put_Line ("Testing indicators ...");
   Test_Indicators;
   Put_Line ("Testing graphs ...");
   Test_Graphs (File);
--   Test_APQ    (File);
   if Fuzzy.Graph.Get_Number_Of_Nodes /= 0 then
      Raise_Exception (Failed'Identity, "Nodes leaking");
   end if;
   if Fuzzy.Feature.Get_Number_Of_Features /= 0 then
      Raise_Exception (Failed'Identity, "Features leaking");
   end if;
   if Get_Number_Of_Lectures /= 1 then
      Raise_Exception (Failed'Identity, "Lectures leaking");
   end if;
   Put_Line ("It seems to be OK");
   Put_Line ("   You might take a look at test.htm generated");
   Put_Line ("   in the current directory.");
   Put_Line (File, "</body>");
   Close (File);
exception
   when Error : Failed =>
      Put_Line (File, "</body>");
      Close (File);
      Put ("Check error: ");
      Put_Line (Exception_Information (Error));
   when Error : others =>
      Put_Line (File, "</body>");
      Close (File);
      Put ("Fatal error: ");
      Put_Line (Exception_Information (Error));
end Test_Graph_Schemes;
