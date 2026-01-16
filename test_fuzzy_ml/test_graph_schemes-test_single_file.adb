--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_Single_File         Luebeck            --
--  Separate body implementation                   Autumn, 2014       --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;           use Ada.Exceptions;
with Fuzzy.Persistence;        use Fuzzy.Persistence;
with Persistent.Handle;        use Persistent.Handle;
with Persistent.Single_File;   use Persistent.Single_File;

--with Persistent.Single_File.Text_IO;
--with GNAT.Exception_Traces;
with ada.Text_IO;
separate (Test_Graph_Schemes)
   procedure Test_Single_File (File : in out File_Type) is

   procedure Dir
             (  File    : File_Type;
                Storage : Storage_Handle
             )  is
      use Persistent.Catalogue;
      List : constant Persistent.Catalogue.Set := Get_List (Storage);
   begin
      Put_Line (File, "Persistent storage catalogue:<BR>");
      for Item in 1..Get_Size (List) loop
         Put_Line (File, "<li>" & Get (List, Item) & "</li>");
      end loop;
   end Dir;

   procedure Test_1 is separate;
   procedure Test_2 is separate;
begin
   Test_1;
-- GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   Test_2;
end Test_Single_File;
