--                                                                    --
--  function                        Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.Factory                Luebeck            --
--  Implementation                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Ada.IO_Exceptions;
--with Persistent.APQ.Lectures;
--with Persistent.ODBC.Lectures;
with Persistent.Native_ODBC.Lectures;
with Persistent.Single_File.Lectures;
with Persistent.SQLite.Lectures;

package body Fuzzy.Lecture.Handle.Factory is

   function Create_Persistent
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Lecture_Handle is
   begin
      if not Is_Valid (Storage) then
         raise Constraint_Error;
--    elsif Persistent.APQ.Is_APQ (Storage) then
--       return Persistent.APQ.Lectures.Create (Storage, Name, Parent);
      elsif Persistent.Single_File.Is_Single_File (Storage) then
         return Persistent.Single_File.Lectures.Create
                (  Storage,
                   Name,
                   Parent
                );
      elsif Persistent.SQLite.Is_SQLite (Storage) then
         return Persistent.SQLite.Lectures.Create
                (  Storage,
                   Name,
                   Parent
                );
      elsif Persistent.Native_ODBC.Is_ODBC (Storage) then
         return Persistent.Native_ODBC.Lectures.Create
                (  Storage,
                   Name,
                   Parent
                );
      else
         raise Ada.IO_Exceptions.Use_Error;
      end if;
   end Create_Persistent;

   function Create_Persistent
            (  Storage : Storage_Handle
            )  return Lecture_Handle is
   begin
      if not Is_Valid (Storage) then
         raise Constraint_Error;
--    elsif Persistent.APQ.Is_APQ (Storage) then
--       return Persistent.APQ.Lectures.Create (Storage);
--    elsif Persistent.ODBC.Is_ODBC (Storage) then
--       return Persistent.ODBC.Lectures.Create (Storage);
      elsif Persistent.Single_File.Is_Single_File (Storage) then
         return Persistent.Single_File.Lectures.Create (Storage);
      elsif Persistent.SQLite.Is_SQLite (Storage) then
         return Persistent.SQLite.Lectures.Create (Storage);
      elsif Persistent.Native_ODBC.Is_ODBC (Storage) then
         return Persistent.Native_ODBC.Lectures.Create (Storage);
      else
         raise Ada.IO_Exceptions.Use_Error;
      end if;
   end Create_Persistent;

end Fuzzy.Lecture.Handle.Factory;

