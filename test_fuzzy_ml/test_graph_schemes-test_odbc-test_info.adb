--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Test_Graph_Schemes.Test_ODBC.Test_Info      Luebeck            --
--  Separate body implementation                   Winter, 2003       --
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

with ODBC.API;                 use ODBC.API;
with ODBC.SQLTypes;            use ODBC.SQLTypes;
with HTML;                     use HTML;
with Persistent.Native_ODBC;   use Persistent.Native_ODBC;

separate (Test_Graph_Schemes.Test_ODBC) procedure Test_Info is
   procedure Put_Cell (Text : String) is
   begin
      New_Line (File);
      Put (File, Cell_Beg);
      Put (File, Text);
      Put (File, Cell_End);
   end Put_Cell;

   procedure Put
             (  Command   : in out ODBC_Command;
                Data_Type : SQL_DATA_TYPE
             )  is
   begin
      Put_Line
      (  File,
         "<p>Support of " & SQL_DATA_TYPE'Image (Data_Type) & ":</p>"
      );
      declare
         Info : constant Type_Info :=
                         Get_Type_Info (Command'Access, Data_Type);
      begin
         Put_Line (File, Table_Beg);
         Put_Line (File, Row_Beg);
         Put_Cell ("Name");
         Put_Cell (Info.Type_Name);
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Data type");
         Put_Cell (SQL_DATA_TYPE'Image (Info.Data_Type));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Column size");
         Put_Cell (SQLINTEGER'Image (Info.Column_Size));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Literal prefix");
         Put_Cell (Info.Literal_Prefix);
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Literal suffix");
         Put_Cell (Info.Literal_Suffix);
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Create parameters");
         Put_Cell (Info.Create_Parameters);
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Nullable");
         Put_Cell (Boolean'Image (Info.Nullable));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Case sensitive");
         Put_Cell (Boolean'Image (Info.Case_Sensitive));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Searchable");
         Put_Cell (SQL_COLUMN_SEARCHABLE'Image (Info.Searchable));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Unsigned");
         Put_Cell (Boolean'Image (Info.Unsigned_Attribute));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Fixed");
         Put_Cell (Boolean'Image (Info.Fixed_Prec_Scale));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Auto unique");
         Put_Cell (Boolean'Image (Info.Auto_Unique_Value));
         Put_Line (File, Row_End & Row_Beg);
         Put_Cell ("Local name");
         Put_Cell (Info.Local_Name);
         Put_Line (File, Row_End);
         Put_Line (File, Table_End);
      end;
   exception
      when Constraint_Error =>
         Put_Line (File, "-- Not supported");
   end Put;
   Storage : constant Storage_Handle := Test_ODBC_Session.Open;
   Command : aliased ODBC_Command (Get_Connection (Storage));
begin
   Put_Line (File, "<hr><p><b>ODBC info test</b></p>");
   if Serializable (Storage) then
      Put_Line (File, "Serializable transaction isolation<br>");
   else
      Put_Line (File, "No serializable transaction isolation<br>");
   end if;
   Put (Command, SQL_INTEGER);
   Put (Command, SQL_DOUBLE);
   Put (Command, SQL_BIGINT);
   Put (Command, SQL_GUID);
   Put (Command, SQL_LONGVARCHAR);
   Put (Command, SQL_WCHAR);
   Put (Command, SQL_WLONGVARCHAR);
   Put (Command, SQL_LONGVARBINARY);
   Put (Command, SQL_TIMESTAMP);
   End_Transaction (Get_Connection (Storage).all);
end Test_Info;
