--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNU.DB.SQLCLI.API.Lectures                  Luebeck            --
--  Implementation                                 Summer, 2002       --
--                                                                    --
--                                Last revision :  20:17 02 Jun 2012  --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Confidence_Factors;           use Confidence_Factors;
with Fuzzy.Edit;                   use Fuzzy.Edit;
with GNU.DB.SQLCLI.API.Keys.Edit;  use GNU.DB.SQLCLI.API.Keys.Edit;
with Strings_Edit.Integers;        use Strings_Edit.Integers;

package body GNU.DB.SQLCLI.API.Lectures is

   procedure Create_Table
             (  Command          : in out ODBC_Command'Class;
                Table_Name       : String;
                Integer_SQL_Type : String;
                ID_SQL_Type      : String;
                Data_SQL_Type    : String
             )  is
   begin
      if not Table_Exists (Command.Connection, Table_Name) then
         Execute
         (  Command,
            (  "CREATE TABLE "
            &  Table_Name
            &  "(data "
            &  Data_SQL_Type
            &  ", example_no "
            &  Integer_SQL_Type
            &  ", feature_id "
            &  ID_SQL_Type
            &  ")"
         )  );
      end if;
   exception
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Create_Table;

   function Get_Examples_No
            (  Command    : access ODBC_Command'Class;
               Table_Name : String
            )  return Natural is
      Length : aliased SQLINTEGER;
      Count  : aliased SQLINTEGER;
   begin
      Prepare
      (  Command.all,
         "SELECT MAX(example_no) FROM " & Table_Name
      );
      Bind_Result
      (  Command.all,
         1,
         Count'Unchecked_Access,
         Length'Unchecked_Access
      );
      Execute (Command.all);
      Fetch (Command.all);
      Close_Cursor (Command.all);
      return Natural (Count);
   exception
      when No_Data =>
         Close_Cursor (Command.all);
         return 0;
      when Table_Not_Found =>
         Close_Cursor (Command.all);
         return 0;
      when Error : others =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Examples_No;

   function Get_Features_No
            (  Command    : access ODBC_Command'Class;
               Table_Name : String
            )  return Natural is
      Length : aliased SQLINTEGER;
      Count  : aliased SQLINTEGER;
   begin
      Prepare
      (  Command.all,
         "SELECT DISTINCT COUNT(feature_id) FROM " & Table_Name
      );
      Bind_Result
      (  Command.all,
         1,
         Count'Unchecked_Access,
         Length'Unchecked_Access
      );
      Execute (Command.all);
      Fetch (Command.all);
      Close_Cursor (Command.all);
      return Natural (Count);
   exception
      when No_Data =>
         Close_Cursor (Command.all);
         return 0;
      when Table_Not_Found =>
         Close_Cursor (Command.all);
         return 0;
      when Error : others =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Features_No;

   procedure Get
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : in out Fuzzy.Set
             )  is
   begin
      Execute
      (  Command,
         (  "SELECT data FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
         &  Image (Feature)
      )  );
      Fetch (Command);
      Value :=
         Fuzzy.Edit.Value
         (  Get_Data (Command'Unchecked_Access, 1, Never),
            Value'First,
            Value'Last
         );
      Close_Cursor (Command);
   exception
      when No_Data =>
         Close_Cursor (Command);
         Value := (others => Confidence'Last);
      when Table_Not_Found =>
         Close_Cursor (Command);
         Value := (others => Confidence'Last);
      when End_Error | Constraint_Error =>
         Close_Cursor (Command);
         Raise_Exception
         (  Data_Error'Identity,
            "Table '" & Table_Name & "' is corrupted"
         );
      when Data_Error =>
         Close_Cursor (Command);
         raise;
      when Error : others =>
         Close_Cursor (Command);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get;

   function Is_Defined
            (  Command    : access ODBC_Command'Class;
               Table_Name : String;
               Example    : Positive;
               Feature    : Object_ID
            )  return Boolean is
   begin
      Execute
      (  Command.all,
         (  "SELECT data FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
         &  Image (Feature)
      )  );
      case Fetch (Command) is
         when SQL_SUCCESS =>
            Close_Cursor (Command.all);
            return True;
         when SQL_NO_DATA =>
            Close_Cursor (Command.all);
            return False;
         when others =>
            Raise_Exception
            (  Data_Error'Identity,
               Get_Message (Command.all)
            );
      end case;
   exception
      when No_Data =>
         Close_Cursor (Command.all);
         return False;
      when Table_Not_Found =>
         Close_Cursor (Command.all);
         return False;
      when Data_Error =>
         Close_Cursor (Command.all);
         raise;
      when Error : others =>
         Close_Cursor (Command.all);
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Is_Defined;

   procedure Put_Checked
             (  Command       : in out ODBC_Command'Class;
                Table_Name    : String;
                Data_SQL_Type : SQL_DATA_TYPE;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : access String;
                Update        : Boolean
             )  is
      Length : aliased SQLINTEGER;
   begin
      if Update then
         Prepare
         (  Command,
            (  "UPDATE "
            &  Table_Name
            &  " SET data = ? WHERE example_no ="
            &  Image (Example)
            &  " AND feature_id ="
            &  Image (Feature)
         )  );
      else
         Prepare
         (  Command,
            (  "INSERT INTO "
            &  Table_Name
            &  " VALUES (?, "
            &  Image (Example)
            &  ", "
            &  Image (Feature)
            &  ")"
         )  );
      end if;
      Bind_Parameter
      (  Command,
         1,
         Value,
         Length'Unchecked_Access,
         Data_SQL_Type
      );
      Execute (Command);
   exception
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Put_Checked;

   procedure Put
             (  Command       : in out ODBC_Command'Class;
                Table_Name    : String;
                Data_SQL_Type : SQL_DATA_TYPE;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : Fuzzy.Set
             )  is
      Data : aliased String := Image (Value);
   begin
      Put_Checked
      (  Command       => Command,
         Table_Name    => Table_Name,
         Data_SQL_Type => Data_SQL_Type,
         Value         => Data'Access,
         Example       => Example,
         Feature       => Feature,
         Update        =>
            Is_Defined
            (  Command'Unchecked_Access,
               Table_Name,
               Example,
               Feature
      )     );
   end Put;

   procedure Put
             (  Command       : in out ODBC_Command'Class;
                Table_Name    : String;
                Data_SQL_Type : SQL_DATA_TYPE;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : Positive;
                Cardinality   : Positive
             )  is
      Data   : Set (1..Cardinality);
      Update : constant Boolean :=
                  Is_Defined
                  (  Command'Unchecked_Access,
                     Table_Name,
                     Example,
                     Feature
                  );
   begin
      if Update then
         Get
         (  Command,
            Table_Name,
            Example,
            Feature,
            Data
         );
      else
         Data := (others => Confidence'First);
      end if;
      Data (Value) := Confidence'Last;
      declare
         Value : aliased String := Image (Data);
      begin
         Put_Checked
         (  Command       => Command,
            Table_Name    => Table_Name,
            Data_SQL_Type => Data_SQL_Type,
            Value         => Value'Access,
            Example       => Example,
            Feature       => Feature,
            Update        => Update
         );
      end;
   end Put;

   procedure Set_Undefined
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             )  is
   begin
      Execute
      (  Command,
         (  "DELETE FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
         &  Image (Feature)
      )  );
   exception
      when No_Data | Table_Not_Found =>
         return;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Set_Undefined;

   procedure Set_Undefined
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Feature    : Object_ID
             )  is
   begin
      Execute
      (  Command,
         (  "DELETE FROM "
         &  Table_Name
         &  " WHERE feature_id ="
         &  Image (Feature)
      )  );
   exception
      when No_Data | Table_Not_Found =>
         return;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Set_Undefined;

end GNU.DB.SQLCLI.API.Lectures;
