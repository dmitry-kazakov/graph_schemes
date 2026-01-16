--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     GNU.DB.SQLCLI.API.Lectures                  Luebeck            --
--  Interface                                      Summer, 2004       --
--                                                                    --
--                                Last revision :  09:11 03 Jun 2012  --
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
--
--  This  package  defines  several  data types of commands used to deal
--  with a training set stored in an ODBC data base table:
--
--  (o)  Examples_Count_Command
--  (o)  Features_Count_Command
--  (o)  Get_Command
--
--  The training set table has the following columns:
--
--  data       : fuzzy set obtained by Fuzzy.Edit.Value, a text
--  example_no : integer
--  feature_id : object identifier
--
--  One  table  is used per image. So data column corresponds to a fuzzy
--  set.
--
with Fuzzy;                   use Fuzzy;
with GNU.DB.SQLCLI.API.Keys;  use GNU.DB.SQLCLI.API.Keys;

package GNU.DB.SQLCLI.API.Lectures is
   pragma Elaborate_Body (GNU.DB.SQLCLI.API.Lectures);
--
-- Create_Table -- Create a training set table
--
--    Command          - A scratch command
--    Table_Name       - The table name
--    Integer_SQL_Type - Integers' SQL type (the name of)
--    ID_SQL_Type      - Identifiers' SQL type
--    Data_SQL_Type    - Data' SQL type
--
-- Table is created only if it does not exist.
--
-- Exceptions :
--
--    Data_Error - A data base error
--
-- Effects :
--
--    Command is modified to be
--
   procedure Create_Table
             (  Command          : in out ODBC_Command'Class;
                Table_Name       : String;
                Integer_SQL_Type : String;
                ID_SQL_Type      : String;
                Data_SQL_Type    : String
             );
--
-- Get_Examples_No -- In an image table
--
--    Command     - The command to use
--    Table_Name  - The table name
--
-- When the table does not exist, the result is 0.
--
-- Returns :
--
--    The number of examples in the table
--
-- Exceptions :
--
--    Data_Error - Corrupted data base
--
-- Effects :
--
--    Command is used
--
   function Get_Examples_No
            (  Command    : access ODBC_Command'Class;
               Table_Name : String
            )  return Natural;
--
-- Get_Features_No -- In an image table
--
--    Command     - The command to use
--    Table_Name  - The table name
--
-- When the table does not exist, the result is 0.
--
-- Returns :
--
--    The number of features in the table
--
-- Exceptions :
--
--    Data_Error - Corrupted data base
--
-- Effects :
--
--    Command is used
--
   function Get_Features_No
            (  Command    : access ODBC_Command'Class;
               Table_Name : String
            )  return Natural;
--
-- Get -- Image of a fuzzy feature
--
--    Command    - The command to use
--    Table_Name - The table name
--    Example    - The example number
--    Feature    - The identifier of
--    Value      - The value of the image (output)
--
-- This function extracts the value of a feature  image  represented  by
-- the table. If no such exists all 1 set is returned.
--
-- Returns :
--
--    The value
--
-- Exceptions :
--
--    Data_Error - Corrupted data base
--
-- Effects :
--
--    Command is used
--
   procedure Get
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : in out Fuzzy.Set
             );
--
-- Is_Defined -- Check if value is defined
--
--    Command     - The command
--    Table_Name  - The table name
--    Example     - The example number
--    Feature     - The identifier of
--    Cardinality - Of the feature
--
-- This  function  searches for the value of a feature image represented
-- by the table.
--
-- Returns :
--
--    True if there is a value defined
--
-- Exceptions :
--
--    Data_Error - Corrupted data base
--
-- Effects :
--
--    Command is used
--
   function Is_Defined
            (  Command    : access ODBC_Command'Class;
               Table_Name : String;
               Example    : Positive;
               Feature    : Object_ID
            )  return Boolean;
--
-- Put -- An example into an image table
--
--    Command       - The command
--    Table_Name    - The table name
--    Data_SQL_Type - The type used to store data (variable strings)
--    Example       - The example
--    Feature       - The feature
--    Value         - The value of the image
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Put
             (  Command       : in out ODBC_Command'Class;
                Table_Name    : String;
                Data_SQL_Type : SQL_DATA_TYPE;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : Fuzzy.Set
             );
--
-- Put -- An example into an image table
--
--    Command       - The command
--    Table_Name    - The table name
--    Data_SQL_Type - The type used to store data (variable strings)
--    Example       - The example
--    Feature       - The feature
--    Value         - The domain point of the image to set
--    Cardinality   - Of the feature
--
-- Exceptions :
--
--    Constraint_Error - Illegal domain point
--    Data_Error       - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Put
             (  Command       : in out ODBC_Command'Class;
                Table_Name    : String;
                Data_SQL_Type : SQL_DATA_TYPE;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : Positive;
                Cardinality   : Positive
             );
--
-- Set_Undefined -- Remove an example
--
--    Command   - The command
--  [ Example ] - The example number
--    Feature   - The identifier of
--
-- This  procedure  deletes  the  example  for the feature from. Nothing
-- happens if the example, feature or lecture does not exist.
--
-- Exceptions :
--
--    Data_Error - Data base error
--
-- Effects :
--
--    Command is modified
--
   procedure Set_Undefined
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             );
   procedure Set_Undefined
             (  Command    : in out ODBC_Command'Class;
                Table_Name : String;
                Feature    : Object_ID
             );
end GNU.DB.SQLCLI.API.Lectures;
