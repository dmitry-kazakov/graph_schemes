--                                                                    --
--  package APQ.Lectures            Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
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
--
--  This  package  defines  several  data types of commands used to deal
--  with a training set stored in an APQ interfaced data base table:
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
with APQ.Common;  use APQ.Common;
with APQ.Keys;    use APQ.Keys;
with Fuzzy;       use Fuzzy;

package APQ.Lectures is
   pragma Elaborate_Body (APQ.Lectures);
--
-- Create_Table -- Create a training set table
--
--    Connection - To use
--    Table_Name - The table name
--
-- Note that it is not checked whether the table already exists.
--
-- Exceptions :
--
--    Data_Error - A data base error
--    Use_Error  - Data base access violation
--
   procedure Create_Table
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String
             );
--
-- Get_Examples_No -- In an image table
--
--    Connection - To use
--    Command    - A scratch command of the connection
--    Table_Name - The table name
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
--    Use_Error  - Data base access violation
--
   function Get_Examples_No
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String
            )  return Natural;
--
-- Get_Features_No -- In an image table
--
--    Connection - To use
--    Command    - A scratch command of the connection
--    Table_Name - The table name
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
--    Use_Error  - Data base access violation
--
-- Effects :
--
--    Command is used
--
   function Get_Features_No
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String
            )  return Natural;
--
-- Get -- Image of a fuzzy feature
--
--    Connection - To use
--    Command    - A scratch command of the connection
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
--    Use_Error  - Data base access violation
--
   procedure Get
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : in out Set
             );
--
-- Is_Defined -- Check if value is defined
--
--    Connection  - To use
--    Command     - A scratch command of the connection
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
--    Use_Error  - Data base access violation
--
   function Is_Defined
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String;
               Example    : Positive;
               Feature    : Object_ID
            )  return Boolean;
--
-- Put -- An example into an image table
--
--    Connection - To use
--    Command    - A scratch command of the connection
--    Table_Name - The table name
--    Example    - The example
--    Feature    - The feature
--    Value      - The value of the image
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Data base access violation
--
   procedure Put
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : Set
             );
--
-- Put -- An example into an image table
--
--    Connection  - To use
--    Command     - A scratch command of the connection
--    Table_Name  - The table name
--    Example     - The example
--    Feature     - The feature
--    Value       - The domain point of the image to set
--    Cardinality - Of the feature
--
-- Exceptions :
--
--    Constraint_Error - Illegal domain point
--    Data_Error       - Data base error
--    Use_Error        - Data base access violation
--
   procedure Put
             (  Data_Base   : in out APQ_Data_Base'Class;
                Table_Name  : String;
                Example     : Positive;
                Feature     : Object_ID;
                Value       : Positive;
                Cardinality : Positive
             );
--
-- Set_Undefined -- Remove an example
--
--    Connection - To use
--    Command    - A scratch command of the connection
--  [ Example ]  - The example number
--    Feature    - The identifier of
--
-- This  procedure  deletes  the  example  for the feature from. Nothing
-- happens if the example, feature or lecture does not exist.   
--
-- Exceptions :
--
--    Data_Error - Data base error
--    Use_Error  - Data base access violation
--
   procedure Set_Undefined
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             );
   procedure Set_Undefined
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Feature    : Object_ID
             );
end APQ.Lectures;
