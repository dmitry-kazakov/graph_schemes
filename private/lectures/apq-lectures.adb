--                                                                    --
--  package APQ.Lectures            Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2002       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with APQ.Common;             use APQ.Common;
with Confidence_Factors;     use Confidence_Factors;
with Fuzzy.Edit;             use Fuzzy.Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

package body APQ.Lectures is

   function Value is new Integer_Value (Integer);

   procedure Create_Table
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String
             )  is
   begin
      Prepare
      (  Data_Base.Query.all,
         (  "CREATE TABLE "
         &  Table_Name
         &  "(data "
         &  String_Type (Data_Base)
         &  ", example_no "
         &  Integer_Type (Data_Base)
         &  ", feature_id "
         &  Ref_Type (Data_Base)
         &  ")"
      )  );
      Execute (Data_Base);
   end Create_Table;

   function Get_Examples_No
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String
            )  return Natural is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         "SELECT MAX(example_no) FROM " & Table_Name
      );
      Execute (Data_Base);
      Fetch (Command);
      return Value (Command, 1);
   exception
      when No_Tuple | Null_Value =>
         return 0;
   end Get_Examples_No;

   function Get_Features_No
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String
            )  return Natural is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         "SELECT DISTINCT COUNT(feature_id) FROM " & Table_Name
      );
      Execute (Data_Base);
      Fetch (Command);
      return Value (Command, 1);
   exception
      when No_Tuple | Null_Value =>
         return 0;
   end Get_Features_No;

   procedure Get
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : in out Set
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "SELECT data FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
      )  );
      Append (Command, Feature);
      Execute (Data_Base);
      Fetch (Command);      
      Value :=
         Fuzzy.Edit.Value
         (  APQ.Value (Command, 1),
            Value'First,
            Value'Last
         );
   exception
      when No_Tuple | Null_Value =>
         Value := (others => Confidence'Last);
      when Constraint_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Table '" & Table_Name & "' is corrupted" 
         );         
   end Get;

   function Is_Defined
            (  Data_Base  : APQ_Data_Base'Class;
               Table_Name : String;
               Example    : Positive;
               Feature    : Object_ID
            )  return Boolean is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "SELECT data FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
      )  );
      Append (Command, Feature);
      Execute (Data_Base);
      Fetch (Command);
      Clear (Command);
      return True;
   exception
      when No_Tuple | Null_Value =>
         Clear (Command);
         return False;
   end Is_Defined;

   procedure Put_Checked
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : String;
                Update     : Boolean
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      if Update then
         Prepare
         (  Command,
            (  "UPDATE "
            &  Table_Name
            &  " SET data = "
         )  );
         Append_Quoted
         (  Data_Base,
            Value,
            (  " WHERE example_no ="
            &  Image (Example)
            & " AND feature_id ="
         )  );
         Append (Command, Feature);
      else
         Prepare
         (  Command,
            (  "INSERT INTO "
            &  Table_Name
            &  " VALUES ("
         )  );
         Append_Quoted
         (  Data_Base,
            Value,
            "," & Image (Example) & ","
         );
         Append (Command, Feature, ")");
      end if;
      Execute (Data_Base);
   end Put_Checked;

   procedure Put
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : Set
             )  is
   begin
      Put_Checked
      (  Data_Base  => Data_Base,
         Table_Name => Table_Name,
         Value      => Image (Value),
         Example    => Example,
         Feature    => Feature,
         Update     =>
            Is_Defined (Data_Base, Table_Name, Example, Feature)
         );
   end Put;

   procedure Put
             (  Data_Base   : in out APQ_Data_Base'Class;
                Table_Name  : String;
                Example     : Positive;
                Feature     : Object_ID;
                Value       : Positive;
                Cardinality : Positive
             )  is
      Data   : Set (1..Cardinality);
      Update : constant Boolean :=
                  Is_Defined
                  (  Data_Base,
                     Table_Name,
                     Example,
                     Feature
                  );
   begin
      if Update then
         Get
         (  Data_Base,
            Table_Name,
            Example,
            Feature,
            Data
         );
      else
         Data := (others => Confidence'First);
      end if;
      Data (Value) := Confidence'Last;
      Put_Checked
      (  Data_Base  => Data_Base,
         Table_Name => Table_Name,
         Value      => Image (Data),
         Example    => Example,
         Feature    => Feature,
         Update     => Update
      );
   end Put;

   procedure Set_Undefined
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "DELETE FROM "
         &  Table_Name
         &  " WHERE example_no ="
         &  Image (Example)
         &  " AND feature_id ="
      )  );
      Append (Command, Feature);
      Execute (Data_Base);
   end Set_Undefined;

   procedure Set_Undefined
             (  Data_Base  : in out APQ_Data_Base'Class;
                Table_Name : String;
                Feature    : Object_ID
             )  is
      Command : Root_Query_Type'Class renames Data_Base.Query.all;
   begin
      Prepare
      (  Command,
         (  "DELETE FROM "
         &  Table_Name
         &  " WHERE feature_id ="
      )  );
      Append (Command, Feature);
      Execute (Data_Base);
   end Set_Undefined;

end APQ.Lectures;
