--                                                                    --
--  package Fuzzy.Feature.Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2003       --
--                                                                    --
--                                Last revision :  10:01 09 Apr 2016  --
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

with Fuzzy.Abstract_Edit;  use Fuzzy.Abstract_Edit;
with Integer_Intervals;    use Integer_Intervals;

with Fuzzy.Intuitionistic;
with Measures_Universal_Edit;

package Fuzzy.Feature.Edit is
   pragma Elaborate_Body (Fuzzy.Feature.Edit);
--
-- Get -- Get a range of values of the feature from a string
--
--    Source     - The string to be processed
--    Pointer    - The current position in the string
--    Feature    - The feature
--    Value      - The range of (a subinterval of [1..Cardinality])
--    Parameters - The input parameters
--
-- This procedure gets a range of feature values from the string Source.
-- The process starts from Source (Pointer). Pointer is advanced to  the
-- string position  following  the  range.  The  result  in  the  output
-- parameter  Value  is  an  interval  of  indices of the feature domain
-- value.  It  is  a  subinterval of 1..Cardinality. The range syntax is
-- usually:
--
--    <value> [{..|...}<value>]
--
-- Spaces and tabs may surround ellipsis. Ellipsis may be built  of  two
-- or  three  dots. The syntax of <value> depends on the feature and the
-- fields of Parameters.
--
-- Exceptions:
--
--    Constraint_Error - A value is out of range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--    Unit_Error       - Error in units
--
   procedure Get
             (  Source     : in String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Interval;
                Parameters : Input_Parameters'Class
             );
--
-- Get -- Get a value of the feature from a string
--
--    Source     - The string to be processed
--    Pointer    - The current position in the string
--    Feature    - The feature
--    Value      - The result (set, intuitionistic set, classification)
--    Parameters - The input parameters
--
-- These procedures get a feature value  from  the  string  Source.  The
-- value  is  either  a  subset,  a  proper  intuitionistic  subset or a
-- classification of the feature domain set. The second  one  tells  how
-- possible  and  necessary the feature domain values are subsets of the
-- given one. The third tells how the value is a  subset  of  them.  The
-- process  starts  from  Source  (Pointer).  Pointer is advanced to the
-- string position following the value. Constraint_Error  is  propagated
-- when the cardinality of  the  parameter  Value  differ  from  one  of
-- Feature. The fields of  Parameters  determine  the  way  the  feature
-- values are input.
--
-- Exceptions:
--
--    Constraint_Error - Wrong value cardinality
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--    Unit_Error       - Error in units
--
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Set;
                Parameters : Input_Parameters'Class
             );
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Fuzzy.Intuitionistic.Classification;
                Parameters : Input_Parameters'Class
             );
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Fuzzy.Intuitionistic.Set;
                Parameters : Input_Parameters'Class
             );
--
-- Image -- A string representation of a feature domain interval
--
--    Feature    - The feature
--    Value      - A subinterval of 1..Cardinality
--    Parameters - The output parameters
--
-- Returns :
--
--    Image string of the value
--
-- Exceptions :
--
--    Constraint_Error - Illegal interval
--
   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Interval;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Image -- A string representation of a value
--
--    Feature    - The feature
--    Value      - The fuzzy set
--    Parameters - The output parameters
--
-- These  functions  are  shortcut  for  the corresponding Put ones. The
-- cardinality of the feature and of Value should  be  same.  Otherwise,
-- Constraint_Error is propagated.
--
-- Returns :
--
--    Image string of the realization
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality
--
   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Domain_Subset;
               Parameters : Output_Parameters'Class
            )  return String;
   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Set;
               Parameters : Output_Parameters'Class
            )  return String;
   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Fuzzy.Intuitionistic.Classification;
               Parameters : Output_Parameters'Class
            )  return String;
   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Fuzzy.Intuitionistic.Set;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Value -- Conversion of string to feature domain value range
--
--    Source     - The string to be processed
--    Feature    - The feature
--    Parameters - The input parameters
--
-- This  function  gets  a  feature  domain  value range from the string
-- Source.  The  range  has  the  syntax  described  in  Get.  It can be
-- surrounded by spaces and tabs. The  whole  string  Source  should  be
-- matched. Otherwise the exception Data_Error is propagated. The result
-- is a range of the indices of the feature domain values. The fields of
-- Parameters determine the way the feature values are input.
--
-- Returns :
--
--    A subinterval of 1..Cardinality
--
-- Exceptions:
--
--    Constraint_Error - A value is out of range
--    Data_Error       - Syntax error
--    End_Error        - Nothing matched
--    Unit_Error       - Error in units
--
   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Interval;
--
-- Value -- String to value conversion
--
--    Source     - The string to be processed
--    Feature    - The feature
--    Parameters - The input parameters
--
-- These  functions get a feature value from the string Source. They are
-- shorcuts for the corresponding  Get  procedures.  The  value  in  the
-- string  can be surrounded by spaces and tabs. The whole string Source
-- should be matched. Otherwise the exception Data_Error is  propagated.
-- The  fields  of  Parameters  determine the way the feature values are
-- input.
--
-- Returns :
--
--    Set, intuitionistic set, classification
--
-- Exceptions:
--
--    Data_Error - Syntax error
--    End_Error  - Nothing matched
--    Unit_Error - Error in units
--
   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Set;
   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Fuzzy.Intuitionistic.Classification;
   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Fuzzy.Intuitionistic.Set;
--
-- Put -- Put a feature values range into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Feature     - The feature
--    Value       - The interval of indices to be put
--    Parameters  - The output parameters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places  the  feature  domain values specified by the
-- parameter Value into the output string  Destination.  The  string  is
-- written starting from Destination (Pointer). Pointer is then advanced
-- to point after the end of  the  output  field.  The  parameter  Field
-- determines the width of  the  output  field.  Zero  width  means  the
-- minimal  possible  width. If Field is not zero Justify determines the
-- way the  value  should  be  aligned  within  the  output  field.  The
-- parameter Fill is then the fill  character.  If  the  interval  Value
-- contains only one point the syntax <value> is  used.  Otherwise,  the
-- syntax of output is <value>..<value>. The syntax of values depends on
-- the  feature  and  is  additionally  controlled  by  the  fields   of
-- Parameters.
--
-- Exceptions:
--
--    Constraint_Error - Illegal interval
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Interval;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Put -- Put a feature value into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Feature     - The feature
--    Value       - To be put (set, intuitionistic set, classification)
--    Parameters  - The output parameters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- These procedures place a feature value (the parameter Value) into the
-- output string  Destination.  The  string  is  written  starting  from
-- Destination (Pointer). Value is either a domain subset  of,  a  fuzzy
-- subset  of,  a  proper intuitionistic subset of, or an intuitionistic
-- classification  into  the  feature domain set. Pointer is advanced to
-- position  after  the  end  of  the  output field. The parameter Field
-- determines the width of  the  output  field.  Zero  width  means  the
-- minimal  possible  width. If Field is not zero Justify determines the
-- way the  value  should  be  aligned  within  the  output  field.  The
-- parameter Fill  is  then  the  fill  character.  Constraint_Error  is
-- propagated  when  the  cardinality of the parameter Value differ from
-- one of Feature. The  fields  of  Parameters  determines  the  way  of
-- output.
--
-- Exceptions:
--
--    Constraint_Error - Wrong cardinality
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Domain_Subset;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Set;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Fuzzy.Intuitionistic.Classification;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Fuzzy.Intuitionistic.Set;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );

private
   type Feature_Ptr is access constant Feature_Object'Class;

   type Input_Parameters_Ptr is access
      constant Input_Parameters'Class;
   type Input_Data is new User_Data with record
      Parameters : Input_Parameters_Ptr;
      Feature    : Feature_Ptr;
   end record;
--
-- Get -- Overrides Fuzzy.Abstract_Edit...
--
   overriding
   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : Input_Data;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             );
--
-- Get_Max_Range -- Overrides Fuzzy.Abstract_Edit...
--
   overriding
   procedure Get_Max_Range
             (  Data    : Input_Data;
                From    : out Integer;
                To      : out Integer
             );
--
-- Put -- Instantiated from Fuzzy.Generic_Edit...
--
   overriding
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : Input_Data;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   type Output_Parameters_Ptr is access
      constant Output_Parameters'Class;
   type Output_Data is new User_Data with record
      Parameters : Output_Parameters_Ptr;
      Feature    : Feature_Ptr;
   end record;
--
-- Get -- Overrides Fuzzy.Abstract_Edit...
--
   overriding
   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : Output_Data;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             );
--
-- Get_Max_Range -- Overrides Fuzzy.Abstract_Edit...
--
   overriding
   procedure Get_Max_Range
             (  Data    : Output_Data;
                From    : out Integer;
                To      : out Integer
             );
--
-- Put -- Instantiated from Fuzzy.Generic_Edit...
--
   overriding
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : Output_Data;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );

end Fuzzy.Feature.Edit;
