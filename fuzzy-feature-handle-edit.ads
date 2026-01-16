--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Handle.Edit                   Luebeck            --
--  Interface                                      Spring, 2002       --
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
--
--  This package provides string editing for features. See  Strings_Edit
--  for further information.
--
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;
with Integer_Intervals;          use Integer_Intervals;
with Units;                      use Units;

package Fuzzy.Feature.Handle.Edit is
   pragma Elaborate_Body (Fuzzy.Feature.Handle.Edit);
--
-- The input parameters:
--
-- (o)  Base is one of numeric feature values. It influences only  input
--      of the values of integer features;
-- (o)  Default is used for intuitionistic sets and classifications.  It
--      determines  the  confidence  factors  when omitted. If both are,
--      then  Default  is  the  result. If only the necessity is omitted
--      then  the  result  necessity  is  the  specified possibility and
--      Default.Necessity;
-- (o)  Get_Units, when true, forces explicit unit  specification.  When
--      no unit specified Unit_Error is propagated  unless  the  feature
--      scale is 1 SI. So dimensionless features never need any explicit
--      unit specification;
-- (o)  Mode is the character set used for parsing unit specifications;
-- (o)  Quote_Units,  when true, requires all unit specifications should
--      be put in [] brackets.
--
   Input_Defaults : constant Input_Parameters :=
                    (  Base        => 10,
                       Default     => Certain_True,
                       Get_Units   => False,
                       Mode        => UTF8_Set,
                       Quote_Units => False
                    );
--
-- The output parameters:
--
-- (o)  Abs_Small together  with  Rel_Small  control  the  precision  of
--      floating-point  values  output  as  described  in  the   package
--      Strings_Edit.Float_Edit;
-- (o)  Base  is  one  of  numeric  feature  values.  It influences only
--      integer features;
-- (o)  Default  is  used for intuitionistic sets and classifications. A
--      range  is output as a singleton if the interval consists of only
--      it.  Otherwise  both  the  left and right bounds are output. The
--      parameter  Default  determines  the  way  truth  values  can  be
--      omitted. If the possibility / necessity pair equals  to  Default
--      then the truth values specification is omitted. If the necessity
--      equals to the possibility and Default.Necessity  then  only  the
--      possibility is output;
-- (o)  Mode is the character used unit specifications. It only used for
--      SI units;
-- (o)  Rel_Small, see Abs_Small;
-- (o)  Put_Plus,  if  true,  forces  positive  numeric  feature  values
--      (integer and floating-point) be  output  with  +  preceding  the
--      mantissa;
-- (o)  Put_Units,  if  true,  forces explicit unit specification unless
--      feature is output in SI units and the unit is 1 SI;
-- (o)  Quote_Units, if true, instructs unit specifications be put in []
--      brackets;
-- (o)  Use_Derived, if true, allows using derived SI units;
-- (o)  Use_SI,  if true, forces output in SI units. Note that SI output
--      is also used when Put_Units is true and the feature values scale
--      has empty unit text.
--
   Output_Defaults : constant Output_Parameters :=
                     (  Abs_Small   =>-MaxSmall,
                        Base        => 10,
                        Default     => Certain_True,
                        Mode        => UTF8_Set,
                        Rel_Small   => MaxSmall,
                        Put_Plus    => False,
                        Put_Units   => False,
                        Quote_Units => False,
                        Use_Derived => True,
                        Use_SI      => False
                     );
--
-- Get -- Get a value of the feature from a string
--
--    Source     - The string to be processed
--    Pointer    - The current position in the string
--    Feature    - The feature handle
--    Value      - The result (set, intuitionistic set, classification)
--    Parameters - Controlling input
--
-- This  procedure  gets  a  feature  value  from the string Source. The
-- process  starts  from  Source  (Pointer).  Pointer is advanced to the
-- string position following the domain value. The feature value can  be
-- either an  interval  of  domain  values,  a  fuzzy  subset,  a  fuzzy
-- intuitionistic  subset  or  an  intuitionistic  classification of the
-- domain.  The  cardinality  of  the feature and of the parameter Value
-- should  be  same.  Otherwise,  Constraint_Error  is  propagated.  The
-- parameter Default determines the confidence factors when omitted.
--
-- Exceptions:
--
--    Constraint_Error - Invalid handle, wrong Value cardinality
--    Data_Error       - Syntax error in the value
--    End_Error        - Nothing matched
--    Layout_Error     - Pointer not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : in Feature_Handle;
                Value      : out Interval;
                Parameters : Input_Parameters'Class := Input_Defaults
             );
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Set;
                Parameters : Input_Parameters'Class := Input_Defaults
             );
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Intuitionistic.Set;
                Parameters : Input_Parameters'Class := Input_Defaults
             );
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Handle;
                Value      : out Intuitionistic.Classification;
                Parameters : Input_Parameters'Class := Input_Defaults
             );
--
-- Get_Domain
--
--    Feature    - The feature handle
--    Domain     - Feature domain description
--    Parameters - The output parameters
--
-- This  procedure  retunrs  the  feature  domain  description  in   the
-- parameter  Domain. Note that for numeric features the description may
-- contain illegal names.
--
   procedure Get_Domain
             (  Feature    : Feature_Handle;
                Domain     : out Domain_Description'Class;
                Parameters : Output_Parameters'Class := Output_Defaults
             );
--
-- Image -- A string representation of a feature value
--
--    Feature    - The feature handle
--    Value      - Interval, set, intuitionistic set, classification
--    Parameters - The output parameters
--
-- For a range of the domain set points Value the result is  its  string
-- representation. This function is a shortcut for Put.
--
-- Returns :
--
--    Image string of the value
--
-- Exceptions :
--
--    Constraint_Error - Illegal value or invalid handle
--
   function Image
            (  Feature    : Feature_Handle;
               Value      : Interval;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
   function Image
            (  Feature    : Feature_Handle;
               Value      : Domain_Subset;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
   function Image
            (  Feature    : Feature_Handle;
               Value      : Set;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
   function Image
            (  Feature    : Feature_Handle;
               Value      : Intuitionistic.Set;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
   function Image
            (  Feature    : Feature_Handle;
               Value      : Intuitionistic.Classification;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
--
-- Value -- String to feature value conversion
--
--    Source     - The string to be processed
--    Feature    - The feature handle
--    Parameters - Controlling input
--
-- This  function gets a feature value from the string Source. The input
-- can  be surrounded by spaces and tabs. The whole string Source should
-- be  matched.  Otherwise  the  exception Data_Error is propagated.
--
-- Returns :
--
--    Interval, set, intuitionistic set, classification
--
-- Exceptions:
--
--    Constraint_Error - Invalid handle
--    Data_Error       - Syntax error in the value
--    End_Error        - There is no any value
--
   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Interval;
   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Set;
   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Intuitionistic.Set;
   function Value
            (  Source     : String;
               Feature    : Feature_Handle;
               Parameters : Input_Parameters'Class := Input_Defaults
            )  return Intuitionistic.Classification;
--
-- Put -- Put a feature value into a string
--
--    Destination - The string that accepts the output
--    Pointer     - The current position in the string
--    Feature     - The feature handle
--    Value       - Interval, set, intuitionistic set, classification
--    Parameters  - The output parameters
--    Field       - The output field
--    Justify     - Alignment within the field
--    Fill        - The fill character
--
-- This  procedure  places  a  feature  value specified by the parameter
-- Value  into  the  output  string  Destination. The parameter Value is
-- either  an  interval  or a domain subset or a fuzzy subset or a fuzzy
-- intuitionistic set or an intuitionistic classification of the feature
-- domain. The string is written starting  from  Destination  (Pointer).
-- Pointer is then advanced to point after the end of the output  field.
-- Parameters determine the way the output is done. The parameter  Field
-- determines the width of  the  output  field.  Zero  width  means  the
-- minimal  possible  width. If Field is not zero Justify determines the
-- way the  value  should  be  aligned  within  the  output  field.  The
-- parameter  Fill  is  then  the fill character. The cardinality of the
-- feature and  of  the  parameter  Value  should  be  same.  Otherwise,
-- Constraint_Error is propagated.
--
-- Exceptions:
--
--    Constraint_Error - Invalid handle or illegal value
--    Layout_Error     - Pointer is not in Destination'Range or there is
--                       no room for the output.
--
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Interval;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Domain_Subset;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Set;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Intuitionistic.Set;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Handle;
                Value       : Intuitionistic.Classification;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
private
   pragma Inline (Get);
   pragma Inline (Image);
   pragma Inline (Value);
   pragma Inline (Put);

end Fuzzy.Feature.Handle.Edit;
