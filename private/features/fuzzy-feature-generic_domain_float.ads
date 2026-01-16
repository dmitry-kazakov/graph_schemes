--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.                              Luebeck            --
--        Generic_Domain_Float                     Summer, 2005       --
--  Interface                                                         --
--                                Last revision :  22:14 29 Jan 2012  --
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
--  This generic package is the root  for  all  floating-point  features
--  based  on same type. It defines the interface of such features which
--  particular packages implements. The formal parameters are:
--
--  Number         - The type used for domain values
--  Suffix         - The class name suffix used for  the  features  that
--                   are based upon
--  Interval_Index - The index type used to count intervals
--  Interval_Map   - The array type mapping index to confidence
--  To_Confidence  - Interval_Index->Confidence. It shall be an array of
--                   ascending   values.   The  last  value  has  to  be
--                   Confidence'Last.
--
with Units;  use Units;

with Fuzzy.Feature.Generic_Independent;
with Fuzzy.Floats;
with Fuzzy.Intuitionistic;
with Fuzzy.Linguistics.Edit;
with Fuzzy.Linguistics.Sets.Edit;
with Fuzzy.Measures.Linguistics.Sets;
with Intervals.Floats;
with Intervals.Measures;
with Measures;
with Measures_Derived;
with Measures_Irregular;
with Measures_UTF8_Edit;
with Strings_Edit.Float_Edit;

generic
   type Domain_Number is digits <>;
   Suffix : String;
   type Interval_Index is (<>);
   type Interval_Map is array (Interval_Index) of Confidence;
   To_Confidence : Interval_Map;
package Fuzzy.Feature.Generic_Domain_Float is
   subtype Domain_Float is Domain_Number;
   package Float_Intervals is new Intervals.Floats (Domain_Float'Base);
   package Fuzzy_Floats is
      new Fuzzy.Floats
          (  Float_Intervals => Float_Intervals,
             Interval_Index  => Interval_Index,
             Interval_Map    => Interval_Map,
             To_Confidence   => To_Confidence
          );
   package Float_Edit is
      new Strings_Edit.Float_Edit (Domain_Float'Base);
   package Float_Measures is new Standard.Measures (Domain_Float'Base);
   package Interval_Measures is
      new Intervals.Measures
          (  Float_Measures  => Float_Measures,
             Float_Intervals => Float_Intervals
          );
   package Fuzzy_Measures is
      new Fuzzy.Measures
          (  Interval_Measures => Interval_Measures,
             Fuzzy_Floats      => Fuzzy_Floats
          );
   package Variables is new Fuzzy.Linguistics (Fuzzy_Floats);
   package Variable_Measures is
      new Fuzzy_Measures.Linguistics (Variables);
   package Variable_Sets is new Variables.Sets;
   package Variable_Measure_Sets is
      new Variable_Measures.Sets (Variable_Sets);
   package Derived_Measures is
      new Measures_Derived (Float_Measures);
   package Irregular_Measures is
      new Measures_Irregular (Derived_Measures);
   package Measure_UTF8_Edit is
      new Measures_UTF8_Edit (Irregular_Measures, Float_Edit);
   package Measure_Edit renames Measure_UTF8_Edit.Universal_Edit;
   package Variable_Edit is new Variables.Edit (Float_Edit);
   package Variable_Set_Edit is new Variable_Sets.Edit (Variable_Edit);

   use Fuzzy_Measures;
   use Variable_Measures;
   use Variable_Measure_Sets;
   use Variable_Sets;
   use Variable_Set_Edit;
   use Variables;
   use Float_Edit;
   use Measure_Edit;

--
-- Domain_Feature_Object -- The  abstract  base  type  for  all  domain-
--                          valued features
--
   type Domain_Feature_Object is abstract
      new Feature_Object with null record;
   type Domain_Feature_Object_Ptr is access Domain_Feature_Object'Class;
   for Domain_Feature_Object_Ptr'Storage_Pool
      use Feature_Object_Ptr'Storage_Pool;
--
-- Accumulate -- A fuzzy set
--
--    Feature - The feature
--    Value   - The value to be accumulated
--
-- This  function  evaluates  the  accumulated  result of a fuzzy domain
-- subset.  Value is the subset. Constraint_Error is propagated when the
-- cardinality of Value differs from one of Feature.
--
-- Returns :
--
--    A dimensioned variable representing the accumulated result
--
-- Exceptions :
--
--    Constraint_Error - Invalid value
--
   function Accumulate
            (  Feature : Domain_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measure is abstract;
--
-- Classify -- Fuzzify a value using the domain set
--
--    Feature - The feature
--    Value   - The value to be converted
--
-- The  result  is  a classification of the value. It tells how possible
-- and necessary the value  is  contained  by  the  subsets  (linguistic
-- variables) of the feature domain set. The parameter Value is either a
-- dimensioned  number,  a  dimensioned  interval or a dimensioned fuzzy
-- number or a dimensioned variable. The cardinality of  the  result  is
-- one of the feature.
--
-- Returns :
--
--    Fuzzified value
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification is abstract;
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification is abstract;
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification is abstract;
   function Classify
            (  Feature : Domain_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is abstract;
--
-- Get -- Get dimensioned value from string
--
--    Source     - To get the value from
--    Pointer    - The position in the source to start with
--    Value      - The result
--    Parameters - Input parameters
--
-- This procedure gets a value starting  from  Source  (Pointer).  After
-- completion Pointer is advanced to the position following  the  value.
-- When  the value is dimensioned Has_Unit is set to true, otherwise, it
-- is set  to  false.  When  Parameters.Quote_Units  is  true  the  unit
-- specification has to be put in [] brackets, for example, 2.34 [km/h].
-- Otherwise, units can be intermixed with the value,  like  2.34  km/h.
-- Input shall always start with a  number.  So  km/h  is  illegal.  The
-- parameter Scale is the dimension specified for Value. In the examples
-- considered, it is 1 km/h.
--
-- Exceptions :
--
--    Constraint_Error - Numberic overflow
--    Data_Error       - Syntax error
--    Layout_Error     - Pointer is not in Source'First..Source'Last + 1
--    Unit_Error       - Illegal unit
--
   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Value      : out Scaled;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Scale -- Get the scale of a feature
--
--    Feature - The feature
--
-- The result is the scale of the feature  values  used  by  default  in
-- input and output.
--
-- Returns :
--
--    The feature scale
--
   function Get_Scale (Feature : Domain_Feature_Object)
      return Measure is abstract;
--
-- Get_Scale_Text -- Get the default scale specification of the feature
--
--    Feature    - The feature
--    Parameters - Of the output
--
-- The result is the string describing  the  feature  scale.  Parameters
-- control the result. When Parameters.Use_SI is true, or else when  the
-- feature  was created with no scale string specified, the result is in
-- SI  units.  Otherwise  it  the  scale  string  as it was specified in
-- Create. In the former case Parameters.Mode controls the character set
-- to use. Additionally, Parameters.Use_Derived allows use of derived SI
-- units.  The  result  is an empty string if 1 SI should be the result.
-- When  the  result  is  not  empty, it is surrounded by []-brackets if
-- Parameters.Quote_Units is true.
--
-- Returns :
--
--    The feature scale
--
   function Get_Scale_Text
            (  Feature    : Domain_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String is abstract;
--
-- Get_Scaled -- Get scaled value
--
--    Source - The string containing the value
--    Scale  - The scale
--
-- This function parses Source for a dimensioned value. The value can be
-- specified either as a plain number (Float_Edit.Get) or  as  a  number
-- followed by a unit  specification  (Measure_UTF8_Edit.Get).  In  that
-- case it should be  compatible  with  Scale,  or  else  Unit_Error  is
-- propagated.
--
-- Returns :
--
--    The value scaled by Scale
--
-- Exceptions :
--
--    Constraint_Error - Numeric error
--    Data_Error       - Syntax error
--    End_Error        - Nothing specified
--    Unit_Error       - Illegal unit
--    Use_Error        - Specified scale has incompatible unit
--
   function Get_Scaled
            (  Source : String;
               Scale  : Measure
            )  return Domain_Float;
--
-- Get_Variable -- Get a linguistic variable by its domain value index
--
--    Feature - The feature
--    Value   - The value (domain set value index 1..Cardinality)
--
-- Returns :
--
--    The dimensioned variable
--
-- Exceptions :
--
--    Constraint_Error - Invalid value
--
   function Get_Variable
            (  Feature : Domain_Feature_Object;
               Value   : Positive
            )  return Variable_Measure is abstract;
--
-- Get_Unit -- Get SI dimension of the feature
--
--    Feature - The feature
--
-- Returns :
--
--    The SI unit compatible with the feature units
--
   function Get_Unit (Feature : Domain_Feature_Object)
      return Unit is abstract;
--
-- Get_Unit -- Get unit specification
--
--    Source     - The string to be processed
--    Pointer    - The current position in the string
--    Value      - The result
--    Got_It     - Indicates presense of unit specification
--    Parameters - The input parameters
--
-- This procedure gets a unit specification from the string Source.  The
-- unit     specification     is     optional     and    depending    on
-- Parameters.Quote_Units   should   be  placed  in  []  brackets.  When
-- recogniyed, the parameter Value is the unit obtained  and  Got_It  is
-- true. Otherwise Got_It is false and  Value  is  unchanged.  The  unit
-- specification shall  have  positive  gain,  otherwise  Unit_Error  is
-- propagated.
--
-- Exceptions:
--
--    Data_Error   - Syntax error
--    Layout_Error - Pointer not in Source'First..Source'Last + 1
--    Unit_Error   - Error in units
--
   procedure Get_Unit
             (  Source     : String;
                Pointer    : in out Integer;
                Value      : in out Measure;
                Got_It     : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Is_Domain_Linguistic -- Check if the domain values are named
--
--    Feature - The feature
--
-- Returns :
--
--    True if the domain values (variables) are named
--
   function Is_Domain_Linguistic (Feature : Domain_Feature_Object)
      return Boolean is abstract;
--
-- Put_Dimension -- Of a feature
--
--    Destination - The string to put dimension into
--    Pointer     - The postion to start at
--    Scale       - The text that describes the unit
--    Feature     - The feature object
--
-- The  procedure  verifies  Scale  for  being  a  valid non-empty UTF-8
-- string. If not the dimension is taken from the feature.
--
   procedure Put_Dimension
             (  Destination : in out String;
                Pointer     : in out Integer;
                Scale       : String;
                Feature     : Domain_Feature_Object'Class
             );
--
-- To_Set -- Get an intuitionistic set from the value
--
--    Feature - The feature
--    Value   - The value to be converted
--
-- The result is an intuitionistic fuzzy set of the value. It tells  how
-- possible and  necessary  is  that  the  value  contains  the  subsets
-- (linguistic variables) of the feature domain set. The parameter Value
-- is  either  a  dimensioned  number,  a  dimensioned  interval  or   a
-- dimensioned  fuzzy  number or a dimensioned variable. The cardinality
-- of the result is one of the feature.
--
-- Returns :
--
--    Set value
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set is abstract;
   function To_Set
            (  Feature : Domain_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is abstract;
--
-- Independent_Features -- Independent domain-valued features
--
   package Independent_Features is
      new Generic_Independent (Domain_Feature_Object);
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Domain_Feature_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Domain_Feature_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a feature
--
   function To_Domain_Feature_Object_Ptr (Ptr : Feature_Object_Ptr)
      return Domain_Feature_Object_Ptr;
   pragma Inline (To_Domain_Feature_Object_Ptr);
--
-- Float_Handles -- To domain feature objects
--
   package Float_Handles is
      new Object.Handle
          (  Domain_Feature_Object'Class,
             Domain_Feature_Object_Ptr
          );
end Fuzzy.Feature.Generic_Domain_Float;
