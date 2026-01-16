--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Handle                           Autumn, 2005       --
--  Interface                                                         --
--                                Last revision :  19:05 25 Nov 2009  --
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

with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Handle is
--
-- Accumulate -- A fuzzy set
--
--    Feature - A handle to
--    Value   - The value to be accumulated
--
-- This  function  evaluates  the  accumulated  result of a fuzzy domain
-- subset.  Value is the subset. Constraint_Error is propagated when the
-- cardinality of Value differs from one of Feature or when  Feature  is
-- not a valid handle to a float feature.
--
-- Returns :
--
--    A dimensioned variable representing the accumulated result
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or value
--
   function Accumulate
            (  Feature : Feature_Handle;
               Value   : Fuzzy.Set
            )  return Variable_Measure;
--
-- Classify -- Fuzzify a value using the domain set
--
--    Feature - A handle to
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
--    Constraint_Error - Invalid handle or value
--    Unit_Error       - Incompatible units
--
   function Classify
            (  Feature : Feature_Handle;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification;
   function Classify
            (  Feature : Feature_Handle;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   function Classify
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   function Classify
            (  Feature : Feature_Handle;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Get_Scale -- Get the scale of the feature
--
--    Feature - A handle to it
--
-- The result is the scale of the feature  values  used  by  default  in
-- input and output.
--
-- Returns :
--
--    The feature scale
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Scale (Feature : Feature_Handle) return Measure;
--
-- Get_Scale_Text -- Get the default scale specification of the feature
--
--    Feature    - A handle to
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
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Scale_Text
            (  Feature    : Feature_Handle;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String;
--
-- Get_Unit -- Get SI dimension of the feature
--
--    Feature - A handle to
--
-- Returns :
--
--    The SI unit compatible with the feature units
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Unit (Feature : Feature_Handle) return Unit;
--
-- Get_Variable -- Get a linguistic variable by its domain value index
--
--    Feature - A handle to
--    Value   - The value (domain set value index 1..Cardinality)
--
-- Returns :
--
--    The dimensioned variable
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or value
--
   function Get_Variable
            (  Feature : Feature_Handle;
               Value   : Positive
            )  return Variable_Measure;
--
-- Is_Domain_Float -- Check if a handle refers a float feature
--
--    Feature - A handle to
--
-- Returns :
--
--    True if Feature is a valid float feature (handle)
--
   function Is_Domain_Float (Feature : Feature_Handle) return Boolean;
--
-- Is_Domain_Linguistic -- Check if a handle refers a feature with named
--                         linguistic variables
--
--    Feature - A handle to
--
-- Returns :
--
--    True if Feature variables are named
--
   function Is_Domain_Linguistic (Feature : Feature_Handle)
      return Boolean;
--
-- To_Set -- Get an intuitionistic set from the value
--
--    Feature - A handle to
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
--    Constraint_Error - Invalid handle or value
--    Unit_Error       - Incompatible units
--
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Feature : Feature_Handle;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;

private
   pragma Inline (Accumulate);
   pragma Inline (Classify);
   pragma Inline (Get_Scale);
   pragma Inline (Get_Scale_Text);
   pragma Inline (Get_Unit);
   pragma Inline (Get_Variable);
   pragma Inline (Is_Domain_Float);
   pragma Inline (Is_Domain_Linguistic);
   pragma Inline (To_Set);

end Fuzzy.Feature.Generic_Domain_Float.Generic_Handle;
