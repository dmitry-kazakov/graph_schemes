--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Float                            Summer, 2002       --
--  Interface                                                         --
--                                Last revision :  14:48 30 May 2014  --
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
--  This generic package is used to create floating-point feature types.
--
--  1. Input  of  the  floating-point features. The ranges of the values
--     are  recognized  in  the interval and slice formats.
--
--     1.1. The interval format is:
--
--          [<from>,<to>]  [<from>,<to>[  ]<from>,<to>]  ]<from>,<to>[
--
--          In the interval format either both bounds are dimensioned or
--          a dimension can be specified following the interval.  Spaces
--          and tabs can be used as separators. For examples:
--
--          [1m, 2m]
--          [1,2] m
--
--     1.2. The slice format  is:
--
--          <from>..<to>   <from>
--
--          In  the  slice  format  dimensioned  can be both bounds, the
--          upper  bound  or  none  of  them.  When  the  upper bound is
--          dimensioned then this dimension  is  applied  to  the  lower
--          bound.
--
--     When the dimension is omitted it is one  of  the  feature  scale.
--     Except  for  the  case  when  the  dimension  is  required by the
--     Get_Units field of the input parameters.  No  dimension  is  ever
--     required if the feature scale is 1 SI. The parameter  Quote_Units
--     when  true  requires any  dimension specification to be put in []
--     brackets.  For  example:  [1,  2]  [m]. This can be used to avoid
--     ambiguities in space separated input.
--
--  2. Output of the floating-point features. The feature values  always
--     output in  the  interval  format.  The  dimension  is  output  if
--     Put_Units field of the output parameter is set.  The  bounds  are
--     output in the feature scale units  if  Use_SI  does  not  require
--     otherwise. SI units also forced when the feature  scale  was  not
--     specified as a string during feature creation. In all cases  when
--     SI units are forced, the dimension 1 SI is always omitted. When a
--     dimension specification is used it is placed after  the  interval
--     separated  by  one  space. For example:
--
--        [1.034, 2.901] m
--
--     The parameter Quote_Units places [] brackets around all dimension
--     specifications. The parameters Put_Plus, Abs_Small and  Rel_Small
--     control the output of the bounds.
--
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Units;                 use Units;

generic
package Fuzzy.Feature.Generic_Domain_Float.Generic_Float is
--
-- Float_Class -- The prefix of all rectangualar float feature classes
--
   Float_Class : constant String := Feature_Class & "Float." & Suffix;
--
-- Create -- Create a floating-point feature
--
--    Name        - Of the feature
--    Cardinality - Of the feature (number of elementary intervals)
--    From        - The lower boundary of the feature values range
--    To          - The upper boundary
--    Scale       - The scale of the feature values
--  [ Mode ]      - Scale character set
--
-- The parameters From and To determine the range of the feature domain.
-- The feature domain is the interval [From..To], the of union of domain
-- elmentary   intervals   [Li,  Ui],  i=1..Cardinality.  Li+1=Ui.  Each
-- elementary interval has the width (To  -  From)  /  Cardinality.  The
-- parameter  Scale  is the feature values scale. It is specified either
-- as  a measure or as a string which is then parsed. The parameter Mode
-- is the character set used for parsing a string Scale. The scale shall
-- have positive gain, Unit_Error is propagated otherwise.
--
-- Returns :
--
--    The handle to the newly created feature
--
-- Exceptions :
--
--    Constraint_Error - From = To and Cardinality is not 1
--    End_Error        - Empty domain (To < From)
--    Unit_Error       - Wrong Scale
--
   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Scale       : Measure := Float_Measures.Np
            )  return Feature_Handle;
   function Create
            (  Name        : String;
               Cardinality : Positive;
               From        : Domain_Float;
               To          : Domain_Float;
               Scale       : String;
               Mode        : Code_Set := UTF8_Set
            )  return Feature_Handle;
--
-- Get_From -- Get the lower bound of the values
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or object
--
   function Get_From (Feature : Feature_Handle) return Measure;
   function Get_From (Feature : Feature_Object'Class)
      return Measure;
--
-- Get_Interval -- Get interval
--
--    Feature - Object or a handle to it
--    Value   - The interval number 1..Cardinality
--
-- Returns :
--
--    The interval corresponding to Index
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or value
--
   function Get_Interval
            (  Feature : Feature_Handle;
               Value   : Positive
            )  return Interval_Measure;
   function Get_Interval
            (  Feature : Feature_Object'Class;
               Value   : Positive
            )  return Interval_Measure;
--
-- Get_Span -- Get the feature values range
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The interval of feature values
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or object
--
   function Get_Span (Feature : Feature_Handle)
      return Interval_Measure;
   function Get_Span (Feature : Feature_Object'Class)
      return Interval_Measure;
--
-- Get_To -- Get the upper bound of the values
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    The bound
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or object
--
   function Get_To (Feature : Feature_Handle) return Measure;
   function Get_To (Feature : Feature_Object'Class) return Measure;
--
-- Is_Float -- Check if a handle refers an float feature
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature is a valid float feature (handle)
--
   function Is_Float (Feature : Feature_Handle) return Boolean;
   function Is_Float (Feature : Feature_Object'Class) return Boolean;

private
   pragma Inline (Get_From);
   pragma Inline (Get_Interval);
   pragma Inline (Get_Span);
   pragma Inline (Get_To);
   pragma Inline (Is_Float);

   use Independent_Features;
--
-- Float_Feature -- The feature object
--
   type Float_Feature_Object
        (  Cardinality : Positive;
           SI          : Unit;
           Unit_Length : Natural
        )  is new Independent_Feature_Object (Cardinality) with
   record
      From      : Domain_Float;
      To        : Domain_Float;
      Gain      : Domain_Float'Base;
      Offset    : Domain_Float'Base;
      Dimension : String (1..Unit_Length);
   end record;
--
-- Accumulate -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Accumulate
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measures.Variable_Measure;
--
-- Classify -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification;
   overriding
   function Classify
            (  Feature : Float_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Feature : Float_Feature_Object)
      return String;
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Float_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Scale -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale (Feature : Float_Feature_Object)
      return Measure;
--
-- Get_Scale_Text -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Scale_Text
            (  Feature    : Float_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String;
--
-- Get_Unit -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Unit (Feature : Float_Feature_Object)
      return Unit;
--
-- Get_Variable -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Get_Variable
            (  Feature : Float_Feature_Object;
               Value   : Positive
            )  return Variable_Measure;
--
-- Is_Domain_Linguistic -- Overrides
--                         Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function Is_Domain_Linguistic (Feature : Float_Feature_Object)
      return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Float_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Float_Feature_Object
             );
--
-- To_Set -- Overrides Fuzzy.Feature.Generic_Domain_Float...
--
   overriding
   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   overriding
   function To_Set
            (  Feature : Float_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;

   Restore_Ptr : constant Object.Archived.Restore := Restore'Access;

end Fuzzy.Feature.Generic_Domain_Float.Generic_Float;
