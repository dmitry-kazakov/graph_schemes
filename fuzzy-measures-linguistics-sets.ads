--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures.Linguistics.                 Luebeck            --
--        Sets                                     Autumn, 2006       --
--  Interface                                                         --
--                                Last revision :  14:34 08 Oct 2006  --
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

with Fuzzy.Intuitionistic;    use Fuzzy.Intuitionistic;
with Fuzzy.Linguistics.Sets;

generic
   with package Fuzzy_Linguistic_Sets is new Fuzzy_Linguistics.Sets;
package Fuzzy.Measures.Linguistics.Sets is
   package Fuzzy_Linguistic_Sets_Of renames Fuzzy_Linguistic_Sets;
   use Fuzzy_Linguistic_Sets_Of;
--
-- Set_Measure -- A set of dimensioned lingustic variables
--
--    SI     - The dimension unit
--    Gain   - The set of the variables
--    Offset - The dimension unit shift
--
   type Set_Measure (SI : Unit := Units.Base.Unitless) is record
      Gain   : Linguistic_Set;
      Offset : Number'Base := 0.0;
   end record;
--
-- Defuzzification_Result -- A result of defuzzification process
--
--    Class - The type of
--
   type Defuzzification_Result
        (  Class : Defuzzification_Result_Class
        )  is
   record
      case Class is
         when Empty     => null;
         when Singleton => Singleton : Measure;
         when Segment   => Segment   : Interval_Measure;
         when Subset    => Subset    : Variable_Measure;
      end case;
   end record;
--
-- Accumulate -- A combination of weighted variables from a set
--
--    Set          - The set of linguistic variables
--    Distribution - A fuzzy set (weights)
--
-- The  result  is  a  linguistic  variable  evaluated   as   union   of
-- intersections Set (i) and Distribution (i). The result  is  empty  if
-- Set  is empty. Constraint_Error is propagated if the cardinalities of
-- Set and the domain set of Distribution differ.
--
-- Result :
--
--    Accumulated result of the set's dimension
--
-- Exceptions :
--
--    Constraint_Error - Cardinality error
--
   function Accumulate
            (  Set          : Set_Measure;
               Distribution : Fuzzy.Set
            )  return Variable_Measure;
--
-- Add -- A variable into a set
--
--    Set   - The set to be modified
--    Name  - The name of the variable
--    Value - The variable
--
-- The variable Value is  added  to  the  end  of  Set  under  the  name
-- specified  by  the  parameter  Name. The dimension of Value should be
-- compatible  with  the  dimension  of  Set.  Otherwise  Unit_Error  is
-- propagated. Value and Set may have differently shifted units.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name
--    Name_Error       - The name is already used by another variable
--    Unit_Error       - Incompatible units
--
   procedure Add
             (  Set   : in out Set_Measure;
                Name  : String;
                Value : Variable_Measure
             );
--
-- Classify -- A value
--
--    Set   - A set of linguistic variables
--    Value - The value to be classified (fuzzified)
--
-- The result is a classification of Value in terms of Set elements. The
-- value can be a measure, interval  measure,  fuzzy  measure,  variable
-- measure.
--
-- Returns :
--
--    The classification of Value
--
-- Exceptions :
--
--    Constraint_Error - Empty set
--
   function Classify
            (  Set   : Set_Measure;
               Value : Measure
            )  return Classification;
   function Classify
            (  Set   : Set_Measure;
               Value : Interval_Measure
            )  return Classification;
   function Classify
            (  Set   : Set_Measure;
               Value : Fuzzy_Measure
            )  return Classification;
   function Classify
            (  Set   : Set_Measure;
               Value : Variable_Measure
            )  return Classification;
   pragma Inline (Classify);
--
-- Defuzzify -- A classification of a lingustic set
--
--    Set   - The set of linguistic variables
--    Value - A classification on Set
--
-- The  result  of  the  function  is  defuzzified  Value, i.e. a set of
-- dimensioned  numbers  which  being  fuzzified  (see  Classify)  might
-- produce Value. Constraint_Error is propagated when the cardinality of
-- Value differs from one of Set.
--
-- Returns :
--
--    Defuzzified Value
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality
--
   function Defuzzify
            (  Set   : Set_Measure;
               Value : Classification
            )  return Defuzzification_Result;
--
-- Empty -- Empty set
--
--    Scale - Of the set
--
-- Returns :
--
--    Empty set
--
   function Empty (Scale : Measure) return Set_Measure;
   pragma Inline (Empty);
--
-- Empty -- Empty set
--
--    SI     - The dimension of
--    Offset - The offset
--
-- Returns :
--
--    Empty set
--
   function Empty (SI : Unit; Offset : Number'Base := 0.0)
      return Set_Measure;
   pragma Inline (Empty);
--
-- Erase -- Make a set empty
--
--    Set - To be erased
--
   procedure Erase (Set : in out Set_Measure);
--
-- Equal -- Compare two sets (equality)
--
--    Left  - The first argument
--    Right - The second argument
--    Eps   - Tolerance level
--
-- Two  sets  are  equal  if  they  have  same dimension, same number of
-- variables,   the   corresponding   variables    have    same    names
-- (case-insensitive comparison) and the variables are equivalent up  to
-- Eps. Left and Right may have differently shifted units.
--
-- Returns :
--
--    True if Left and Right are same
--
   function Equal
            (  Left, Right : Set_Measure;
               Eps         : Number'Base := 0.0
            )  return Boolean;
--
-- Get -- Get a linguistic variable by its domain value index
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    Constraint_Error - There is no such variable
--
   function Get
            (  Set   : Set_Measure;
               Index : Positive
            )  return Variable_Measure;
   pragma Inline (Get);
--
-- Get -- Get index of a variable by its name
--
--    Set  - A set of linguistic variables
--    Name - Of the variable
--
-- Returns :
--
--    The domain value index of Name
--
-- Exceptions :
--
--    End_Error - No variable matched
--
   function Get
            (  Set  : Set_Measure;
               Name : String
            )  return Positive;
   pragma Inline (Get);
--
-- Get -- Get a linguistic variable by its domain value name
--
--    Set  - A set of linguistic variables
--    Name - Of the variable
--
-- Returns :
--
--    The variable
--
-- Exceptions :
--
--    End_Error - No variable matched
--
   function Get (Set : Set_Measure; Name : String)
      return Variable_Measure;
   pragma Inline (Get);
--
-- Get -- Get index of a variable by its name
--
--    Source  - String to parse
--    Pointer - Starting string position
--    Set     - A set of linguistic variables
--    Index   - Of the variable
--
-- This procedure parses the string Source. If a name  of  a  linguistic
-- variable from  Set  is  successfully  matched  starting  from  Source
-- (Pointer),  Pointer is advanced to the position following the matched
-- name and Index is set to respond to the domain value associated  with
-- the variable.
--
-- Exceptions :
--
--    End_Error    - No variable matched
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Set_Measure;
                Index   : out Positive
             );
   pragma Inline (Get);
--
-- Get_Cardinality -- Get number of elements
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    The number of variables in the set
--
   function Get_Cardinality (Set : Set_Measure) return Natural;
   pragma Inline (Get_Cardinality);
--
-- Get_Domain -- Get the domain set description
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    Pointer to the domain set description
--
-- Exceptions :
--
--    Constraint_Error - An empty set
--
   function Get_Domain (Set : Set_Measure)
      return Domain_Description_Ptr;
   pragma Inline (Get_Domain);
--
-- Get_Name -- Get name of a linguistic variable
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Returns :
--
--    The name of the variable
--
-- Exceptions :
--
--    Constraint_Error - There is no such variable
--
   function Get_Name (Set : Set_Measure; Index : Positive)
      return String;
   pragma Inline (Get_Name);
--
-- Get_Value -- Get SI value of a set
--
--    Set - A set of linguistic variables
--
-- The result has the same number of variables with the same names.  The
-- variables of the result are of the Variable type.
--
-- Returns :
--
--    SI equivalent of Set
--
   function Get_Value (Set : Set_Measure) return Linguistic_Set;
--
-- Get_Value_As -- Get value measured in non-SI units
--
--    Set   - A set of linguistic variables
--    Scale - The measure of the result
--
-- The result has the same number of variables with the same names.  The
-- variables of the result have the type Variable. They are  numerically
-- equivalent  to  the  counterparts  in  Set  in  the unit Scale, as if
-- Get_Value_As were applied to each of the variables of Set.
--
-- Returns :
--
--    Scale equivalent of Set
--
-- Exceptions :
--
--    Unit_Error -- Set and Scale have different units
--
   function Get_Value_As (Set : Set_Measure; Scale : Measure)
      return Linguistic_Set;
--
-- Get_Unit -- Get unit
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    SI component
--
   function Get_Unit (Set : Set_Measure) return Unit;
   pragma Inline (Get_Unit);
--
-- Insert -- A variable into a set
--
--    Set   - The set to be modified
--    Index - The value (domain set value index 1..)
--    Name  - The name of the variable
--    Value - The variable
--
-- The variable Value is inserted into Set under the name  specified  by
-- the parameter Name and domain value Index.
--
-- Exceptions :
--
--    Constraint_Error - Invalid name or illegal index
--    Name_Error       - The name is already used by another variable
--    Unit_Error       - Incompatible units
--
   procedure Insert
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String;
                Value : Variable_Measure
             );
   pragma Inline (Insert);
--
-- Is_Empty -- Empty set test
--
--    Set - A set of linguistic variables
--
-- Returns :
--
--    True if the set is empty
--
   function Is_Empty (Set : Set_Measure) return Boolean;
   pragma Inline (Is_Empty);
--
-- Move -- Move a variable in the set
--
--    Set  - A set of linguistic variables
--    From - The value (domain set value index 1..)
--    To   - The new value
--
-- This procedure moves the variable from the domain set index  From  to
-- To. The indices of the variables in the range From..To will change as
-- well.
--
-- Exceptions :
--
--    Contraint_Error - Wrong indices
--
   procedure Move
             (  Set  : in out Set_Measure;
                From : Positive;
                To   : Positive
             );
   pragma Inline (Move);
--
-- Remove -- Remove variable from the set
--
--    Set   - A set of linguistic variables
--    Index - The value (domain set value index 1..)
--
-- Exceptions :
--
--    Contraint_Error - Wrong index
--
   procedure Remove (Set : in out Set_Measure; Index : Positive);
   pragma Inline (Remove);
--
-- Remove -- Remove variable from the set by its name
--
--    Set  - A set of linguistic variables
--    Name - To be removed
--
-- If the Name is not in the set, nothing happens.
--
   procedure Remove (Set : in out Set_Measure; Name : String);
   pragma Inline (Remove);
--
-- Rename -- The domain value
--
--    Set   - The set to be modified
--    Index - The value (domain set value index 1..)
--    Name  - The name of the variable
--
-- This procedure renames the domain value specified by Index.
--
-- Exceptions :
--
--    Contraint_Error - Wrong index or name
--    Name_Error      - The name is already used by another variable
--
   procedure Rename
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String
             );
   pragma Inline (Rename);
--
-- Replace -- Replace a variable
--
--    Set     - The set to be modified
--    Index   - The value (domain set value index 1..)
--  [ Name  ] - The new name of the domain value
--    Value   - The new domain value
--
-- This  procedure  replaces  the domain value of a variable of the set.
-- When the parameter Name is not specified the domain value name is not
-- changed. Otherwise both the name and the value are changed.
--
-- Exceptions :
--
--    Contraint_Error - Wrong index or name
--    Name_Error      - The name is already used by another variable
--    Unit_Error      - Incompatible units
--
   procedure Replace
             (  Set   : in out Set_Measure;
                Index : Positive;
                Name  : String;
                Value : Variable_Measure
             );
   procedure Replace
             (  Set   : in out Set_Measure;
                Index : Positive;
                Value : Variable_Measure
             );
   pragma Inline (Replace);
--
-- Swap -- Variables in the set
--
--    Set     - The set to be modified
--    Index_1 - The value (domain set value index 1..)
--    Index_2 - The value (domain set value index 1..)
--
-- This  procedure  swaps  two  variables of Set specified by the domain
-- value indices. Nothing happens when Index_1 = Index_2.
--
-- Exceptions :
--
--    Constraint_Error - Illegal indices
--
   procedure Swap
             (  Set     : in out Set_Measure;
                Index_1 : Positive;
                Index_2 : Positive
             );
   pragma Inline (Swap);
--
-- To_Set -- Convert a value to a subset of linguistic variables
--
--    Set   - A set of linguistic variables
--    Value - The value to be converted
--
-- The result is an estimation of Value in terms of  Set  elements.  The
-- value  can  be a measure, interval measure, fuzzy measure or variable
-- measure.
--
-- Returns :
--
--    The estimation of Value
--
-- Exceptions :
--
--    Constraint_Error - Empty set
--
   function To_Set
            (  Set   : Set_Measure;
               Value : Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Set_Measure;
               Value : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Set_Measure;
               Value : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Set;
   function To_Set
            (  Set   : Set_Measure;
               Value : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set;
   pragma Inline (To_Set);

end Fuzzy.Measures.Linguistics.Sets;
