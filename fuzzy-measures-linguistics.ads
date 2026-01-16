--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Measures.Linguistics                  Luebeck            --
--  Interface                                      Autumn, 2006       --
--                                                                    --
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

with Units;   use Units;

with Fuzzy.Linguistics;
with Units.Base;

generic
   with package Fuzzy_Linguistics is
      new Fuzzy.Linguistics (Fuzzy_Floats);
package Fuzzy.Measures.Linguistics is
   package Fuzzy_Linguistics_Of renames Fuzzy_Linguistics;
   use Fuzzy_Linguistics_Of;
--
-- Variable_Measure -- A dimensioned linguistic variable
--
--    SI     - The dimension unit
--    Gain   - The value of the variable in the dimension unit
--    Offset - The dimension unit shift
--
   type Variable_Measure (SI : Unit := Units.Base.Unitless) is record
      Gain   : Variable;
      Offset : Number'Base := 0.0;
   end record;
--
-- Pair_Measure -- A measure - confidence level pair
--
   type Pair_Measure is record
      Value : Measure;
      Level : Confidence;
   end record;
--
-- Append -- One point to a linguistic variable
--
--    Var   - The variable
--    Value - The membership function argument
--    Level - The membership function value in the argument
--
-- This function is used to form the membership function of a linguistic
-- variable. It adds a new point to the  function.  Points  have  to  be
-- added in ascending order. It is allowed to define several points with
-- same argument (Value). The dimension units of Value  should  be  same
-- though  maybe  shifted  as ones of Var. Otherwise Constraint_Error is
-- propagated.
--
-- Exceptions :
--
--    Data_Error - The point is out of order
--    Unit_Error - Incompatible units
--
   procedure Append
             (  Var   : in out Variable_Measure;
                Value : Measure;
                Level : Confidence
             );
--
-- Convert -- Measure conversion
--
--    Value - The variable measurement to convert
--    Scale - The measure of the result
--
-- This function is used  to convert a variable measure  Value  to  the
-- measurement units specified by the parameter Scale. When  offsets  of
-- Value and Scale are same this is the null operation.
--
--    Convert (T, Celsius)  -- Temperature in Celsius degrees
--
-- Returns :
--
--    Scale equivalent of Value
--
-- Exceptions :
--
--    Unit_Error -- Item and Scale have different units
--
   function Convert (Value : Variable_Measure; Scale : Measure)
      return Variable_Measure;
   pragma Inline (Convert);
--
-- Empty -- Empty variable
--
--    Scale - The measure of the result
--
-- Returns :
--
--    An empty variable
--
   function Empty (Scale : Measure) return Variable_Measure;
   pragma Inline (Empty);
--
-- Empty -- Empty set
--
--    SI     - The dimension of
--    Offset - The offset
--
-- Returns :
--
--    Empty variable
--
   function Empty (SI : Unit; Offset : Number'Base := 0.0)
      return Variable_Measure;
   pragma Inline (Empty);
--
-- Erase -- Makes a variable an empty set
--
--    Var - The variable
--
   procedure Erase (Var : in out Variable_Measure);
   pragma Inline (Erase);
--
-- Equal -- Compare two variables (equality)
--
--    A   - The first argument
--    B   - The second argument
--    Eps - Tolerance level
--
-- One of the arguments can be a value-confidence pair. Two  points  are
-- assumed same if their confidence levels are same and values differ no
-- more than Eps.
--
-- Returns :
--
--    True if A and B are exactly same
--
   function Equal
            (  A, B : Variable_Measure;
               Eps  : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A   : Variable_Measure;
               B   : Pair_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A   : Pair_Measure;
               B   : Variable_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A   : Pair_Measure;
               B   : Pair_Measure;
               Eps : Number'Base := 0.0
            )  return Boolean;
--
-- Find -- Points contained by an interval
--
--    Var   - The variable
--    Span  - Interval of domain values
--    From  - The index of the last point left of the interval
--    To    - The index of the first point right of the interval
--
-- This  procedure  searches  for  the  membership  function points. The
-- parameter Span is an interval of domain values. After completion From
-- is the index of the point with the least domain value greater than or
-- equal to the lower bound of Span. When there is no such  point,  then
-- From  is  Get_Points_Number  (Var) + 1 . To is the index of the point
-- with the greatest domain value less than or equal to the upper  bound
-- of Span. When there is no such point, then To is 0.
--
-- Exceptions :
--
--    Unit_Error - Incompatible units of Value
--
   procedure Find
             (  Var   : Variable_Measure;
                Span  : Interval_Measure;
                From  : out Positive;
                To    : out Natural
             );
--
-- Get -- The membership function
--
--    Var   - The variable
--    Value - The value (abscissa)
--    Left  - The left limit of the membership function in Value
--    Min   - The lower bound of the membership function in Value
--    Max   - The upper bound of the membership function in Value
--    Right - The right limit of the membership function in Value
--
-- The  membership  function  in  Value  is characterized by four values
-- Left, Min, Max. Right. Note that always: Min <= Left, Right <= Max.
--
-- Exceptions :
--
--    Unit_Error - Incompatible units of Value
--
   procedure Get
             (  Var   : Variable_Measure;
                Value : Measure;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             );
--
-- Get_Point -- Get a point of the membership function
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Value - The value (abscissa)
--    Left  - The left limit of the membership function in Value
--    Min   - The lower bound of the membership function in Value
--    Max   - The upper bound of the membership function in Value
--    Right - The right limit of the membership function in Value
--
-- The  membership  function  in  Value  is characterized by four values
-- Left, Min, Max. Right. Note that always: Min <= Left, Right <= Max.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Get_Point
             (  Var   : Variable_Measure;
                Index : Positive;
                Value : out Measure;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             );
   pragma Inline (Get_Point);
--
-- Get_Points_Number -- Get number of points of the membership function
--
--    Var - The variable
--
-- Returns :
--
--    The number of distinct points of the membership function
--
   function Get_Points_Number (Var : Variable_Measure) return Natural;
   pragma Inline (Get_Points_Number);
--
-- Get_Span -- Between the first and the last points
--
--    Var - The variable
--
-- This function returns the interval of domain values between the value
-- of  the  first  membership  function  point and the value of the last
-- point.
--
-- Returns :
--
--    The interval
--
-- Exceptions :
--
--    Constraint_Error - Empty variable
--
   function Get_Span (Var : Variable_Measure) return Interval_Measure;
   pragma Inline (Get_Span);
--
-- Get_Value -- Get value of a membership function point
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--
-- Returns :
--
--    The domain value of the point
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   function Get_Value (Var : Variable_Measure; Index : Positive)
      return Measure;
   pragma Inline (Get_Value);
--
-- Get_Value -- Get SI value of a variable
--
--    Value - The variable
--
-- Returns :
--
--    SI equivalent of Value
--
   function Get_Value (Value : Variable_Measure) return Variable;
   pragma Inline (Get_Value);
--
-- Get_Value_As -- Get value measured in non-SI units
--
--    Value  - The variable
--    Scale  - The measure of the result
--
-- Returns :
--
--    Scale equivalent of Value as a variable
--
-- Exceptions :
--
--    Unit_Error -- Value and Scale have different units
--
   function Get_Value_As (Value : Variable_Measure; Scale : Measure)
      return Variable;
   pragma Inline (Get_Value_As);
--
-- Get_Unit -- Get unit
--
--    Value  - The variable
--
-- Returns :
--
--    SI component
--
   function Get_Unit (Value : Variable_Measure) return Unit;
   pragma Inline (Get_Unit);
--
-- Insert_Point -- Insert one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number + 1
--    Value - The value (abscissa)
--    Left  - The left limit of the membership function in Value
--    Min   - The lower bound of the membership function in Value
--    Max   - The upper bound of the membership function in Value
--    Right - The right limit of the membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure  inserts  a  the  membership  function  point  at  the
-- position  specified  by its Index. The value of Index shall be in the
-- range  1..Get_Points_Number  (Var) + 1. The parameter Value specifies
-- the  domain  value  of the point. It shall be greater than one of the
-- point before and less than one of the  point  after  it.  The  latter
-- point   has   the   position   Index   before   insertion.  Otherwise
-- Constraint_Error is propagated. When the parameter Max is  less  than
-- Left  or  Right  it  is  ignored.  The  parameter Min is ignored when
-- greater  than  Left  or Right. When Purge is true, the inserted point
-- and  its  immediate  neighbours  are  checked  for  being  redundant.
-- Eventually they can be removed.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index or Value
--    Unit_Error       - Incompatible units of Value
--
   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             );
--
-- Insert_Point -- Insert one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Value - The value (abscissa)
--    Left  - The left limit of the membership function in Value
--    Right - The right limit of the membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure changes the membership function  by  inserting  a  new
-- point  at  the  position specified by its Index. This is a simplified
-- version without the parameters Min and Max.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index or Value
--    Unit_Error       - Incompatible units of Value
--
   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             );
--
-- Insert_Point -- Insert one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Value - The value (abscissa)
--    Level - The membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure changes the membership function  by  inserting  a  new
-- point at the position specified by its Index.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--    Unit_Error       - Incompatible units of Value
--
   procedure Insert_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Level : Confidence;
                Purge : Boolean := True
             );
   pragma Inline (Insert_Point);
--
-- Is_Empty -- Check if a variable is an empty set
--
--    Var - The variable
--
-- Returns :
--
--    True if the membership function is everywhere False
--
   function Is_Empty (Var : Variable_Measure) return Boolean;
   pragma Inline (Is_Empty);
--
-- Is_In -- Membership / subset test
--
--    A - Measure, interval/fuzzy/variable measure
--    B - Measure, interval/fuzzy/variable measure
--
-- At  least  one  argument  should be a variable measure. Arguments can
-- have any units.
--
-- Returns :
--
--    (P(B|A), N(B|A))
--
   function Is_In (A, B : Variable_Measure) return Fuzzy_Boolean;
   function Is_In
            (  A : Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean;
   function Is_In
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean;
   function Is_In
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Fuzzy_Boolean;
   function Is_In
            (  A : Variable_Measure;
               B : Measure
            )  return Fuzzy_Boolean;
   function Is_In
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Fuzzy_Boolean;
   function Is_In
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Fuzzy_Boolean;
--
-- Is_Interval -- Check if a variable is an interval
--
--    Var - The variable
--
-- This function returns True if the membership function of Var is 1  on
-- a finite interval and 0 outside it.
--
-- Returns :
--
--    True if it is an interval
--
   function Is_Interval (Var : Variable_Measure) return Boolean;
   pragma Inline (Is_Interval);
--
-- Is_Singleton -- Check if a variable is an singleton
--
--    Var   - The variable
--
-- This function returns True if the membership function of Vae is 1 in
-- one point and 0 elsewhere.
--
-- Returns :
--
--    True if is a singleton
--
   function Is_Singleton (Var : Variable_Measure) return Boolean;
   pragma Inline (Is_Singleton);
--
-- Move_Point -- Move a point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number + 1
--    Value - The value (abscissa)
--    Purge - Remove redundant membership function points
--
-- This procedure moves a the membership function point at the  position
-- specified  by  its  Index.  The  point  is  moved to the domain value
-- specified  by  the  parameter  Value.  Index  shall  be  in the range
-- 1..Get_Points_Number  (Var). The parameter Value specifies the domain
-- value  of the point. It shall be greater than one of the point before
-- and  less  than  one  of  the  point  after  it  if  any.   Otherwise
-- Constraint_Error  is  propagated.  When Purge is true the moved point
-- and  its two adjacent points are checked for redundancy. Consequently
-- they can be removed.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index or Value
--    Unit_Error       - Incompatible units of Value
--
   procedure Move_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Value : Measure;
                Purge : Boolean := True
             );
   pragma Inline (Move_Point);
--
-- Necessity -- Conditional necessity
--
--    A - The first argument
--    B - The second argument (condition)
--
-- With  one  argument the condition is assumed to be the domain set. At
-- least one argument should be a variable.
--
-- Returns :
--
--    N(A|B)
--
   function Necessity (A : Variable_Measure) return Confidence;
   function Necessity (A, B : Variable_Measure) return Confidence;
   function Necessity
            (  A : Measure;
               B : Variable_Measure
            )  return Confidence;
   function Necessity
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Confidence;
   function Necessity
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Confidence;
   function Necessity
            (  A : Variable_Measure;
               B : Measure
            )  return Confidence;
   function Necessity
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Confidence;
   function Necessity
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Confidence;
--
-- Normalize -- Shift removing
--
--    Value - The variable
--
-- This  function  is  used  to  convert  Value  to  to  its   unshifted
-- equivalent.
--
-- Returns :
--
--    Unshifted equivalent of Value
--
   function Normalize (Value : Variable_Measure)
      return Variable_Measure;
   pragma Inline (Normalize);
--
-- Possibility -- Conditional possibility
--
--    A - The first argument
--    B - The second argument (condition)
--
-- With  one  argument the condition is assumed to be the domain set. At
-- least one argument should be a variable.
--
-- Returns :
--
--    P(A|B)
--
   function Possibility (A : Variable_Measure) return Confidence;
   function Possibility (A, B : Variable_Measure) return Confidence;
   function Possibility
            (  A : Measure;
               B : Variable_Measure
            )  return Confidence;
   function Possibility
            (  A : Interval_Measure;
               B : Variable_Measure
            )  return Confidence;
   function Possibility
            (  A : Fuzzy_Measure;
               B : Variable_Measure
            )  return Confidence;
   function Possibility
            (  A : Variable_Measure;
               B : Measure
            )  return Confidence;
   function Possibility
            (  A : Variable_Measure;
               B : Interval_Measure
            )  return Confidence;
   function Possibility
            (  A : Variable_Measure;
               B : Fuzzy_Measure
            )  return Confidence;
--
-- Purge -- The membership function of a variable
--
--    Var       - The variable
--    Keep_Ends - Prevent the end points from purging
--
-- The procedure checks the points of the membership function for  being
-- redundant.  These points are always such that the membership function
-- is continuous and linear in the point:
--
--          |___| (end)      | | |___        |  |  ___
--         /|   |            | |/|           |  | /
--    ____/ |   |            | | |     (end) |__|/
--          |   |         ___|/| |           |  |
--             (x)            (x)           (x)
--
-- The parameter Keep_Ends when set to true prevents the first  and  the
-- last points of the membership function from being removed.
--
   procedure Purge
             (  Var       : in out Variable_Measure;
                Keep_Ends : Boolean := False
             );
--
-- Remove_Point -- Delete one point from the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Purge - Remove redundant membership function points
--
-- This  procedure changes the membership function by removing the point
-- specified by its Index. When Purge is true two adjacent points of the
-- removed  one  are  checked  for  redundancy.  Eventually  they can be
-- removed as well.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Remove_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Purge : Boolean := True
             );
   pragma Inline (Remove_Point);
--
-- Set_Point -- Change one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Left  - The left limit of the membership function in Value
--    Min   - The lower bound of the membership function in Value
--    Max   - The upper bound of the membership function in Value
--    Right - The right limit of the membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure changes the membership function in the point specified
-- by its Index. When the parameter Max is less than Left or Right it is
-- ignored.  The  parameter  Min  is  ignored  when greater than Left or
-- Right. When Purge is true the point  and its two immediate neighbours
-- are checked for being redundant and eventually can be removed.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Left  : Confidence;
                Min   : Confidence;
                Max   : Confidence;
                Right : Confidence;
                Purge : Boolean := True
             );
--
-- Set_Point -- Change one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Left  - The left limit of the membership function in Value
--    Right - The right limit of the membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure changes the membership function in the point specified
-- by its Index. This is a simplified version without the parameters Min
-- and Max.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Left  : Confidence;
                Right : Confidence;
                Purge : Boolean := True
            );
--
-- Set_Point -- Change one point of the variable
--
--    Var   - The variable
--    Index - Of the point 1..Get_Points_Number
--    Level - The membership function in Value
--    Purge - Remove redundant membership function points
--
-- This procedure changes the membership function in the point specified
-- by its Index.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Set_Point
             (  Var   : in out Variable_Measure;
                Index : Positive;
                Level : Confidence;
                Purge : Boolean := True
             );
   pragma Inline (Set_Point);
--
-- Shift -- Non-destructive shift
--
--    Value  - A variable to shift
--    Shift  - The shift
--
-- This function is used to convert Value to its shifted equivalent.
--
-- Returns :
--
--    Shifted equivalent of Value
--
   function Shift
            (  Value : Variable_Measure;
               Shift : Number'Base
            )  return Variable_Measure;
   pragma Inline (Shift);
--
-- To_Interval_Measure -- Conversion
--
--    Value - A variable to convert
--
-- Returns :
--
--    The corresponding interval
--
-- Exceptions :
--
--    Constraint_Error - Value is not an interval
--
   function To_Interval_Measure (Value : Variable_Measure)
      return Interval_Measure;
   pragma Inline (To_Interval_Measure);
--
-- To_Measure -- Conversion
--
--    Value - A variable to convert
--
-- Returns :
--
--    The corresponding measure
--
-- Exceptions :
--
--    Constraint_Error - Value is not a singleton (number)
--
   function To_Measure (Value : Variable_Measure) return Measure;
   pragma Inline (To_Measure);
--
-- To_Variable_Measure -- Conversion
--
--    Value - A number, interval or a fuzzy number
--
-- Returns :
--
--    The corresponding variable
--
   function To_Variable_Measure (Value : Number)
      return Variable_Measure;
   function To_Variable_Measure (Value : Interval)
      return Variable_Measure;
   function To_Variable_Measure (Value : Fuzzy_Float)
      return Variable_Measure;
   function To_Variable_Measure (Value : Variable)
      return Variable_Measure;
   function To_Variable_Measure (Value : Measure)
      return Variable_Measure;
   function To_Variable_Measure (Value : Interval_Measure)
      return Variable_Measure;
   function To_Variable_Measure (Value : Fuzzy_Measure)
      return Variable_Measure;
   pragma Inline (To_Variable_Measure);
--
-- Trim -- The membership function of a variable
--
--    Var - The variable
--
-- The  procedure  removes end points of the membership function if they
-- don't influence its values anywhere.
--
   procedure Trim (Var : in out Variable_Measure);
--
-- +, -, *, / -- By a scalar
--
--    Left  - Variable
--    Right - Number
--
-- The scalar in a multiplicative operation may be either a number or  a
-- dimensioned number.
--
-- Returns :
--
--    The result of the operation
--
-- Exceptions :
--
--    Constraint_Error - Unit power overflow
--    Unit_Error       - Incompatible units
--
   function "+" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure;
   function "-" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure;
   function "*" (Left : Variable_Measure; Right : Number)
      return Variable_Measure;
   function "*" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure;
   function "*" (Left : Variable; Right : Measure)
      return Variable_Measure;
   function "/" (Left : Variable_Measure; Right : Number)
      return Variable_Measure;
   function "/" (Left : Variable_Measure; Right : Measure)
      return Variable_Measure;
   function "/" (Left : Variable; Right : Measure)
      return Variable_Measure;
--
-- = -- Equality
--
--   A - Dimensioned number, interval, fuzzy number, variable
--   B - Dimensioned number, interval, fuzzy number, variable
--
-- At least one argument should be a dimensioned variable.
--
-- Returns :
--
--    (P(A|B), N(A|B) and N(B|A))
--
   function "=" (A, B : Variable_Measure) return Fuzzy_Boolean;
   function "=" (A : Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "=" (A : Interval_Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "=" (A : Fuzzy_Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "=" (A : Variable_Measure; B : Measure)
      return Fuzzy_Boolean;
   function "=" (A : Variable_Measure; B : Interval_Measure)
      return Fuzzy_Boolean;
   function "=" (A : Variable_Measure; B : Fuzzy_Measure)
      return Fuzzy_Boolean;
   pragma Inline ("=");
--
-- /= -- Inequality
--
--   A - Dimensioned number, interval, fuzzy number, variable
--   B - Dimensioned number, interval, fuzzy number, variable
--
-- At least one argument should be a dimensioned variable.
--
-- Returns :
--
--    not (A = B)
--
   function "/=" (A, B : Variable_Measure) return Fuzzy_Boolean;
   function "/=" (A : Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "/=" (A : Interval_Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "/=" (A : Fuzzy_Measure; B : Variable_Measure)
      return Fuzzy_Boolean;
   function "/=" (A : Variable_Measure; B : Measure)
      return Fuzzy_Boolean;
   function "/=" (A : Variable_Measure; B : Interval_Measure)
      return Fuzzy_Boolean;
   function "/=" (A : Variable_Measure; B : Fuzzy_Measure)
      return Fuzzy_Boolean;
   pragma Inline ("/=");
--
-- & -- Appending a point to a lingquistic variable
--
--    Left  - The first argument (either a variable or a pair)
--    Right - The second argument (a pair)
--
-- These  functions  are  equivalent  to Append. When Left and Right are
-- pairs  then  both  shall  have exactly same units. Otherwise Left and
-- Right can be differently shifted.
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Data_Error - The point provided by Right is out of order
--    Unit_Error - Incompatible units
--
   function "&" (Left : Pair_Measure; Right : Pair_Measure)
      return Variable_Measure;
   function "&" (Left : Variable_Measure; Right : Pair_Measure)
      return Variable_Measure;
--
-- and -- Intersection
--
--    A : The first argument
--    B : The second argument
--
-- Returns :
--
--    A and B
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "and" (A, B : Variable_Measure) return Variable_Measure;
   function "and" (A : Variable_Measure; B : Confidence)
      return Variable_Measure;
   function "and" (A : Confidence; B : Variable_Measure)
      return Variable_Measure;
   pragma Inline ("and");
--
-- not -- Complement
--
--    A : The argument
--
-- Returns :
--
--    not A
--
   function "not" (A : Variable_Measure) return Variable_Measure;
   pragma Inline ("not");
--
-- or -- Union
--
--    A : The first argument
--    B : The second argument
--
-- Returns :
--
--    A or B
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "or" (A, B : Variable_Measure) return Variable_Measure;
   function "or" (A : Variable_Measure; B : Confidence)
      return Variable_Measure;
   function "or" (A : Confidence; B : Variable_Measure)
      return Variable_Measure;
   pragma Inline ("or");
--
-- xor -- Distance
--
--    A : The first argument
--    B : The second argument
--
-- Returns :
--
--    A xor B
--
-- Exceptions :
--
--    Unit_Error - Incompatible units
--
   function "xor" (A, B : Variable_Measure) return Variable_Measure;
   function "xor" (A : Variable_Measure; B : Confidence)
      return Variable_Measure;
   function "xor" (A : Confidence; B : Variable_Measure)
      return Variable_Measure;
   pragma Inline ("xor");

end Fuzzy.Measures.Linguistics;
