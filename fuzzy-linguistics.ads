--                                                                    --
--  package Fuzzy.Linguistics       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2003       --
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
--
--  This generic  package  provides  linguistic  variables.  The  formal
--  generic parameter is an package implementing fuzzy floats,  i.e.  an
--  instantiation of Fuzzy.Floats:
--
--     Fuzzy_Floats - An instance of Fuzzy.Floats
--
--  A  linguistic  variable  is  a  fuzzy  subset  of  real numbers. The
--  membership function of a  linguistic  variable  consists  of  linear
--  segments and singletons. It is defined by a finite set of points. In
--  each point there are values defined:
--
--     (o)  left limit
--     (o)  right limit
--     (o)  possibility at the point
--     (o)  necessity at the point
--
--  This allows to represent  functions built as a union of  trapezoids,
--  rectangles and singular points.
--
with Fuzzy.Logic;  use Fuzzy.Logic;

with Fuzzy.Floats;
with Generic_Set;

generic
   with package Fuzzy_Floats is new Fuzzy.Floats (<>);
package Fuzzy.Linguistics is
   package Fuzzy_Floats_Of renames Fuzzy_Floats;
   use Fuzzy_Floats_Of;
   use Float_Intervals_Of;
--
-- Variable -- A linguistic variable
--
   type Variable is private;
--
-- Pair -- A value - confidence level pair
--
   type Pair is record
      Value : Number;
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
-- same argument (Value).
--
-- Exceptions :
--
--    Data_Error - The point is out of order
--
   procedure Append
             (  Var   : in out Variable;
                Value : Number;
                Level : Confidence
             );
--
-- Empty -- Empty variable
--
   function Empty return Variable;
--
-- Erase -- Makes a variable an empty set
--
--    Var - The variable
--
   procedure Erase (Var : in out Variable);
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
--    True if A and B are same up to Eps
--
   function Equal
            (  A, B : Variable;
               Eps  : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A    : Variable;
               B    : Pair;
               Eps  : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A    : Pair;
               B    : Variable;
               Eps  : Number'Base := 0.0
            )  return Boolean;
   function Equal
            (  A, B : Pair;
               Eps  : Number'Base := 0.0
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
   procedure Find
             (  Var   : Variable;
                Span  : Interval;
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
   procedure Get
             (  Var   : Variable;
                Value : Number'Base;
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
             (  Var   : Variable;
                Index : Positive;
                Value : out Number;
                Left  : out Confidence;
                Min   : out Confidence;
                Max   : out Confidence;
                Right : out Confidence
             );
--
-- Get_Points_Number -- Get number of points of the membership function
--
--    Var - The variable
--
-- Returns :
--
--    The number of distinct points of the membership function
--
   function Get_Points_Number (Var : Variable) return Natural;
--
-- Get_Span -- Between the first and the last points
--
--    Var - The variable
--
-- This function returns the interval of domain values between the value
-- of  the  first  membership  function  point and the value of the last
-- point. It is equivalent to:
--
--    (Get_Value (Var, 1), Get_Value (Var, Get_Points_Number (Var)))
--
-- Returns :
--
--    The interval
--
-- Exceptions :
--
--    Constraint_Error - Empty variable
--
   function Get_Span (Var : Variable) return Interval;
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
   function Get_Value (Var : Variable; Index : Positive)
      return Number;
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
--
   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
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
--
   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
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
--
   procedure Insert_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Level : Confidence;
                Purge : Boolean := True
             );
--
-- Is_Empty -- Check if a variable is an empty set
--
--    Var - The variable
--
-- Returns :
--
--    True if the membership function is everywhere False
--
   function Is_Empty (Var : Variable) return Boolean;
--
-- Is_In -- Membership / subset test
--
--    A - Number, interval, fuzzy number, a lingustic variable
--    B - Number, interval, fuzzy number, a lingustic variable
--
-- At least one argument should be a Variable.
--
-- Returns :
--
--    (P(B|A), N(B|A))
--
   function Is_In (A, B : Variable) return Fuzzy_Boolean;
   function Is_In (A : Number; B : Variable) return Fuzzy_Boolean;
   function Is_In (A : Interval; B : Variable    ) return Fuzzy_Boolean;
   function Is_In (A : Fuzzy_Float; B : Variable) return Fuzzy_Boolean;
   function Is_In (A : Variable; B : Number) return Fuzzy_Boolean;
   function Is_In (A : Variable; B : Interval    ) return Fuzzy_Boolean;
   function Is_In (A : Variable; B : Fuzzy_Float) return Fuzzy_Boolean;
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
   function Is_Interval (Var : Variable)  return Boolean;
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
   function Is_Singleton (Var : Variable) return Boolean;
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
--
   procedure Move_Point
             (  Var   : in out Variable;
                Index : Positive;
                Value : Number;
                Purge : Boolean := True
             );
--
-- Necessity -- Conditional necessity
--
--    A - The first argument
--    B - The second argument (condition)
--
-- With  one  argument the condition is assumed to be the domain set. At
-- least one argument should be a Variable.
--
-- Returns :
--
--    N(A|B)
--
   function Necessity (A : Variable) return Confidence;
   function Necessity (A, B : Variable) return Confidence;
   function Necessity (A : Number; B : Variable)
      return Confidence;
   function Necessity (A : Interval; B : Variable) return Confidence;
   function Necessity (A : Fuzzy_Float; B : Variable)
      return Confidence;
   function Necessity (A : Variable; B : Number)
      return Confidence;
   function Necessity (A : Variable; B : Interval) return Confidence;
   function Necessity (A : Variable; B : Fuzzy_Float)
      return Confidence;
--
-- Possibility -- Conditional possibility
--
--    A - The first argument
--    B - The second argument (condition)
--
-- With  one  argument the condition is assumed to be the domain set. At
-- least one argument should be a Variable.
--
-- Returns :
--
--    P(A|B)
--
   function Possibility (A : Variable) return Confidence;
   function Possibility (A, B : Variable) return Confidence;
   function Possibility (A : Number; B : Variable)
      return Confidence;
   function Possibility (A : Interval; B : Variable) return Confidence;
   function Possibility (A : Fuzzy_Float; B : Variable)
      return Confidence;
   function Possibility (A : Variable; B : Number)
      return Confidence;
   function Possibility (A : Variable; B : Interval) return Confidence;
   function Possibility (A : Variable; B : Fuzzy_Float)
      return Confidence;
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
             (  Var       : in out Variable;
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
             (  Var   : in out Variable;
                Index : Positive;
                Purge : Boolean := True
             );
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
-- Right.  When Purge is true the point and its two immediate neighbours
-- are checked for being redundant and eventually can be removed.
--
-- Exceptions :
--
--    Constraint_Error - Wrong Index
--
   procedure Set_Point
             (  Var   : in out Variable;
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
             (  Var   : in out Variable;
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
--    Value - The value (abscissa)
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
             (  Var   : in out Variable;
                Index : Positive;
                Level : Confidence;
                Purge : Boolean := True
             );
--
-- To_Interval -- Conversion
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
   function To_Interval (Value : Variable) return Interval;
--
-- To_Number -- Conversion
--
--    Value - A variable to convert
--
-- Returns :
--
--    The corresponding number
--
-- Exceptions :
--
--    Constraint_Error - Value is not a singleton (number)
--
   function To_Number (Value : Variable) return Number;
--
-- To_Variable -- Conversion
--
--    Value - A number, interval or a fuzzy number
--
-- Returns :
--
--    The corresponding variable
--
   function To_Variable (Value : Number) return Variable;
   function To_Variable (Value : Interval    ) return Variable;
   function To_Variable (Value : Fuzzy_Float) return Variable;
--
-- Trim -- The membership function of a variable
--
--    Var - The variable
--
-- The  procedure  removes end points of the membership function if they
-- don't influence its values anywhere.
--
   procedure Trim (Var : in out Variable);
--
-- +, -, *, / -- By a number
--
--    Left  - Variable
--    Right - Number
--
-- Returns :
--
--    The result of the operation
--
   function "+" (Left : Variable; Right : Number) return Variable;
   function "-" (Left : Variable; Right : Number) return Variable;
   function "*" (Left : Variable; Right : Number) return Variable;
   function "/" (Left : Variable; Right : Number) return Variable;
--
-- = -- Equality
--
--    A - Number, Interval, Fuzzy number, Variable
--    B - Number, Interval, Fuzzy number, Variable
--
-- At least one argument should be a Variable.
--
-- Returns :
--
--    (P(A|B), N(A|B) and N(B|A))
--
   function "=" (A, B : Variable) return Fuzzy_Boolean;
   function "=" (A : Number; B : Variable) return Fuzzy_Boolean;
   function "=" (A : Interval;     B : Variable) return Fuzzy_Boolean;
   function "=" (A : Fuzzy_Float; B : Variable) return Fuzzy_Boolean;
   function "=" (A : Variable; B : Number) return Fuzzy_Boolean;
   function "=" (A : Variable; B : Interval    ) return Fuzzy_Boolean;
   function "=" (A : Variable; B : Fuzzy_Float) return Fuzzy_Boolean;
--
-- /= -- Inequality
--
--    A - Number, Interval, Fuzzy number, Variable
--    B - Number, Interval, Fuzzy number, Variable
--
-- At least one argument should be a Variable.
--
-- Returns :
--
--    not (A = B)
--
   function "/=" (A, B : Variable) return Fuzzy_Boolean;
   function "/=" (A : Number; B : Variable) return Fuzzy_Boolean;
   function "/=" (A : Interval; B : Variable    ) return Fuzzy_Boolean;
   function "/=" (A : Fuzzy_Float; B : Variable) return Fuzzy_Boolean;
   function "/=" (A : Variable; B : Number) return Fuzzy_Boolean;
   function "/=" (A : Variable; B : Interval    ) return Fuzzy_Boolean;
   function "/=" (A : Variable; B : Fuzzy_Float) return Fuzzy_Boolean;
--
-- & -- Appending a point to a lingquistic variable
--
--    Left  - The first argument (either a variable or a pair)
--    Right - The second argument (a pair)
--
-- These functions are equivalent to Append.
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Data_Error - The point provided by Right is out of order
--
   function "&" (Left : Pair;     Right : Pair) return Variable;
   function "&" (Left : Variable; Right : Pair) return Variable;
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
   function "and" (A, B : Variable) return Variable;
   function "and" (A : Variable;   B : Confidence) return Variable;
   function "and" (A : Confidence; B : Variable)   return Variable;
--
-- not -- Complement
--
--    A : The argument
--
-- Returns :
--
--    not A
--
   function "not" (A : Variable) return Variable;
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
   function "or" (A, B : Variable) return Variable;
   function "or" (A : Variable;   B : Confidence) return Variable;
   function "or" (A : Confidence; B : Variable)   return Variable;
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
   function "xor" (A, B : Variable) return Variable;
   function "xor" (A : Variable;   B : Confidence) return Variable;
   function "xor" (A : Confidence; B : Variable)   return Variable;

private
--
-- Purge -- The membership function of a variable
--
--    Var       - The variable
--    Last      - The last point to check
--    Count     - Number of points to check
--    Keep_Ends - Prevent the end points from purging
--
-- The procedure removes redundant points of the membership function for
-- being redundant. The parameter Last is the last point to check. Count
-- is  the  number  of  points  to  check. When Last is greater than the
-- number of points, it is assumed to be equal to.  Keep_Ends determines
-- dealing with the first and the last points of the function.
--
   procedure Purge
             (  Var       : in out Variable;
                Index     : Positive;
                Count     : Natural;
                Keep_Ends : Boolean
             );
--
-- Point -- One interpolation point of a membership function
--
--    Value - The argument of the function
--    Left  - The left limit of the function
--    Right - The right limit of the function
--    Max   - The upper value of the function in the argument
--    Min   - The lower value of the function in the argument
--
   type Point is record
      Value : Number;
      Left  : Confidence := Confidence'First;
      Right : Confidence := Confidence'First;
      Min   : Confidence := Confidence'First;
      Max   : Confidence := Confidence'First;
   end record;
--
-- Is_Singular -- Check if a point is a singular value
--
--    Dot : To be tested
--
-- Returns :
--
--    True if all levels are same
--
   function Is_Singular (Dot : Point) return Boolean;
--
-- Equal -- Compare to points
--
--    A   - The first argument
--    B   - The second argument
--    Eps - Tolerance
--
-- Returns :
--
--    True if Left is equal to Right
--
   function Equal
            (  A, B : Point;
               Eps  : Number'Base := 0.0
            )  return Boolean;
   function "=" (Left, Right : Point) return Boolean;
   function "<" (Left, Right : Point) return Boolean;

   type Point_Array is array (Positive range <>) of Point;
--
-- Points -- Instantiation of generic set
--
-- The package  provides  sets  of  interpolation  points.  Such  a  set
-- represents a membership function.
--
   package Points is
      new Generic_Set
          (  Object_Type  => Point,
             Null_Element =>
               (  Number'First,
                  Confidence'First,
                  Confidence'First,
                  Confidence'Last,
                  Confidence'First
               ),
             Minimal_Size => 8
          );
   use Points;
   type Variable is record
      Data : Points.Set;
   end record;
--
-- Binary_Operation -- Operations on two variables
--
--    User_Data   - The user-defined data type (operation context)
--    Do_Empty    - Action when one of the arguments is an empty set
--    Do_Point    - Action for a singular point
--    Do_Interval - Action on an interval
--
-- An instance is a procedure with the following parameters:
--
--    Data - The user data
--    A    - The first parameter (variable)
--    B    - The first parameter (variable)
--
-- If  either  A  or  B  is  empty, then Do_Empty is called with another
-- parameter. Otherwise, the procedure enumerates the joining points  of
-- A and B. The points are enumerated in ascending order. For each  such
-- point Do_Point is called at least once. The  parameters  of  Do_Point
-- are the values of the membership functions in the point.  The  values
-- are  interpolated  if  necessary. Do_Point is called several times if
-- Left,  Min,  Max,  Right  differs  there. It is warrantied that it is
-- called for Min-Min and Max-Max combinations if they differ. For  each
-- pair of points Do_Interval is called.
--
   generic
      type User_Data is limited private;
      with procedure Do_Empty (Data : in out User_Data; A : Variable);
      with procedure Do_Point
                     (  Data : in out User_Data;
                        X    : Number'Base;
                        A, B : Confidence
                     );
      with procedure Do_Interval
                     (  Data   : in out User_Data;
                        X1     : Number'Base;
                        A1, B1 : Confidence;
                        X2     : Number'Base;
                        A2, B2 : Confidence
                     );
   procedure Binary_Operation (Data : in out User_Data; A, B : Variable);
--
-- Unary_Operation -- Operations on a variable
--
--    User_Data   - The user-defined data type (operation context)
--    Do_Empty    - Action when one of the arguments is an empty set
--    Do_Point    - Action for a singular point
--    Do_Interval - Action on an interval
--
-- An instance is a procedure with the following parameters:
--
--    Data - The user data
--    A    - The parameter (variable)
--
-- If A is empty, then Do_Empty  is  called.  Otherwise,  the  procedure
-- enumerates the joining points of A.  The  points  are  enumerated  in
-- ascending order. For each such point  Do_Point  is  called  at  least
-- once.  Do_Point  is  called  several  times  if Left, Min, Max, Right
-- differs there. For each pair of points Do_Interval is called.
--
   generic
      type User_Data is limited private;
      with procedure Do_Empty (Data : in out User_Data);
      with procedure Do_Point
                     (  Data : in out User_Data;
                        X    : Number'Base;
                        Y    : Confidence
                     );
      with procedure Do_Interval
                     (  Data : in out User_Data;
                        X1   : Number'Base;
                        Y1   : Confidence;
                        X2   : Number'Base;
                        Y2   : Confidence
                     );
   procedure Unary_Operation (Data : in out User_Data; A : Variable);
--
-- Interpolate -- Confidence factors (linear)
--
--    X  - The argument
--    X1 - The first interpolation point
--    Y1 - The value at this point
--    X2 - The second interpolation point
--    Y2 - The value there
--
-- Returns :
--
--    An interpolated value in X
--
   function Interpolate
            (  X  : Number;
               X1 : Number;
               Y1 : Confidence;
               X2 : Number;
               Y2 : Confidence
            )  return Confidence;
   pragma Inline (Interpolate);
--
-- Root -- Intersection of two linear functions on an interval
--
--    X1     - The left bound of the interval
--    A1, B1 - The function values there
--    X2     - The right bound
--    A2, B2 - The function values there
--
-- The  result  is  valid if in ]X1,X2[. Otherwise, the functions do not
-- intersect on ]X1, X2[ and the result has to be ignored.
--
-- Returns :
--
--    The result
--
   function Root
            (  X1     : Number;
               A1, B1 : Confidence;
               X2     : Number;
               A2, B2 : Confidence
            )  return Number'Base;
--
-- Trim -- The membership function of a variable
--
--    Var   - The variable
--    First - The index of the first point
--    Last  - The index of the last point
--
-- The procedure removes the end points of the  membership  function  if
-- they don't influence its values anywhere.  When  First..Last  is  the
-- range of the remaining points.
--
   procedure Trim
             (  Var   : Variable;
                First : out Positive;
                Last  : out Natural
             );

   pragma Inline (Empty);
   pragma Inline (Equal);
   pragma Inline (Erase);
   pragma Inline (Get_Points_Number);
   pragma Inline (Get_Span);
   pragma Inline (Is_Singular);
   pragma Inline ("=", "/=", "<", "=");

end Fuzzy.Linguistics;
