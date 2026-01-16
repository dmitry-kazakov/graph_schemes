--                                                                    --
--  package Fuzzy.Numbers           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
--  This generic package provides  fuzzy  numbers.  A  fuzzy  number  is
--  modelled by a set of nested intervals:
--
--     Fuzzy_Number = { [Li, Ri] | i>j => Li >= Lj & Ri <= Rj }
--
--  For each i there is a confidence level associated with  it.  Because
--  operations on intervals are inclusion-monotonic, operations on fuzzy
--  numbers  defined  as  above  are  O(N),  where  N  is  the number of
--  intervals  used  to model them. The package has the following formal
--  parameters:
--
--     Interval_Index - The index type used to count intervals
--     Interval_Map   - The array type mapping index to confidence
--     Number         - A numeric type
--     Interval       - An interval type built upon Number
--     To_Confidence  - Interval_Index->Confidence. It shall be an array
--                      of ascending values. The last value  has  to  be
--                      Confidence'Last.
--
--  Other  parameters are the operations defined on Number and Interval.
--  Usually  they  should be visible at instantiation point, so that the
--  defaults would work.
--
with Fuzzy.Logic; use Fuzzy.Logic;
with Intervals;   use Intervals;

generic
   type Interval_Index is (<>);
   type Interval_Map is array (Interval_Index) of Confidence;
   type Number is private;
   type Interval is private;
   To_Confidence : Interval_Map;
   with function From (Left : Interval) return Number is <>;
   with function Is_In (Left, Right : Interval) return Boolean is <>;
   with function Is_In (Left : Number; Right : Interval)
      return Boolean is <>;
   with function To (Left : Interval) return Number is <>;
   with function To_Interval (Left : Number) return Interval is <>;
   with function To_Interval (Left, Right : Number)
      return Interval is <>;
   with function "abs" (Left : Interval) return Interval is <>;
   with function "+" (Left, Right : Interval) return Interval is <>;
   with function "+" (Left : Interval; Right : Number)
      return Interval is <>;
   with function "+" (Left : Number; Right : Interval)
      return Interval is <>;
   with function "-" (Left : Interval) return Interval is <>;
   with function "-" (Left, Right : Interval) return Interval is <>;
   with function "-" (Left : Interval; Right : Number)
      return Interval is <>;
   with function "-" (Left : Number; Right : Interval)
      return Interval is <>;
   with function "*" (Left : Interval; Right : Number)
      return Interval is <>;
   with function "*" (Left : Number; Right : Interval)
      return Interval is <>;
   with function "*" (Left, Right : Interval) return Interval is <>;
   with function "/" (Left : Interval; Right : Number)
      return Interval is <>;
   with function "/" (Left : Number; Right : Interval)
      return Interval is <>;
   with function "/" (Left, Right : Interval) return Interval is <>;
   with function ">" (Left : Interval; Right : Number)
      return Logical is <>;
   with function "**" (Left : Interval; Right : Natural)
      return Interval is <>;
   with function ">" (Left : Number; Right : Interval)
      return Logical is <>;
   with function ">" (Left, Right : Interval) return Logical is <>;
   with function ">=" (Left : Number; Right : Number)
      return Boolean is <>;
   with function ">=" (Left : Interval; Right : Number)
      return Logical is <>;
   with function ">=" (Left : Number; Right : Interval)
      return Logical is <>;
   with function ">=" (Left, Right : Interval) return Logical is <>;
   with function "&" (Left, Right : Interval) return Boolean is <>;

package Fuzzy.Numbers is
   type Fuzzy_Number is private;
--
-- Equal -- Equivalence as sets
--
--    Left  - The first argument
--    Right - The first argument
--
-- Returns :
--
--    True if Left is identic to Right
--
   function Equal (Left, Right : Fuzzy_Number) return Boolean
      renames "=";
--
-- Is_In -- Membership and inclusion
--
--    Left  - The first argument
--    Right - The second argument
--
-- One  of  arguments  shall  be  a fuzzy number. Other can be interval,
-- number or fuzzy number.
--
-- Returns :
--
--    ( Possibility (Right | Left), Necessity (Right | Left) )
--
   function Is_In (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function Is_In (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function Is_In (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function Is_In (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function Is_In (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
--
-- Necessity -- Get necessity
--
--    Left  - A number / interval / fuzzy number
--    Right - A fuzzy number
--
-- The result tells how  necessary  is  Left  given  Right.  It  is  the
-- inversed confidence level of the most nested interval  of  Right  not
-- intersecting Left.
--
-- Returns :
--
--    Necessity (Left | Right}
--
   function Necessity (Left, Right : Fuzzy_Number)
      return Confidence;
   function Necessity (Left : Number; Right : Fuzzy_Number)
      return Confidence;
   function Necessity (Left : Interval; Right : Fuzzy_Number)
      return Confidence;
   function Necessity (Left : Fuzzy_Number; Right : Number)
      return Confidence;
   function Necessity (Left : Fuzzy_Number; Right : Interval)
      return Confidence;
--
-- Possibility -- Get possibility
--
--    Left  - A number / interval / fuzzy number
--    Right - A fuzzy number
--
-- The  result  tells  how  possible  is  Left  given  Right.  It is the
-- confidence  level  of  the most nested interval of Right intersecting
-- Left.
--
-- Returns :
--
--    Possibility (Left | Right}
--
   function Possibility (Left, Right : Fuzzy_Number)
      return Confidence;
   function Possibility (Left : Number; Right : Fuzzy_Number)
      return Confidence;
   function Possibility (Left : Interval; Right : Fuzzy_Number)
      return Confidence;
   function Possibility (Left : Fuzzy_Number; Right : Number)
      return Confidence;
   function Possibility (Left : Fuzzy_Number; Right : Interval)
      return Confidence;
--
-- To_Fuzzy -- A fuzzy number from a number or interval
--
--    Left - The argument (number / interval)
--
-- Returns :
--
--    The fuzzy number corresponding Left
--
   function To_Fuzzy (Left : Number) return Fuzzy_Number;
   function To_Fuzzy (Left : Interval) return Fuzzy_Number;
--
-- abs -- Absolute value
--
--    Left - The argument
--
-- Returns :
--
--    The absolute value of the argument
--
   function "abs" (Left : Fuzzy_Number) return Fuzzy_Number;
--
-- + -- Unary plus
--
--    Left - The argument
--
-- Returns :
--
--    The argument
--
   function "+" (Left : Fuzzy_Number) return Fuzzy_Number;
--
-- - -- Unary minus
--
--    Left - The argument
--
-- Returns :
--
--    Negated argument
--
   function "-" (Left : Fuzzy_Number) return Fuzzy_Number;
--
-- + -- Addition
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both operands are fuzzy numbers or one of them is a number or
-- an interval.
--
-- Returns :
--
--    Left + Right
--
-- Exceptions :
--
--    Constraint_Error
--
   function "+" (Left, Right : Fuzzy_Number) return Fuzzy_Number;
   function "+" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number;
   function "+" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number;
   function "+" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number;
   function "+" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number;
--
-- - -- Subtraction
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both operands are fuzzy numbers or one of them is a number or
-- an interval.
--
-- Returns :
--
--    Left - Right
--
-- Exceptions :
--
--    Constraint_Error
--
   function "-" (Left, Right : Fuzzy_Number) return Fuzzy_Number;
   function "-" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number;
   function "-" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number;
   function "-" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number;
   function "-" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number;
--
-- * -- Multiplication
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both operands are fuzzy numbers or one of them is a number or
-- an interval.
--
-- Returns :
--
--    Left * Right
--
-- Exceptions :
--
--    Constraint_Error
--
   function "*" (Left, Right : Fuzzy_Number) return Fuzzy_Number;
   function "*" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number;
   function "*" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number;
   function "*" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number;
   function "*" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number;
--
-- / -- Division
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both operands are fuzzy numbers or one of them is a number or
-- an interval.
--
-- Returns :
--
--    Left / Right
--
-- Exceptions :
--
--    Constraint_Error
--
   function "/" (Left, Right : Fuzzy_Number) return Fuzzy_Number;
   function "/" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Number;
   function "/" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Number;
   function "/" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Number;
   function "/" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Number;
--
-- ** -- Exponentiation
--
--    Left  - The first operand
--    Right - The second operand (natural power)
--
-- Returns :
--
--    Left * Right
--
-- Exceptions :
--
--    Constraint_Error
--
   function "**" (Left : Fuzzy_Number; Right : Natural)
      return Fuzzy_Number;
--
-- >, >=, =, /=, <, <= -- Relations
--
--    Left  - The first operand
--    Right - The second operand
--
-- Either  both operands are fuzzy numbers or one of them is a number or
-- an interval.
--
-- Returns :
--
--    The result of the comparison
--
   function ">" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function ">" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function ">" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function ">" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function ">" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;

   function ">=" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function ">=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function ">=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function ">=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function ">=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;

   function "=" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function "=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function "=" ( Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function "=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function "=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;

   function "/=" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function "/=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function "/=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function "/=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function "/=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;

   function "<" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function "<" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function "<" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function "<" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function "<" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;

   function "<=" (Left, Right : Fuzzy_Number) return Fuzzy_Boolean;
   function "<=" (Left : Fuzzy_Number; Right : Number)
      return Fuzzy_Boolean;
   function "<=" (Left : Number; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
   function "<=" (Left : Fuzzy_Number; Right : Interval)
      return Fuzzy_Boolean;
   function "<=" (Left : Interval; Right : Fuzzy_Number)
      return Fuzzy_Boolean;
--
-- Get_Interval -- Get an interval of a number
--
--    Value - The number
--    Index - Of the interval
--
-- The result is [Li, Ui], where i is the interval index.
--
-- Returns :
--
--    The corresponding interval
--
   function Get_Interval
            (  Value : Fuzzy_Number;
               Index : Interval_Index
            )  return Interval;

private
   type Interval_Array is array (Interval_Index) of Interval;
   type Fuzzy_Number is record
      Set : Interval_Array;
   end record;

   pragma Inline (Get_Interval);

end Fuzzy.Numbers;
