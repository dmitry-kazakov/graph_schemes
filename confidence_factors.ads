--                                                                    --
--  package Confidence_Factors      Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2000       --
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
--  This  package  provides  the  type  Confidence used as the belonging
--  level of a fuzzy set element. Instances of  Confidence  are  ordered
--  numbers in the range 0..1. The following operations are defined  for
--  Confidence:
--
--     *        Multiplication (probabilistic)
--     +        Addition (probabilistic)
--     -        Distance (euclidean)
--     and      Intersection (min)
--     mod      Truncation
--     not      Complement (1-value)
--     or       Union (max)
--     rem      Rounding
--     xor      Distance (exclusive or)
--
package Confidence_Factors is
   pragma Pure (Confidence_Factors);
--
-- The constant Confidence_Size defines how fine  grained  is  the  type
-- Confidence. It is the minmal number of bits used to keep a  value  of
-- Confidence.
--
   Confidence_Size : constant := 8; -- One byte per value
--
-- The type Confidence is a fixed point number.
--
   type Confidence is delta 2.0 ** (-Confidence_Size) range 0.0..1.0;
   for Confidence'Size use Confidence_Size;
--
-- not -- Fuzzy complement
--
--    Left - The argument
--
-- The operation is implemented as 1 - Left.
--
-- Returns :
--
--    not Left
--
   function "not" (Left : Confidence) return Confidence;
--
-- and -- Fuzzy intersection
--
--    Left  - The left argument
--    Right - The right argument
--
-- The operation is commutative. It is implemented as max (Left, Right).
--
-- Returns :
--
--    Left and Right
--
   function "and" (Left, Right : Confidence) return Confidence;
--
-- or -- Fuzzy union
--
--    Left  - The left argument
--    Right - The right argument
--
-- The operation is commutative. It is implemented as min (Left, Right).
--
-- Returns :
--
--    Left and Right
--
   function "or"  (Left, Right : Confidence) return Confidence;
--
-- xor -- Fuzzy distance
--
--    Left  - The left argument
--    Right - The right argument
--
-- The  distance  is  commutative and defined as (Left and not Right) or
-- (not Left and Right).
--
-- Returns :
--
--    Distance between Left and Right
--
   function "xor" (Left, Right : Confidence) return Confidence;
--
-- + -- Fuzzy addition
--
--    Left  - The left argument
--    Right - The right argument
--
-- The  operation  is  commutative  and defined as Left + Right - Left *
-- Right, i.e. A + B = not (not A * not B).
--
-- Returns :
--
--    Left + Right
--
   function Add (Left, Right : Confidence)
      return Confidence renames "+";
   function "+" (Left, Right : Confidence) return Confidence;
--
-- - -- Fuzzy euclidean distance
--
--    Left  - The left argument
--    Right - The right argument
--
-- The distance is commutative and defined as abs (Left - Right).
--
-- Returns :
--
--    Euclidean distance between Left and Right
--
   function Sub (Left, Right : Confidence)
      return Confidence renames "-";
   function "-" (Left, Right : Confidence) return Confidence;
--
-- mod -- Truncation
--
--    Left  - The argument
--    Right - The threshould
--
-- The  operation  gives  the  argument  if  it  is  greater  than   the
-- threshould. Otherwise the result is False.
--
-- Returns :
--
--    Left if Left > Right, otherwise False
--
   function "mod" (Left, Right : Confidence) return Confidence;
--
-- rem -- Rounding
--
--    Left  - The argument
--    Right - The threshould
--
-- The operation gives the argument if it is less than  the  threshould.
-- Otherwise the result is True.
--
-- Returns :
--
--    Left if Left < Right, otherwise True
--
   function "rem" (Left, Right : Confidence) return Confidence;
--
-- To_Confidence -- Convert a Boolean value
--
--    Value - To be converted
--
-- Returns :
--
--    The conversion result
--
   function To_Confidence (Value : Boolean) return Confidence;

private
   pragma Inline (To_Confidence);
   pragma Inline ("+", "-");
   pragma Inline ("and", "mod", "not", "or", "rem", "xor");

end Confidence_Factors;
