--                                                                    --
--  package Fuzzy.Logic             Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
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
--  This package provides the type Fuzzy_Boolean  representing  a  fuzzy
--  logical value.  A  fuzzy  logical  value  is  a  pair  (Possibility,
--  Necessity). The first component is the possibility that the value is
--  true. The second component is the necessary of that. The possibility
--  component  shall  be  always  greater than or equal to the necessity
--  component. A complement view of a fuzzy logical  value  is  the  set
--  view.  A  logical  value  can  be  thought  as  a  fuzzy  set   over
--  {true,false}. In this case the confidence  factor  of  true  is  the
--  possibility  that  the value is true. The confidence factor of false
--  is  the  possibility  that the value is false. Its complement is the
--  necessity that the value is true. Most important  algebraic  results
--  of the fuzzy logic:
--
--     1. Associativity:          (X and Y) and Z = X and (Y and Z)
--     2. Commutativity:          X and Y = Y and X
--     3. Distributivity:         (X and Y) or Z = (X or Z) and (Y or Z)
--     4. de Morgan's theorems:   not (X and Y) = (not X) or (not Y)
--
--  The results of conventional logic that do not hold:
--
--     5. X and (not X)  is  not  false.  However  its  possibility  and
--        necessity are less than or equal to ones both X and not X.
--     6. X or  (not  X)  is  not  true.  However  its  possibility  and
--        necessity are greater than or equal to ones both X and not X.
--
--  How  close  the  results  5.  and  6.  are  to  ones of conventional
--  determines  the  certainty  of  a  logical  value.  certainty  is  a
--  confidence  factor  in  [0..1].  0  corresponds  to  an   absolutely
--  uncertain logical value. 1 corresponds to either true  or  false  of
--  conventional logic.
--
package Fuzzy.Logic is
   pragma Preelaborate (Fuzzy.Logic);
   type Fuzzy_Boolean is record
      Possibility : Confidence;
      Necessity   : Confidence;
   end record;
--
-- Certain_True -- An equivalent to sharp true
--
   Certain_True : Fuzzy_Boolean :=
      (Confidence'Last, Confidence'Last);
--
-- Certain_False -- An equivalent to sharp false
--
   Certain_False : Fuzzy_Boolean :=
      (Confidence'First, Confidence'First);
--
-- Uncertain -- Absolute uncertainty = don't know
--
   Uncertain : Fuzzy_Boolean := (Confidence'Last, Confidence'First);
--
-- Contradictory -- Absolute inconsistency
--
   Contradictory : Fuzzy_Boolean :=
      (Confidence'First, Confidence'Last);
--
-- To_Fuzzy -- Convert a boolean to fuzzy boolean
--
--      Left - The argument
--
   function To_Fuzzy (Left : Boolean) return Fuzzy_Boolean;
--
-- not -- Fuzzy inversion/complement
--
--      Left - The argument
--
-- The  result is defined as ( not Necessity, not Possibility ). Observe
-- that as expected: not not A = A.
--
-- Returns :
--
--      The complement of the argument
--
   function "not" (Left : Fuzzy_Boolean) return Fuzzy_Boolean;
--
-- and -- Fuzzy and
--
--      Left  - The first argument
--      Right - The second argument
--
-- The  result is defined as per component minimum. One of arguments can
-- be of Boolean as well. Note that de Morgan theorems hold:
--
--      not (X and Y) = (not X) or (not Y)
--
-- Returns :
--
--      Logical and of arguments
--
   function "and" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "and" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "and" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- or -- Fuzzy or
--
--      Left  - The first argument
--      Right - The second argument
--
-- The  result is defined as per component maximum. One of arguments can
-- be of Boolean as well.
--
-- Returns :
--
--      Logical or of arguments
--
   function "or" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "or" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "or" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- xor -- Fuzzy xor
--
--      Left  - The left argument
--      Right - The right argument
--
-- The result is defined as (Left  and  not  Right)  or  (not  Left  and
-- Right). One of arguments can be of Boolean as well.
--
-- Returns :
--
--      Logical xor of arguments
--
   function "xor" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "xor" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "xor" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- * -- A pessimistic combination of fuzzy evidences
--
--      Left  - The left argument
--      Right - The right argument
--
-- The result is defined as the minimum of possibilities and the maximum
-- of necessities. One of arguments can be of Boolean as well.
--
-- Returns :
--
--      The result
--
   function "*" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "*" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "*" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- + -- An optimistic combination of fuzzy evidences
--
--      Left  - The left argument
--      Right - The right argument
--
-- The result is defined as the maximum of possibilities and the minimum
-- of necessities. One of arguments can be of Boolean as well.
--
-- Returns :
--
--      The result
--
   function "+" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "+" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "+" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- >= -- Implication of fuzzy events
--
--      Left  - The left argument
--      Right - The right argument
--
-- The result is defined as (not Left.Possibility or  Right.Possibility,
-- not Left.Necessity or Right.Necessity). Note that it differs from not
-- Left or Right.
--
-- Returns :
--
--      Left implies Right
--
   function ">=" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function ">=" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function ">=" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- <= -- Implication of fuzzy events
--
--      Left  - The left argument
--      Right - The right argument
--
-- The result is defined as Right >= Left. Note that Left <= Right is not
-- equal to not (Left => Right).
--
-- Returns :
--
--      Right implies Left
--
   function "<=" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean;
   function "<=" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean;
   function "<=" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean;
--
-- Certainty -- Certainty
--
--      Left - The argument
--
-- The operation calculates the certainty of  a  logical  value.  It  is
-- defined as: 1 - |Possibility - Necessity|.
--
-- Returns :
--
--      The certainty of the argument
--
   function Certainty (Left : Fuzzy_Boolean) return Confidence;
--
-- Is_In -- Fuzzy subset relation
--
--    A - The first fuzzy set
--    B - The second fuzzy set
--
-- This function returns a logical value charaterizing whether A is
-- a subset of B. It is defined as:
--
--    (  Possibility => Possibility (B, A),
--       Necessity   => Necessity   (B, A)
--    )
--
-- Returns :
--
--    A is in B
--
-- Exceptions :
--
--    Constraint_Error - A and B have different cardinality
--
   function Is_In (A, B : Set) return Fuzzy_Boolean;
   pragma Inline (Is_In);

private
   pragma Inline (To_Fuzzy);
   pragma Inline ("and", "not", "or", "xor", "*", "+", ">=", "<=");
   pragma Inline (Certainty);

end Fuzzy.Logic;
