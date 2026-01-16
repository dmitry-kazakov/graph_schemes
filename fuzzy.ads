--                                                                    --
--  package Fuzzy                   Copyright (c)  Dmitry A. Kazakov  --
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
--  This  package provides the type Set representing a fuzzy set over an
--  interval of integer numbers. The following  operations  are  defined
--  for Set:
--
--  Tests
--
--     Is_In    Gives the confidence factor for a domain point
--     ()       Same
--
--  Per point operations. The result is a fuzzy set:
--
--     *        Multiplication {Ai*Bi}
--     +        Addition {Ai+Bi-Ai*Bi}
--     -        Distance {abs(Ai-Bi)}
--     and      Intersection {min(Ai,Bi)}
--     mod      Truncation
--     not      Complement {1-Ai}
--     or       Union {max(Ai,Bi)}
--     rem      Rounding
--     xor      Distance (A and not B) or (not A and B)
--
--  Variants with an accumulator
--
--     Add_At   Addition
--     And_At   Intersection
--     Mod_At   Truncation
--     Mul_At   Multiplication
--     Not_At   Complement
--     Or_At    Union
--     Rem_At   Rounding
--     Xor_At   Distance
--
--  Integral operations. The result is a scalar
--
--     Possibility      Conditional possibility max min(Ai,Bi)
--     Necessity        Conditional necessity min max(Ai,1-Bi)
--     Distance         Point to set distance
--
with Confidence_Factors; use Confidence_Factors;

package Fuzzy is
   pragma Pure (Fuzzy);
   type Set is array (Integer range <>) of Confidence;
--
-- Add_At -- Fuzzy set probabilistic addition with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result  is defined as {Ai+Bi-Ai*Bi}, which is an
-- equivalent to not (not A * not B). When one  of  the  argument  is  a
-- confidence then the result is computed as if the argument were a  set
-- with  all elements belonging to it with the confidence level equal to
-- the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Add_At (A : in out Set; B : Set);
   procedure Add_At (A : in out Set; B : Confidence);
--
-- And_At -- Fuzzy set intersection with an accumulator
--
--    A - The first argument
--    B - The second argument
--  [ C - The third argument ]
--
-- For  two  sets the result is defined as {min(Ai,Bi)}. When one of the
-- arguments is a confidence then the  result  is  computed  as  if  the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence  level  equal  to  the  argument.  The  variant with three
-- parameters evaluates A := A and (B or C).
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure And_At (A : in out Set; B : Set);
   procedure And_At (A : in out Set; B : Confidence);
   procedure And_At (A : in out Set; B : Set; C : Confidence);
--
-- Diff_At -- Fuzzy set distance with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets the result is defined as {abs(Ai-Bi)}. When one of the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Diff_At (A : in out Set; B : Set);
   procedure Diff_At (A : in out Set; B : Confidence);
--
-- Is_In -- Test if a point belongs to the set
--
--    Point - To be tested
--    A     - The argument
--
-- The  result  is the confidence factor of Point is in A. This function
-- is an equivalent to A (Point), however it never raises an exception.
--
-- Returns :
--
--    Confidence factor
--
   function Is_In (Point : Integer; A : Set) return Confidence;
--
-- Mod_At -- Fuzzy set truncation with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result is defined as Ai, if Ai>Bi; 0, otherwise.
-- When one of the argument is a confidence then the result is  computed
-- as  if the argument were a set with all elements belonging to it with
-- the confidence level equal to the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Mod_At (A : in out Set; B : Set);
   procedure Mod_At (A : in out Set; B : Confidence);
--
-- Mul_At -- Fuzzy set multiplication with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For two sets the result is  defined  as  {Ai*Bi}.  When  one  of  the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Mul_At (A : in out Set; B : Set);
   procedure Mul_At (A : in out Set; B : Confidence);
--
-- Not_At -- Fuzzy set complement with an accumulator
--
--    A - The argument
--
-- The result set is defined as {1-Ai}
--
   procedure Not_At (A : in out Set);
--
-- Or_At -- Fuzzy set union with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets the result is defined as {max(Ai,Bi)}. When one of the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence  level  equal  to  the  argument.  The  variant with three
-- parameters evaluates A := A or (B and C).
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Or_At (A : in out Set; B : Set);
   procedure Or_At (A : in out Set; B : Confidence);
   procedure Or_At (A : in out Set; B : Set; C : Confidence);
--
-- Rem_At -- Fuzzy set rounding with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result is defined as Ai, if Ai<Bi; 1, otherwise.
-- When one of the argument is a confidence then the result is  computed
-- as  if the argument were a set with all elements belonging to it with
-- the confidence level equal to the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Rem_At (A : in out Set; B : Set);
   procedure Rem_At (A : in out Set; B : Confidence);
--
-- Xor_At -- Fuzzy set distance with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- For two sets the result is defined as {(Ai and not Bi) or (not Ai and
-- Bi)}.  When  one  of  the argument is a confidence then the result is
-- computed as if the argument were a set with all elements belonging to
-- it with the confidence level equal to the argument.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure xor_At (A : in out Set; B : Set);
   procedure Xor_At (A : in out Set; B : Confidence);
--
-- not -- Fuzzy set complement
--
--    A  - The argument
--
-- The result set is defined as {1-Ai}
--
-- Returns :
--
--    The complement of the argument
--
   function "not" (A : Set) return Set;
--
-- and -- Fuzzy set intersection
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets the result is defined as {min(Ai,Bi)}. When one of the
-- arguments is a confidence then the  result  is  computed  as  if  the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Returns :
--
--    Intersection of arguments
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "and" (A, B : Set) return Set;
   function "and" (A : Set; B : Confidence) return Set;
   function "and" (A : Confidence; B : Set) return Set;
--
-- or -- Fuzzy set union
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets the result is defined as {max(Ai,Bi)}. When one of the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Returns :
--
--    Union of arguments
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "or" (A, B : Set) return Set;
   function "or" (A : Set; B : Confidence) return Set;
   function "or" (A : Confidence; B : Set) return Set;
--
-- * -- Fuzzy set multiplication
--
--    A - The first argument
--    B - The second argument
--
-- For two sets the result is  defined  as  {Ai*Bi}.  When  one  of  the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Returns :
--
--    Multiplication of arguments
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "*" (A, B : Set) return Set;
   function "*" (A : Set; B : Confidence) return Set;
   function "*" (A : Confidence; B : Set) return Set;
--
-- + -- Fuzzy set probabilistic addition
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result  is defined as {Ai+Bi-Ai*Bi}, which is an
-- equivalent to not (not A * not B). When one  of  the  argument  is  a
-- confidence then the result is computed as if the argument were a  set
-- with  all elements belonging to it with the confidence level equal to
-- the argument.
--
-- Returns :
--
--    Sum of arguments
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "+" (A, B : Set) return Set;
   function "+" (A : Set; B : Confidence) return Set;
   function "+" (A : Confidence; B : Set) return Set;
--
-- - -- Fuzzy set distance
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets the result is defined as {abs(Ai-Bi)}. When one of the
-- argument  is  a  confidence  then  the  result  is computed as if the
-- argument  were  a  set  with  all  elements  belonging to it with the
-- confidence level equal to the argument.
--
-- Returns :
--
--    Set of distances
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "-" (A, B : Set) return Set;
   function "-" (A : Set; B : Confidence) return Set;
   function "-" (A : Confidence; B : Set) return Set;
--
-- xor -- Fuzzy set distance
--
--    A - The first argument
--    B - The second argument
--
-- For two sets the result is defined as {(Ai and not Bi) or (not Ai and
-- Bi)}.  When  one  of  the argument is a confidence then the result is
-- computed as if the argument were a set with all elements belonging to
-- it with the confidence level equal to the argument.
--
-- Returns :
--
--    Set of distances
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "xor" (A, B : Set) return Set;
   function "xor" (A : Set; B : Confidence) return Set;
   function "xor" (A : Confidence; B : Set) return Set;
--
-- mod -- Fuzzy set truncation
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result is defined as Ai, if Ai>Bi; 0, otherwise.
-- When one of the argument is a confidence then the result is  computed
-- as  if the argument were a set with all elements belonging to it with
-- the confidence level equal to the argument.
--
-- Returns :
--
--    Truncated set
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "mod" (A, B : Set) return Set;
   function "mod" (A : Set; B : Confidence) return Set;
   function "mod" (A : Confidence; B : Set) return Set;
--
-- rem -- Fuzzy set rounding
--
--    A - The first argument
--    B - The second argument
--
-- For  two  sets  the  result is defined as Ai, if Ai<Bi; 1, otherwise.
-- When one of the argument is a confidence then the result is  computed
-- as  if the argument were a set with all elements belonging to it with
-- the confidence level equal to the argument.
--
-- Returns :
--
--    Rounded set
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "rem" (A, B : Set) return Set;
   function "rem" (A : Set; B : Confidence) return Set;
   function "rem" (A : Confidence; B : Set) return Set;
--
-- Possibility -- Conditional possibility (P(A|B))
--
--    A - The first argument
--    B - The second argument
--
-- The operation calculates the possibility of "A if B". The possibility
-- is calculated as max min{Ai,Bi}.
--
-- Returns :
--
--    P(A|B)
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function Possibility (A, B : Set) return Confidence;
--
-- Possibility -- Integral possibility (P(A|1))
--
--    A - The fuzzy event
--
-- The operation calculates the possibility of "A if true", i.e. max Ai.
-- It shall be true for a normal set.
--
-- Returns :
--
--    P(A|1)
--
   function Possibility (A : Set) return Confidence;
--
-- Necessity -- Conditional necessity (N(A|B))
--
--    A - The fuzzy event
--    B - The fuzzy condition
--
-- The  operation calculates the necessity of "A if B". The necessity is
-- calculated as min max(Ai,1-Bi).
--
-- Returns :
--
--    N(A|B)
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function Necessity (A, B : Set) return Confidence;
--
-- Necessity -- Integral necessity (N(A|1))
--
--    A - The event
--
-- The operation calculates the necessity of "A if true", i.e. min Ai of
-- confidence levels. It shall be false for a normal set.
--
-- Returns :
--
--    N(A|0)
--
   function Necessity (A : Set) return Confidence;
--
-- Distance -- Distance between point and set
--
--    Point - A point of the domain
--    A     - Fuzzy set
--
-- The  operation calculates the distance between the domain point Point
-- and the nearest point of the domain which confidence level in the set
-- Set  is  not  false.  If  the  set  is  empty  the  function  returns
-- Natural'Last.
--
-- Returns :
--
--    Distance
--
   function Distance (point : Integer; A : Set) return Natural;

private
   pragma Inline (Add_At);
   pragma Inline (Diff_At);
   pragma Inline (Distance);
   pragma Inline (Is_In);
   pragma Inline (Mod_At);
   pragma Inline (Mul_At);
   pragma Inline (Necessity);
   pragma Inline (Not_At);
   pragma Inline (Or_At);
   pragma Inline (Possibility);
   pragma Inline (Rem_At);
   pragma Inline (Xor_At);
   pragma Inline ("not", "and", "mod", "or", "rem", "xor");
   pragma Inline ("*", "+", "-");

end Fuzzy;
