--                                                                    --
--  package Fuzzy.Intuitionistic    Copyright (c)  Dmitry A. Kazakov  --
--  (Intuitionistic fuzzy sets)                    Luebeck            --
--  Interface                                      Winter, 2003       --
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
--  This  package  provides  types  and  operations  for  dealing   with
--  intuitionistic fuzzy set. Intuitionistic fuzzy sets  generalize  the
--  notion  of  fuzzy  set.  Depending  on  the meaning addressed to the
--  membership  function  of  a  set,  it  can be a fuzzy intuitionistic
--  classification or a fuzzy intuitionistic set.
--
--  The type Set has the following operations defined:
--
--  Tests
--
--     Is_In    Membership test and subset relation
--     <=, >=   Symmetric subset relation
--     =, /=    Symmetric equivalence relation
--
--  Per point operations. The result is a fuzzy set:
--
--     and      Intersection
--     not      Complement
--     or       Union
--     xor      Distance (A and not B) or (not A and B)
--
--  In-place variants of the operations above:
--
--     And_At   Intersection
--     Not_At   Complement
--     Or_At    Union
--     Xor_At   Distance
--
--  The type Classification has the following operations defined:
--
--  Tests
--
--     Is_In    Membership test and subset relation
--     <=, >=   Symmetric subset relation
--     =, /=    Symmetric equivalence relation
--
--  Per point operations. The result is a fuzzy set:
--
--     and      Intersection
--     or       Union
--
--  In-place variants of the operations above:
--
--     And_At   Intersection
--     Or_At    Union
--
with Fuzzy.Logic;  use Fuzzy.Logic;

package Fuzzy.Intuitionistic is
   pragma Preelaborate (Fuzzy.Intuitionistic);
--
-- Set -- An intuitionistic set
--
-- A  fuzzy intuitionistic set consists of two fuzzy sets. The upper set
-- tells how possible is that the given domain value belongs to the set.
-- The lower set tells how it is necessary.
--
   type Set (Cardinality : Positive) is record
      Possibility : Fuzzy.Set (1..Cardinality);
      Necessity   : Fuzzy.Set (1..Cardinality);
   end record;
--
-- Classification -- A fuzzy classification
--
-- A  classification consists of two fuzzy sets. The upper set tells how
-- possible is that the set is a subset of given domain value. The lower
-- set tells how it is necessary.
--
   type Classification is new Set;
--
-- Is_In -- Membership and inclusion
--
--    A - The first argument
--    B - The second argument
--
-- This function returns a logical value charaterizing whether A is
-- a subset of B. It is defined as:
--
--    (  Possibility => Possibility (B.Possibility, A.Possibility),
--       Necessity   => Necessity   (B.Necessity,   A.Possibility)
--    )
--
-- If one argument is a fuzzy set, then the result is defined as if  the
-- argument were a fuzzy intuitionistic set with both  components  equal
-- to the argument set. If the first argument  is  an  integer  and  the
-- second is an intuitionistic set then the result is either
--
--    (Possibility => B.Possiblity (A), Necessity => B.Necessity (A))
--
-- or  Certain_False  if  A  is  not  in 1..B.Cardinality. If the second
-- argument  is  an  integer  and  the  first   is   an   intuitionistic
-- classification then the result is either
--
--    (Possibility => A.Possiblity (B), Necessity => A.Necessity (B))
--
-- or Certain_False if B is not in 1..A.Cardinality.
--
-- Returns :
--
--    A is / included in B
--
-- Exceptions :
--
--    Constraint_Error - A and B have different cardinality
--
   function Is_In (A, B : Set) return Fuzzy_Boolean;
   function Is_In (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean;
   function Is_In (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean;
   function Is_In (A : Integer;   B : Set) return Fuzzy_Boolean;
   function Is_In (A : Classification; B : Integer)
      return Fuzzy_Boolean;
--
-- <=, >= -- Symmetric inclusion
--
--    A - To be tested if it is a subset to the second argument
--    B - The second argument
--
-- The  result of "<=" is a fuzzy logical value, which indicates whether
-- A is in B. It is defined as Is_In (A, B) and Is_In (not B, not A). If
-- one argument is a fuzzy set, then the result is  defined  as  if  the
-- argument were an intuitionistic set with both components equal to the
-- set.
--
-- Returns :
--
--    A is in B
--
-- Exceptions :
--
--    Constraint_Error - A and B have different cardinality
--
   function "<=" (A, B : Set) return Fuzzy_Boolean;
   function "<=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean;
   function "<=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean;
   function ">=" (A, B : Set) return Fuzzy_Boolean;
   function ">=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean;
   function ">=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean;
--
-- =, /= -- Symmetric equality and inequality
--
--    A - To be tested if it is same as the the second argument
--    B - The second argument
--
-- The result of "=" is a fuzzy logical value, which indicates whether A
-- is equivalent to B. It is defined as A <=  B  and  B  <=  A.  If  one
-- argument  is  a  fuzzy  set,  then  the  result  is defined as if the
-- argument were an intuitionistic set with both components equal to the
-- set.
--
-- Returns :
--
--    A is like B
--
-- Exceptions :
--
--    Constraint_Error - A and B have different cardinality
--
   function "=" (A, B : Set) return Fuzzy_Boolean;
   function "=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean;
   function "=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean;
   function "/=" (A, B : Set) return Fuzzy_Boolean;
   function "/=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean;
   function "/=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean;
--
-- not -- Fuzzy set complement
--
--    A - The argument
--
-- The result is defined as (not A.Necessity, not A.Possibility).
--
-- Returns :
--
--    The complement of the argument
--
   function "not" (A : Set) return Set;
--
-- and -- Intuitionistic intersection
--
--    A - The first argument
--    B - The second argument
--
-- Upper  sets  (possibilities)  and  the  lower  sets (necessities) are
-- intersected.
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
   function "and" (A : Set; B : Fuzzy.Set)  return Set;
   function "and" (A : Fuzzy.Set; B : Set)  return Set;
   function "and" (A : Set; B : Confidence) return Set;
   function "and" (A : Confidence; B : Set) return Set;
   function "and" (A, B : Classification)
      return Classification;
   function "and" (A : Classification; B : Fuzzy.Set)
      return Classification;
   function "and" (A : Fuzzy.Set; B : Classification)
      return Classification;
   function "and" (A : Classification; B : Confidence)
      return Classification;
   function "and" (A : Confidence; B : Classification)
      return Classification;
--
-- or -- Intuitionistic union
--
--    A - The first argument
--    B - The second argument
--
-- Upper  sets  (possibilities)  and  the  lower  sets (necessities) are
-- united.
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
   function "or" (A : Set; B : Fuzzy.Set)  return Set;
   function "or" (A : Fuzzy.Set; B : Set)  return Set;
   function "or" (A : Set; B : Confidence) return Set;
   function "or" (A : Confidence; B : Set) return Set;
   function "or" (A, B : Classification) return Classification;
   function "or" (A : Classification; B : Fuzzy.Set)
      return Classification;
   function "or" (A : Fuzzy.Set; B : Classification)
      return Classification;
   function "or" (A : Classification; B : Confidence)
      return Classification;
   function "or" (A : Confidence; B : Classification)
      return Classification;
--
-- xor -- Intuitionistic distance
--
--    A - The first argument
--    B - The second argument
--
-- The  result  is  defined as (A and not B) or (B and not A). Note that
-- the  upper  set  of  the result equals to the complement of the lower
-- set.
--
-- Returns :
--
--    A xor B
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   function "xor" (A, B : Set) return Set;
   function "xor" (A : Set; B : Fuzzy.Set)  return Set;
   function "xor" (A : Fuzzy.Set; B : Set)  return Set;
   function "xor" (A : Set; B : Confidence) return Set;
   function "xor" (A : Confidence; B : Set) return Set;
--
-- And_At -- Intuitionistic intersection with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- Upper  sets  (possibilities)  and  the  lower  sets (necessities) are
-- intersected.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure And_At (A : in out Set; B : Set);
   procedure And_At (A : in out Set; B : Fuzzy.Set);
   procedure And_At (A : in out Set; B : Confidence);
   procedure And_At (A : in out Classification; B : Classification);
   procedure And_At (A : in out Classification; B : Fuzzy.Set);
   procedure And_At (A : in out Classification; B : Confidence);
--
-- Not_At -- Intuitionistic complement with an accumulator
--
--    A - The argument
--
-- The result is defined as (not A.Necessity, not A.Possibility).
--
-- Returns :
--
--    The complement of the argument
--
   procedure Not_At (A : in out Set);
--
-- Or_At -- Intuitionistic union with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- Upper  sets  (possibilities)  and  the  lower  sets (necessities) are
-- united.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Or_At (A : in out Set; B : Set);
   procedure Or_At (A : in out Set; B : Fuzzy.Set);
   procedure Or_At (A : in out Set; B : Confidence);
   procedure Or_At (A : in out Classification; B : Classification);
   procedure Or_At (A : in out Classification; B : Fuzzy.Set);
   procedure Or_At (A : in out Classification; B : Confidence);
--
-- Xor_At -- Intuitionistic distance with an accumulator
--
--    A - The first argument
--    B - The second argument
--
-- The  result  is  defined as (A and not B) or (B and not A). Note that
-- the  upper  set  of  the result equals to the complement of the lower
-- set. I.e. the result is a fuzzy set.
--
-- Exceptions :
--
--    Constraint_Error - Arguments have different cardinality
--
   procedure Xor_At (A : in out Set; B : Set);
   procedure Xor_At (A : in out Set; B : Fuzzy.Set);
   procedure Xor_At (A : in out Set; B : Confidence);

private
   pragma Inline ("<=", ">=", "=", "/=");
   pragma Inline ("and", "not", "or", "xor");
   pragma Inline (And_At);
   pragma Inline (Is_In);
   pragma Inline (Not_At);
   pragma Inline (Or_At);
   pragma Inline (Xor_At);

end Fuzzy.Intuitionistic;
