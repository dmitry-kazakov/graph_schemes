--                                                                    --
--  package Fuzzy.Intuitionistic    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Fuzzy.Intuitionistic is

   function Member (A, BU, BL : Fuzzy.Set) return Fuzzy_Boolean;
   pragma Inline (Member);

   function Subset (AU, AL, BU, BL : Fuzzy.Set) return Fuzzy_Boolean;
   pragma Inline (Subset);

   function Equal (AU, AL, BU, BL : Fuzzy.Set) return Fuzzy_Boolean;
   pragma Inline (Equal);

   function Member (A, BU, BL : Fuzzy.Set)
      return Fuzzy_Boolean is
   begin
      if A'Length /= BU'Length or else A'Length /= BL'Length then
         raise Constraint_Error;
      end if;
      declare
         BU_Shift : constant Integer := BU'First - A'First;
         BL_Shift : constant Integer := BL'First - A'First;
         Upper_A  : Confidence;
         Upper_B  : Confidence;
         Lower_B  : Confidence;
         P_BU_A   : Confidence := Confidence'First;
         N_BL_A   : Confidence := Confidence'Last;
      begin
         for Index in A'Range loop
            Upper_A := A (Index);
            Upper_B := BU (Index + BU_Shift);
            Lower_B := BL (Index + BL_Shift);
            P_BU_A := P_BU_A or  (Upper_B and    Upper_A);
            N_BL_A := N_BL_A and (Lower_B or not Upper_A);
         end loop;
         return (P_BU_A, N_BL_A);
      end;
   end Member;

   function Subset (AU, AL, BU, BL : Fuzzy.Set)
      return Fuzzy_Boolean is
   begin
      if (  AU'Length /= BU'Length
         or else
            AU'Length /= BL'Length
         or else
            AU'Length /= AL'Length
         )
      then
         raise Constraint_Error;
      end if;
      declare
         AL_Shift : constant Integer := AL'First - AU'First;
         BU_Shift : constant Integer := BU'First - AU'First;
         BL_Shift : constant Integer := BL'First - AU'First;
         Upper_A  : Confidence;
         Lower_A  : Confidence;
         Upper_B  : Confidence;
         Lower_B  : Confidence;
         P_BU_AU  : Confidence := Confidence'First;
         N_BL_nAL : Confidence := Confidence'Last;
         N_BL_AU  : Confidence := Confidence'Last;
      begin
         for Index in AU'Range loop
            Upper_A  := AU (Index);
            Lower_A  := AL (Index + AL_Shift);
            Upper_B  := BU (Index + BU_Shift);
            Lower_B  := BL (Index + BL_Shift);
            P_BU_AU  := P_BU_AU  or  (Upper_B and    Upper_A);
            N_BL_nAL := N_BL_nAL and (Lower_B or     Lower_A);
            N_BL_AU  := N_BL_AU  and (Lower_B or not Upper_A);
         end loop;
         return (P_BU_AU and not N_BL_nAL, N_BL_AU);
      end;
   end Subset;

   function Equal (AU, AL, BU, BL : Fuzzy.Set)
      return Fuzzy_Boolean is
   begin
      if (  AU'Length /= BU'Length
         or else
            AU'Length /= BL'Length
         or else
            AU'Length /= AL'Length
         )
      then
         raise Constraint_Error;
      end if;
      declare
         AL_Shift : constant Integer := AL'First - AU'First;
         BU_Shift : constant Integer := BU'First - AU'First;
         BL_Shift : constant Integer := BL'First - AU'First;
         Upper_A  : Confidence;
         Lower_A  : Confidence;
         Upper_B  : Confidence;
         Lower_B  : Confidence;
         P_BU_AU  : Confidence := Confidence'First;
         P_BU_nAL : Confidence := Confidence'First;
         N_BL_nAL : Confidence := Confidence'Last;
         N_BL_AU  : Confidence := Confidence'Last;
      begin
         for Index in AU'Range loop
            Upper_A  := AU (Index);
            Lower_A  := AL (Index + AL_Shift);
            Upper_B  := BU (Index + BU_Shift);
            Lower_B  := BL (Index + BL_Shift);
            P_BU_AU  := P_BU_AU  or  (Upper_B and     Upper_A);
            P_BU_nAL := P_BU_nAL or  (Upper_B and not Lower_A);
            N_BL_nAL := N_BL_nAL and (Lower_B or     Lower_A);
            N_BL_AU  := N_BL_AU  and (Lower_B or  not Upper_A);
         end loop;
         return (P_BU_AU and not N_BL_nAL, N_BL_AU and not P_BU_nAL);
      end;
   end Equal;

   function Is_In (A, B : Set) return Fuzzy_Boolean is
   begin
      return Member (A.Possibility, B.Possibility, B.Necessity);
   end Is_In;

   function Is_In (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return Member (A.Possibility, B, B);
   end Is_In;
   
   function Is_In (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean is
   begin
      return Member (A, B.Possibility, B.Necessity);
   end Is_In;

   function Is_In (A : Integer; B : Set) return Fuzzy_Boolean is
   begin
      if A in 1..B.Cardinality then
         return (B.Possibility (A), B.Necessity (A));
      else
         return Certain_False;
      end if; 
   end Is_In;

   function Is_In (A : Classification; B : Integer)
      return Fuzzy_Boolean is
   begin
      if B in 1..A.Cardinality then
         return (A.Possibility (B), A.Necessity (B));
      else
         return Certain_False;
      end if; 
   end Is_In;

   function "<=" (A, B : Set) return Fuzzy_Boolean is
   begin
      return
         Subset (A.Possibility, A.Necessity, B.Possibility, B.Necessity);
   end "<=";

   function "<=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean is
   begin
      return Subset (A, A, B.Possibility, B.Necessity);
   end "<=";

   function "<=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return Subset (A.Possibility, A.Necessity, B, B);
   end "<=";

   function ">=" (A, B : Set) return Fuzzy_Boolean is
   begin
      return B <= A;
   end ">=";

   function ">=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean is
   begin
      return B <= A;
   end ">=";

   function ">=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return B <= A;
   end ">=";

   function "=" (A, B : Set) return Fuzzy_Boolean is
   begin
      return
         Equal (A.Possibility, A.Necessity, B.Possibility, B.Necessity);
   end "=";

   function "=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return Equal (A.Possibility, A.Necessity, B, B);
   end "=";

   function "=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean is
   begin
      return Equal (A, A, B.Possibility, B.Necessity);
   end "=";

   function "/=" (A, B : Set) return Fuzzy_Boolean is
   begin
      return not A = B;
   end "/=";

   function "/=" (A : Set; B : Fuzzy.Set) return Fuzzy_Boolean is
   begin
      return not A = B;
   end "/=";

   function "/=" (A : Fuzzy.Set; B : Set) return Fuzzy_Boolean is
   begin
      return not A = B;
   end "/=";

   function "not" (A : Set) return Set is
      Result : Set (A.Cardinality);
   begin
      for Index in 1..A.Cardinality loop
         Result.Possibility (Index) := not A.Necessity (Index);
         Result.Necessity (Index)   := not A.Possibility (Index);
      end loop;
      return Result;
   end "not";

   procedure Not_At (A : in out Set) is
      Necessity : Confidence;
   begin
      for Index in 1..A.Cardinality loop
         Necessity := not A.Possibility (Index);
         A.Possibility (Index) := not A.Necessity (Index);
         A.Necessity (Index) := Necessity;
      end loop;
   end Not_At;

   function "and" (A, B : Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B.Possibility,
         Necessity   => A.Necessity   and B.Necessity
      );
   end "and";
   
   function "and" (A : Set; B : Fuzzy.Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B,
         Necessity   => A.Necessity   and B
      );
   end "and";

   function "and" (A : Fuzzy.Set; B : Set) return Set is
   begin
      return B and A;
   end "and";
   
   function "and" (A : Set; B : Confidence)
      return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B,
         Necessity   => A.Necessity   and B
      );
   end "and";

   function "and" (A : Confidence; B : Set)
      return Set is
   begin
      return B and A;
   end "and";

   function "and" (A, B : Classification) return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B.Possibility,
         Necessity   => A.Necessity   or  B.Necessity
      );
   end "and";

   function "and" (A : Classification; B : Fuzzy.Set)
      return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B,
         Necessity   => A.Necessity   or  B
      );
   end "and";

   function "and" (A : Fuzzy.Set; B : Classification)
      return Classification is
   begin
      return B and A;
   end "and";
   
   function "and" (A : Classification; B : Confidence)
      return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility and B,
         Necessity   => A.Necessity   or  B
      );
   end "and";

   function "and" (A : Confidence; B : Classification)
      return Classification is
   begin
      return B and A;
   end "and";

   procedure And_At (A : in out Set; B : Set) is
   begin
      And_At (A.Possibility, B.Possibility);
      And_At (A.Necessity,   B.Necessity);
   end And_At;

   procedure And_At (A : in out Set; B : Fuzzy.Set) is
   begin
      And_At (A.Possibility, B);
      And_At (A.Necessity,   B);
   end And_At;

   procedure And_At (A : in out Set; B : Confidence) is
   begin
      And_At (A.Possibility, B);
      And_At (A.Necessity,   B);
   end And_At;

   procedure And_At (A : in out Classification; B : Classification) is
   begin
      And_At (A.Possibility, B.Possibility);
      Or_At  (A.Necessity,   B.Necessity);
   end And_At;

   procedure And_At (A : in out Classification; B : Fuzzy.Set) is
   begin
      And_At (A.Possibility, B);
      Or_At  (A.Necessity,   B);
   end And_At;

   procedure And_At (A : in out Classification; B : Confidence) is
   begin
      And_At (A.Possibility, B);
      Or_At  (A.Necessity,   B);
   end And_At;

   function "or" (A, B : Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B.Possibility,
         Necessity   => A.Necessity   or B.Necessity
      );
   end "or";
   
   function "or" (A : Set; B : Fuzzy.Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B,
         Necessity   => A.Necessity   or B
      );
   end "or";
   
   function "or" (A : Fuzzy.Set; B : Set) return Set is
   begin
      return B or A;
   end "or";
   
   function "or" (A : Set; B : Confidence) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B,
         Necessity   => A.Necessity   or B
      );
   end "or";

   function "or" (A : Confidence; B : Set) return Set is
   begin
      return B or A;
   end "or";

   function "or" (A, B : Classification) return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B.Possibility,
         Necessity   => A.Necessity  and B.Necessity
      );
   end "or";
   
   function "or" (A : Classification; B : Fuzzy.Set)
      return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B,
         Necessity   => A.Necessity  and B
      );
   end "or";
   
   function "or" (A : Fuzzy.Set; B : Classification)
      return Classification is
   begin
      return B or A;
   end "or";
   
   function "or" (A : Classification; B : Confidence)
      return Classification is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility or B,
         Necessity   => A.Necessity  and B
      );
   end "or";

   function "or" (A : Confidence; B : Classification)
      return Classification is
   begin
      return B or A;
   end "or";

   procedure Or_At (A : in out Set; B : Set) is
   begin
      Or_At (A.Possibility, B.Possibility);
      Or_At (A.Necessity,   B.Necessity);
   end Or_At;

   procedure Or_At (A : in out Set; B : Fuzzy.Set) is
   begin
      Or_At (A.Possibility, B);
      Or_At (A.Necessity,   B);
   end Or_At;

   procedure Or_At (A : in out Set; B : Confidence) is
   begin
      Or_At (A.Possibility, B);
      Or_At (A.Necessity,   B);
   end Or_At;

   procedure Or_At (A : in out Classification; B : Classification) is
   begin
      Or_At  (A.Possibility, B.Possibility);
      And_At (A.Necessity,   B.Necessity);
   end Or_At;

   procedure Or_At (A : in out Classification; B : Fuzzy.Set) is
   begin
      Or_At  (A.Possibility, B);
      And_At (A.Necessity,   B);
   end Or_At;

   procedure Or_At (A : in out Classification; B : Confidence) is
   begin
      Or_At  (A.Possibility, B);
      And_At (A.Necessity,   B);
   end Or_At;

   function "xor" (A, B : Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility xor B.Possibility,
         Necessity   => A.Necessity   xor B.Necessity
      );
   end "xor";

   function "xor" (A : Set; B : Fuzzy.Set) return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility xor B,
         Necessity   => A.Necessity   xor B
      );
   end "xor";

   function "xor" (A : Fuzzy.Set; B : Set) return Set is
   begin
      return B xor A;
   end "xor";
   
   function "xor" (A : Set; B : Confidence)
      return Set is
   begin
      return
      (  Cardinality => A.Cardinality,
         Possibility => A.Possibility xor B,
         Necessity   => A.Necessity   xor B
      );
   end "xor";
   
   function "xor" (A : Confidence; B : Set)
      return Set is
   begin
      return B xor A;
   end "xor";

   procedure Xor_At (A : in out Set; B : Set) is
   begin
      Xor_At (A.Possibility, B.Possibility);
      Xor_At (A.Necessity,   B.Necessity);
   end Xor_At;

   procedure Xor_At (A : in out Set; B : Fuzzy.Set) is
   begin
      Xor_At (A.Possibility, B);
      Xor_At (A.Necessity,   B);
   end Xor_At;

   procedure Xor_At (A : in out Set; B : Confidence) is
   begin
      Xor_At (A.Possibility, B);
      Xor_At (A.Necessity,   B);
   end Xor_At;

end Fuzzy.Intuitionistic;
