--                                                                    --
--  package Fuzzy.Logic             Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Fuzzy.Logic is

   function Is_In (A, B : Set) return Fuzzy_Boolean is
      subtype A_Type is Set (A'Range);
      function Do_It (A, B : A_Type) return Fuzzy_Boolean;
      pragma Inline (Do_It);

      function Do_It (A, B : A_Type) return Fuzzy_Boolean is
         Ai   : Confidence;
         Bi   : Confidence;
         P_BA : Confidence := Confidence'First;
         N_BA : Confidence := Confidence'Last;
      begin
         for Index in A'Range loop
            Ai := A (Index);
            Bi := B (Index);
            P_BA := P_BA or (Bi and Ai);
            N_BA := N_BA and (Bi or not Ai);
         end loop;
         return (P_BA, N_BA);
      end Do_It;
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if;
      return Do_It (A, B);
   end Is_In;

   function To_Fuzzy (Left : Boolean) return Fuzzy_Boolean is
   begin
      if Left then
         return Certain_True;
      else
         return Certain_False;
      end if;
   end To_Fuzzy;

   function "not" (Left : Fuzzy_Boolean) return Fuzzy_Boolean is
   begin
      return (not Left.Necessity, not Left.Possibility);
   end "not";

   function "and" (Left, Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return
      (  Left.Possibility and Right.Possibility,
         Left.Necessity   and Right.Necessity
      );
   end "and";
   
   function "and" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return Left;
      else
         return Certain_False;
      end if;
   end "and";
   
   function "and" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return Right;
      else
         return Certain_False;
      end if;
   end "and";

   function "or" (Left, Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return
      (  Left.Possibility or Right.Possibility,
         Left.Necessity   or Right.Necessity
      );
   end "or";
   
   function "or" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return Certain_True;
      else
         return Left;
      end if;
   end "or";
   
   function "or" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return Certain_True;
      else
         return Right;
      end if;
   end "or";

   function "xor" (Left, Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return
      (  (  ((not Left.Necessity) and Right.Possibility)
         or (Left.Possibility and (not Right.Necessity))
         ),
         (  ((not Left.Possibility) and Right.Necessity)
         or (Left.Necessity and (not Right.Possibility))
      )  );
   end "xor";

   function "xor" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return not Left;
      else
         return Left;
      end if;
   end "xor";
   
   function "xor" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return not Right;
      else
         return Right;
      end if;
   end "xor";
   
   function "*" (Left, Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return
      (  Left.Possibility and Right.Possibility,
         Left.Necessity   or  Right.Necessity
      );
   end "*";
   
   function "*" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return (Left.Possibility, Confidence'Last);
      else
         return (Confidence'First, Left.Necessity);
      end if;
   end "*";
   
   function "*" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return (Right.Possibility, Confidence'Last);
      else
         return (Confidence'First, Right.Necessity);
      end if;
   end "*";

   function "+" (Left, Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return
      (  Left.Possibility or Right.Possibility,
         Left.Necessity  and Right.Necessity
      );
   end "+";
   
   function "+" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return (Confidence'Last, Left.Necessity);
      else
         return (Left.Possibility, Confidence'First);
      end if;
   end "+";
   
   function "+" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return (Confidence'Last, Right.Necessity);
      else
         return (Right.Possibility, Confidence'First);
      end if;
   end "+";

   function ">=" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean is
   begin
      return
      (  not Left.Possibility or Right.Possibility,
         not Left.Necessity   or Right.Necessity
      );
   end ">=";

   function ">=" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      if Right then
         return Certain_True;
      else
         return (not Left.Possibility, not Left.Necessity);
      end if;
   end ">=";

   function ">=" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      if Left then
         return Right;
      else
         return Certain_True;
      end if;
   end ">=";
   
   function "<=" (Left, Right : Fuzzy_Boolean) return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Fuzzy_Boolean; Right : Boolean)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";

   function "<=" (Left : Boolean; Right : Fuzzy_Boolean)
      return Fuzzy_Boolean is
   begin
      return Right >= Left;
   end "<=";
   
   function Certainty (Left : Fuzzy_Boolean) return Confidence is
   begin
      return not (Left.Possibility - Left.Necessity);
   end Certainty;

end Fuzzy.Logic;
