--                                                                    --
--  package Fuzzy                   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

package body Fuzzy is

   generic
      with function "**" (Left, Right : Confidence) return Confidence;
   package Operations is
      function Operator (A, B : Set) return Set;
      procedure In_Place (A : in out Set; B : Set);
   end;

   package body Operations is
      procedure In_Place (A : in out Set; B : Set) is
         subtype A_Type is Set (A'Range);
         procedure Do_It (B : A_Type);
         pragma Inline (Do_It);

         procedure Do_It (B : A_Type) is
         begin
            for Index in A'Range loop
               A (Index) := A (Index) ** B (Index);
            end loop;
         end Do_It;
      begin
         if A'Length /= B'Length then
            raise Constraint_Error;
         end if; 
         Do_It (B);
      end In_Place;      

      function Operator (A, B : Set) return Set is
         subtype A_Type is Set (A'Range);
         function Do_It (A, B : A_Type) return A_Type;
         pragma Inline (Do_It);

         function Do_It (A, B : A_Type) return A_Type is
            Result : Set (A'Range);
         begin
            for Index in A_Type'Range loop
               Result (Index) := A (Index) ** B (Index);
            end loop;
            return Result;
         end Do_It;
      begin
         if A'Length /= B'Length then
            raise Constraint_Error;
         end if; 
         return Do_It (A, B);
      end Operator;
   end Operations;

   function Is_In (Point : Integer; A : Set) return Confidence is
   begin
      if Point not in A'Range then
         return Confidence'First;
      else
         return A (Point);
      end if;
   end Is_In;

   function "not" (A : Set) return Set is
      Result : Set (A'Range);
   begin
      for Index in A'Range loop
         Result (Index) := not A (Index);
      end loop;
      return Result;
   end "not";

   procedure Not_At (A : in out Set) is
   begin
      for Index in A'Range loop
         A (Index) := not A (Index);
      end loop;
   end Not_At;

   package And_Operations is new Operations ("and");
   function "and" (A, B : Set) return Set
      renames And_Operations.Operator;
   procedure And_At (A : in out Set; B : Set)
      renames And_Operations.In_Place;

   function "and" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'Last then
         return A;
      elsif B = Confidence'First then
         return (A'Range => Confidence'First);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) and B;
            end loop;
            return Result;
         end;
      end if;
   end "and";      

   function "and" (A : Confidence; B : Set) return Set is
   begin
      return B and A;
   end "and";      

   procedure And_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'Last then
         for Index in A'Range loop
            A (Index) := A (Index) and B;
         end loop;
      end if;
   end And_At;

   procedure And_At (A : in out Set; B : Set; C : Confidence) is
      subtype A_Type is Set (A'Range);
      procedure Do_It (B : A_Type);
      pragma Inline (Do_It);

      procedure Do_It (B : A_Type) is
      begin
         for Index in A'Range loop
            A (Index) := A (Index) and (B (Index) or C);
         end loop;
      end Do_It;
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if; 
      Do_It (B);
   end And_At;

   package Or_Operations is new Operations ("or");
   function "or" (A, B : Set) return Set
      renames Or_Operations.Operator;
   procedure Or_At (A : in out Set; B : Set)
      renames Or_Operations.In_Place;

   function "or" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'First then
         return A;
      elsif B = Confidence'Last then
         return (A'Range => Confidence'Last);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) or B;
            end loop;
            return Result;
         end;
      end if;
   end "or";

   function "or" (A : Confidence; B : Set) return Set is
   begin
      return B or A;
   end "or";      

   procedure Or_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'First then
         for Index in A'Range loop
            A (Index) := A (Index) or B;
         end loop;
      end if;
   end Or_At;

   procedure Or_At (A : in out Set; B : Set; C : Confidence) is
      subtype A_Type is Set (A'Range);
      procedure Do_It (B : A_Type);
      pragma Inline (Do_It);

      procedure Do_It (B : A_Type) is
      begin
         for Index in A'Range loop
            A (Index) := A (Index) or (B (Index) and C);
         end loop;
      end Do_It;
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if; 
      Do_It (B);
   end Or_At;

   function Mul (Left, Right : Confidence) return Confidence is
      Last   : constant Float := Float (Confidence'Last);
      Result : constant Float :=
                  (Float (Left) / Last) * (Float (Right) / Last);
   begin
      if Result >= Last then
         return Confidence'Last;
      else
         return Confidence (Result);
      end if;
   end Mul;

   package Mul_Operations is new Operations (Mul);
   function "*" (A, B : Set) return Set
      renames Mul_Operations.Operator;
   procedure Mul_At (A : in out Set; B : Set)
      renames Mul_Operations.In_Place;

   function "*" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'Last then
         return A;
      elsif B = Confidence'First then
         return (A'Range => Confidence'First);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) * B;
            end loop;
            return Result;
         end;
      end if;
   end "*";      

   function "*" (A : Confidence; B : Set) return Set is
   begin
      return B * A;
   end "*";      

   procedure Mul_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'Last then
         for Index in A'Range loop
            A (Index) := A (Index) * B;
         end loop;
      end if;
   end Mul_At;      

   package Add_Operations is new Operations ("+");
   function "+" (A, B : Set) return Set
      renames Add_Operations.Operator;
   procedure Add_At (A : in out Set; B : Set)
      renames Add_Operations.In_Place;

   function "+" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'First then
         return A;
      elsif B = Confidence'Last then
         return (A'Range => Confidence'Last);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) + B;
            end loop;
            return Result;
         end;
      end if;
   end "+";      

   function "+" (A : Confidence; B : Set) return Set is
   begin
      return B + A;
   end "+";

   procedure Add_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'First then
         for Index in A'Range loop
            A (Index) := A (Index) + B;
         end loop;
      end if;
   end Add_At;      

   package Diff_Operations is new Operations ("-");
   function "-" (A, B : Set) return Set
      renames Diff_Operations.Operator;
   procedure Diff_At (A : in out Set; B : Set)
      renames Diff_Operations.In_Place;

   function "-" (A : Set; B : Confidence) return Set is
      Result : Set (A'Range);
   begin
      for Index in A'Range loop
         Result (Index) := A (Index) xor B;
      end loop;
      return Result;
   end "-";      

   function "-" (A : Confidence; B : Set) return Set is
   begin
      return B - A;
   end "-";
   
   procedure Diff_At (A : in out Set; B : Confidence) is
   begin
      for Index in A'Range loop
         A (Index) := A (Index) xor B;
      end loop;
   end Diff_At;      

   package Xor_Operations is new Operations ("xor");
   function "xor" (A, B : Set) return Set
      renames Xor_Operations.Operator;
   procedure Xor_At (A : in out Set; B : Set)
      renames Xor_Operations.In_Place;

   function "xor" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'First then
         return A;
      elsif B = Confidence'Last then
         return not A;
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) xor B;
            end loop;
            return Result;
         end;
      end if;
   end "xor";      

   function "xor" (A : Confidence; B : Set) return Set is
   begin
      return B xor A;
   end "xor";      

   procedure Xor_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'First then
         for Index in A'Range loop
            A (Index) := A (Index) xor B;
         end loop;
      end if;
   end Xor_At;      

   package Mod_Operations is new Operations ("mod");
   function "mod" (A, B : Set) return Set
      renames Mod_Operations.Operator;
   procedure Mod_At (A : in out Set; B : Set)
      renames Mod_Operations.In_Place;

   function "mod" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'First then
         return A;
      elsif B = Confidence'Last then
         return (A'Range => Confidence'First);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) mod B;
            end loop;
            return Result;
         end;
      end if;
   end "mod";      

   function "mod" (A : Confidence; B : Set) return Set is
      Result : Set (B'Range);
   begin
      for Index in B'Range loop
         Result (Index) := A mod B (Index);
      end loop;
      return Result;
   end "mod";      

   procedure Mod_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'First then
         for Index in A'Range loop
            A (Index) := A (Index) mod B;
         end loop;
      end if;
   end Mod_At;

   package Rem_Operations is new Operations ("rem");
   function "rem" (A, B : Set) return Set
      renames Rem_Operations.Operator;
   procedure Rem_At (A : in out Set; B : Set)
      renames Rem_Operations.In_Place;

   function "rem" (A : Set; B : Confidence) return Set is
   begin
      if B = Confidence'Last then
         return A;
      elsif B = Confidence'First then
         return (A'Range => Confidence'Last);
      else
         declare
            Result : Set (A'Range);
         begin
            for Index in A'Range loop
               Result (Index) := A (Index) rem B;
            end loop;
            return Result;
         end;
      end if;
   end "rem";      

   function "rem" (A : Confidence; B : Set) return Set is
      Result : Set (B'Range);
   begin
      for Index in B'Range loop
         Result (Index) := A rem B (Index);
      end loop;
      return Result;
   end "rem";

   procedure Rem_At (A : in out Set; B : Confidence) is
   begin
      if B /= Confidence'Last then
         for Index in A'Range loop
            A (Index) := A (Index) rem B;
         end loop;
      end if;
   end Rem_At;      

   function Possibility (A, B : Set) return Confidence is
      subtype A_Type is Set (A'Range);
      function Do_It (A, B : A_Type) return Confidence;
      pragma Inline (Do_It);

      function Do_It (A, B : A_Type) return Confidence is
         Sup : Confidence := Confidence'First;
      begin
         for Index in A'Range loop
            Sup := Sup or (A (Index) and B (Index));
         end loop;
         return Sup;
      end Do_It;
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if;
      return Do_It (A, B);
   end Possibility;

   function Possibility (A : Set) return Confidence is
      Sup : Confidence := Confidence'First;
   begin
      for Index in A'Range loop
         Sup := Sup or A (Index);
      end loop;
      return Sup;
   end Possibility;

   function Necessity (A, B : Set) return Confidence is
      subtype A_Type is Set (A'Range);
      function Do_It (A, B : A_Type) return Confidence;
      pragma Inline (Do_It);

      function Do_It (A, B : A_Type) return Confidence is
         Inf : Confidence := Confidence'Last;
      begin
         for Index in A'Range loop
            Inf := Inf and (A (Index) or not B (Index));
         end loop;
         return Inf;
      end Do_It;
   begin
      if A'Length /= B'Length then
         raise Constraint_Error;
      end if;
      return Do_It (A, B);
   end Necessity;

   function Necessity (A : Set) return Confidence is
      Inf : Confidence := Confidence'Last;
   begin
      for Index in A'Range loop
         Inf := Inf and A (Index);
      end loop;
      return Inf;
   end Necessity;

   function Distance (Point : Integer; A : Set) return Natural is
      Offset : constant Natural := 0;
   begin
      if Point not in A'Range then
         return Natural'Last;
      end if;
      loop
         if (  (  Point - Offset >= A'First
               and then
                  Confidence'First /= A (Point - Offset)
               )
            or else
               (  Point + Offset <= A'Last
               and then
                  Confidence'First /= A (Point + Offset)
            )  )
         then
            return Offset;
         end if;
         exit when
            (  Point - Offset < A'First
            and
               Point + Offset > A'Last
            );
      end loop;
      return Natural'Last;
   end Distance;

end Fuzzy;
