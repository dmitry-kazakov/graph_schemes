--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Parsers.FCL.Code.Predicates.                Luebeck            --
--        Lattice                                  Spring, 2005       --
--  Separate body implementation                                      --
--                                Last revision :  10:01 09 Apr 2016  --
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

separate (Parsers.FCL.Code.Predicates) package body Lattice is

   procedure And_To
             (  Conjunction : in out Logical_Conjunction;
                Feature     : Feature_Handle;
                Term        : Logical_Term
             )  is
      pragma Inline (And_To);
   begin
      if Is_Lower (Term) then
         Erase (Conjunction);
      elsif not Is_Upper (Term) then
         declare
            Index : constant Integer := Find (Conjunction, Feature);
         begin
            if Index > 0 then
               declare
                  Replacement : constant Logical_Term :=
                                Get (Conjunction, Index) and Term;
               begin
                  if Is_Lower (Replacement) then
                     Erase (Conjunction);
                  else
                     Replace (Conjunction, Index, Replacement);
                  end if;
               end;
            else
               Add (Conjunction, Feature, Term);
            end if;
         end;
      end if;
   end And_To;

   procedure Or_To
             (  Disjunction : in out Logical_Form;
                Feature     : Feature_Handle;
                Term        : Logical_Term
             )  is
      Replacements : Disjunctions.Set;
      Result       : Logical_Term;
      Index        : Integer;
      Item         : Positive := 1;
      Conjunction  : Logical_Conjunction;
   begin
      if Is_Upper (Term) then
         Erase (Disjunction.Value);
      elsif not Is_Lower (Term) then
         while Item <= Get_Size (Disjunction.Value) loop
            Conjunction := Get (Disjunction.Value, Item);
            Index := Find (Conjunction, Feature);
            if Index > 0 then
               --
               -- This conjunction has same feature
               --
               if Get_Size (Conjunction) = 1 then
                  -- ((a) or b) = ((a or b))
                  Result := Get (Conjunction, 1) or Term;
                  if Is_Upper (Result) then
                     Erase (Disjunction.Value);
                     return;
                  else
                     Replace (Conjunction, Index, Result);
                     Remove  (Disjunction.Value, Item);
                     Replace (Replacements, Conjunction);
                  end if;
               else
                  -- ((... and a) or b) = b if b >= a
                  if Term >= Get (Conjunction, Index) then
                     Remove (Disjunction.Value, Item);
                  else
                     Item := Item + 1;
                  end if;
               end if;
            else
               Item := Item + 1;
            end if;
         end loop;
         if Get_Size (Replacements) > 0 then
            Add (Disjunction.Value, Replacements);
         else
            Erase (Conjunction);
            Add (Conjunction, Feature, Term);
            Add (Disjunction.Value, Conjunction);
         end if;
      end if;
   end Or_To;

   function Normalize (Disjunction : Logical_Form) return Logical_Form is
      type Counters is
         array (Positive range 1..Get_Size (Disjunction.Value))
            of Positive;            
      This        : Counters;
      Size        : Counters;
      Result      : Logical_Form;
      Conjunction : Logical_Conjunction;
   begin
      Result.Location := Disjunction.Location;
      Result.Form     := This_Form;
      for Index in Size'Range loop
         This (Index) := 1;
         Size (Index) := Get_Size (Get (Disjunction.Value, Index));
      end loop;
      loop
         Erase (Conjunction);
         for Index in This'Range loop
            declare
               Item    : constant Positive := This (Index);
               Current : Logical_Conjunction renames
                            Get (Disjunction.Value, Index);
            begin
               And_To
               (  Conjunction,
                  Get_Key (Current, Item),
                  Get (Current, Item)
               );
            end;
         end loop;
         if Get_Size (Conjunction) = 1 then
            Or_To
            (  Result,
               Get_Key (Conjunction, 1),
               Get (Conjunction, 1)
            );
         else
            Add (Result.Value, Conjunction);
         end if;
         for Index in reverse This'Range loop
            if This (Index) < Size (Index) then
               This (Index) := This (Index) + 1;
               exit;
            elsif Index = 1 then
               return Result;
            end if;
            This (Index) := 1;
         end loop;
      end loop;
   end Normalize;

   procedure Or_To (Left : in out Logical_Form; Right : Logical_Form) is
      pragma Inline (Or_To);
      Result     : Logical_Form;
      Singletons : Disjunctions.Set;
   begin
      Result.Form := This_Form;
      for Item in 1..Get_Size (Left.Value) loop
         declare
            Conjunction : Logical_Conjunction renames
               Get (Left.Value, Item);
         begin
            if Get_Size (Conjunction) = 1 then
               Replace (Singletons, Conjunction);
            else
               Replace (Result.Value, Conjunction);
            end if;
         end;
      end loop;
      for Item in 1..Get_Size (Right.Value) loop
         declare
            Conjunction : Logical_Conjunction renames
               Get (Right.Value, Item);
         begin
            if Get_Size (Conjunction) = 1 then
               Replace (Singletons, Conjunction);
            else
               Replace (Result.Value, Conjunction);
            end if;
         end;
      end loop;
      for Item in 1..Get_Size (Singletons) loop
         declare
            Conjunction : Logical_Conjunction renames
               Get (Singletons, Item);
         begin
            Or_To
            (  Result,
               Get_Key (Conjunction, 1),
               Get (Conjunction, 1)
            );
         end;
      end loop;
      Left := Result;
   end Or_To;

   procedure Or_At (Left : in out Logical_Form; Right : Logical_Form) is
   begin
      if Left.Form /= This_Form then
         Left := Normalize (Left);
      end if;
      if Right.Form = This_Form then
         Or_To (Left, Right);
      else
         Or_To (Left, Normalize (Right));
      end if;
   end Or_At;

   function "not" (Left : Logical_Conjunction)
      return Logical_Conjunction is
      Result : Logical_Conjunction;
   begin
      for Index in 1..Get_Size (Left) loop
         And_To
         (  Result,
            Get_Key (Left, Index),
            not Get (Left, Index)
         );
      end loop;
      return Result;
   end "not";

   function "not" (Left : Logical_Form) return Logical_Form is
      Result : Logical_Form;
   begin
      Result.Location := Left.Location;
      if This_Form = DNF then
         Result.Form := CNF;
      else
         Result.Form := DNF;
      end if;
      for Index in 1..Get_Size (Left.Value) loop
         Add (Result.Value, not Get (Left.Value, Index));
      end loop;
      return Result;
   end "not";

end Lattice;
