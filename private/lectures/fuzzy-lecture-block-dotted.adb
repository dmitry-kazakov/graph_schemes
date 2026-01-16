--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Block.Dotted                  Luebeck            --
--  Implementation                                 Winter, 2002       --
--                                                                    --
--                                Last revision :  12:48 30 Aug 2010  --
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

package body Fuzzy.Lecture.Block.Dotted is

   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Set is
   begin
      if Block.Data (Index) <= 0 then
         return (1..Block.Cardinality => Confidence'Last);
      else
         declare
            Result : Set (1..Block.Cardinality) :=
                        (others => Confidence'First);
         begin
            Result (Block.Data (Index)) := Confidence'Last;
            return Result;
         end;
      end if;
   end Get;

   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex;
               Value : Positive
            )  return Confidence is
   begin
      if Value > Block.Cardinality then
         raise Constraint_Error;
      else
         if Block.Data (Index) <= 0 then
            return Confidence'Last;
         else
            if Block.Data (Index) = Value then
               return Confidence'Last;
            else
               return Confidence'First;
            end if;
         end if;
      end if;
   end Get;

   function Get
            (  Block : Dot_Column_Block;
               Index : Subindex;
               Value : Set
            )  return Confidence is
   begin
      if Value'Last > Block.Cardinality then
         raise Constraint_Error;
      else
         if Block.Data (Index) <= 0 then
            return Confidence'First;
         else
            return Value (Block.Data (Index));
         end if;
      end if;
   end Get;

   function Is_Defined
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Value_Status is
   begin
      if Block.Data (Index) in 1..Block.Cardinality then
         return Defined;
      elsif Block.Data (Index) = Undefined_Index then
         return Undefined;
      else
         return Uncertain;
      end if;
   end Is_Defined;

   function Is_Known
            (  Block : Dot_Column_Block;
               Index : Subindex
            )  return Boolean is
   begin
      return Block.Data (Index) in 1..Block.Cardinality;
   end Is_Known;

   procedure Put
             (  Block : in out Dot_Column_Block;
                Index : Subindex;
                Value : Set
             )  is
      Point : Natural := 0;
   begin
      for Index in Value'Range loop
         if Value (Index) /= Confidence'First then
            if Value (Index) /= Confidence'Last or else 0 /= Point then
               raise Constraint_Error;
            end if;
            Point := Index - Value'First + 1;
         end if;
      end loop;
      Put (Block, Index, Point);
   end Put;

   procedure Put
             (  Block : in out Dot_Column_Block;
                Index : Subindex;
                Value : Positive
             )  is
   begin
      if Value > Block.Cardinality then
         raise Constraint_Error;
      else
         if Block.Data (Index) <= 0 then
            Block.Data (Index) := Value;
         elsif Block.Data (Index) /= Value then
            raise Constraint_Error;
         end if;
      end if;
   end Put;

   procedure Set_Undefined
             (  Block  : in out Dot_Column_Block;
                Index  : Subindex;
                Status : Undefined_Status
             )  is
   begin
      case Status is
         when Uncertain =>
            Block.Data (Index) := Uncertain_Index;
         when Undefined =>
            Block.Data (Index) := Undefined_Index;
      end case;
   end Set_Undefined;

end Fuzzy.Lecture.Block.Dotted;
