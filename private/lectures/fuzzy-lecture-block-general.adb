--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Block.General                 Luebeck            --
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

package body Fuzzy.Lecture.Block.General is
   function Get
            (  Block : Set_Column_Block;
               Index : Subindex
            )  return Set is
   begin
      if Block.Flags (Index) = Defined then
         declare
            Table  : Set_Array renames Block.Data;
            Result : Set (Table'range (2));
         begin
            for Domain_Index in Result'range loop
               Result (Domain_Index) := Table (Index, Domain_Index);
            end loop;
            return Result;
         end;
      else
         return (1..Block.Cardinality => Confidence'Last);
      end if;
   end Get;

   function Get
            (  Block : Set_Column_Block;
               Index : Subindex;
               Value : Positive
            )  return Confidence is
   begin
      if Value > Block.Cardinality then
         raise Constraint_Error;
      else
         if Block.Flags (Index) = Defined then
            return Block.Data (Index, Value);
         else
            return Confidence'Last;
         end if;
      end if;
   end Get;

   function Get
            (  Block : Set_Column_Block;
               Index : Subindex;
               Value : Set
            )  return Confidence is
   begin
      if Value'Length /= Block.Cardinality then
         raise Constraint_Error;
      end if;
      if Block.Flags (Index) = Defined then
         declare
            Table  : Set_Array renames Block.Data;
            Result : Confidence;
         begin
            Result := Confidence'First;
            for Domain_Index in Value'range loop
               Result :=
                  (  Result
                  or
                     (  Value (Domain_Index)
                     and
                        Table (Index, Domain_Index)
                  )  );
            end loop;
            return Result;
         end;
      else
         return Confidence'First;
      end if;
   end Get;

   function Is_Defined
            (  Block : Set_Column_Block;
               Index : Subindex
            )  return Value_Status is
   begin
      return Block.Flags (Index);
   end Is_Defined;

   function Is_Known
            (  Block : Set_Column_Block;
               Index : Subindex
            )  return Boolean is
   begin
      if Block.Flags (Index) = Defined then
         declare
            Table : Set_Array renames Block.Data;
         begin
            for Domain_Index in Table'Range (2) loop
               if Table (Index, Domain_Index) /= Confidence'Last then
                  return True;
               end if;
            end loop;
         end;
      end if;
      return False;
   end Is_Known;

   procedure Put
             (  Block : in out Set_Column_Block;
                Index : Subindex;
                Value : Set
             )  is
      Table : Set_Array renames Block.Data;
   begin
      if Value'Length /= Block.Cardinality then
         raise Constraint_Error;
      end if;
      for Domain_Index in Table'Range (2) loop
         Table (Index, Domain_Index) :=
            Value (Domain_Index - Table'First + Value'First);
      end loop;
      Block.Flags (Index) := Defined;
   end Put;

   procedure Put
             (  Block : in out Set_Column_Block;
                Index : Subindex;
                Value : Positive
             )  is
      Table : Set_Array renames Block.Data;
   begin
      if Value > Block.Cardinality then
         raise Constraint_Error;
      end if;
      if Block.Flags (Index) = Defined then
         Table (Index, Value) := Confidence'Last;
      else
         for Domain_Index in 1..Block.Cardinality loop
            Table (Index, Domain_Index) := Confidence'First;
         end loop;
         Table (Index, Value) := Confidence'Last;
      end if;
   end Put;

   procedure Set_Undefined
             (  Block  : in out Set_Column_Block;
                Index  : Subindex;
                Status : Undefined_Status
             )  is
   begin
      Block.Flags (Index) := Status;
   end Set_Undefined;

end Fuzzy.Lecture.Block.General;
