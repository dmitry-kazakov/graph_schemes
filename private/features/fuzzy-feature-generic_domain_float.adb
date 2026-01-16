--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--  Implementation                                 Autumn, 2005       --
--                                                                    --
--                                Last revision :  22:14 29 Jan 2012  --
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

with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Strings_Edit.Quoted;     use Strings_Edit.Quoted;
with Strings_Edit.UTF8;       use Strings_Edit.UTF8;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Name_Tables;

package body Fuzzy.Feature.Generic_Domain_Float is

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Value      : out Scaled;
                Parameters : Input_Parameters'Class
             )  is
      Index : Integer := Pointer;
   begin
      if Parameters.Quote_Units then
         --
         -- Units are quoted thus the value  and  the  unit  are  always
         -- separated as in 23.4 [km/h]
         --
         declare
            Number   : Domain_Float'Base;
            Scale    : Measure;
            Has_Unit : Boolean;
         begin
            Get (Source, Index, Number);
            Get_Unit (Source, Index, Scale, Has_Unit, Parameters);
            if Has_Unit then
               Value :=
                  (Float_Measures.Canonic, Number, Scale);
            else
               Value :=
                  (Float_Measures.Scalar, Number, Float_Measures.Np);
            end if;
         end;
      else
         --
         -- Units   and   value  are  intermixed.  We  try  get  both  a
         -- dimensioned number and a plain one. If they stop at the same
         -- position, then it is a dimensionless value.
         --
         Get (Source, Index, Value, Parameters.Mode);
      end if;
      Pointer := Index;
   end Get;

   function Get_Scaled
            (  Source : String;
               Scale  : Measure
            )  return Domain_Float is
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      declare
         Result : Domain_Float;
         Index  : Integer := Pointer;
      begin
         Get (Source, Index, Result);
         Get (Source, Index, Name_Tables.Blanks);
         if Index > Source'Last then
            return Result;
         end if;
      exception
         when End_Error | Data_Error | Constraint_Error =>
             null;
      end;
      declare
         Result : Measure;
      begin
         Measure_UTF8_Edit.Get (Source, Pointer, Result);
         Get (Source, Pointer, Name_Tables.Blanks);
         if Pointer <= Source'Last then
            raise Data_Error;
         end if;
         if Scale.SI /= Result.SI then
            raise Use_Error;
         end if;
         return Get_Value_As (Result, Scale);
      end;
   end Get_Scaled;

   procedure Get_Unit
             (  Source     : String;
                Pointer    : in out Integer;
                Value      : in out Measure;
                Got_It     : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
      Index : Integer := Pointer;
   begin
      Got_It := False;
      Get (Source, Index, Name_Tables.Blanks);
      if Parameters.Quote_Units then
         if Index <= Source'Last and then Source (Index) = '[' then
            Index := Index + 1;
            Get (Source, Index, Name_Tables.Blanks);
            begin
               Get (Source, Index, Value, Parameters.Mode);
            exception
               when Constraint_Error | Data_Error | End_Error =>
                  raise Unit_Error;
            end;
            Get (Source, Index, Name_Tables.Blanks);
            if Index < Source'Last and then Source (Index) = ']' then
               Pointer := Index + 1;
               Got_It  := True;
            else
               raise Data_Error;
            end if;
         end if;
      else
         begin
            Get (Source, Index, Value, Parameters.Mode);
            Pointer := Index;
            Got_It  := True;
         exception
            when End_Error =>
               null;
            when Constraint_Error | Data_Error =>
               raise Unit_Error;
         end;
      end if;
      if Got_It and then Value.Gain <= 0.0 then
         raise Unit_Error;
      end if;
   end Get_Unit;

   procedure Put_Dimension
             (  Destination : in out String;
                Pointer     : in out Integer;
                Scale       : String;
                Feature     : Domain_Feature_Object'Class
             )  is
   begin
      if Scale'Length > 0 then
         declare
            Dimension : Measure;
         begin
            Dimension := Value (Scale, UTF8_Set);
            Put_Quoted (Destination, Pointer, Scale);
            return;
         exception
            when others =>
               null;
         end;
      end if;
      Put (Destination, Pointer, Get_Scale (Feature), ASCII_Set);
   end Put_Dimension;

   function To_Domain_Feature_Ptr is
      new Ada.Unchecked_Conversion
          (  Feature_Object_Ptr,
             Domain_Feature_Object_Ptr
          );

   function To_Domain_Feature_Object_Ptr (Ptr : Feature_Object_Ptr)
      return Domain_Feature_Object_Ptr is
   begin
      if Ptr.all in Domain_Feature_Object'Class then
         return To_Domain_Feature_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Domain_Feature_Object_Ptr;

end Fuzzy.Feature.Generic_Domain_Float;
