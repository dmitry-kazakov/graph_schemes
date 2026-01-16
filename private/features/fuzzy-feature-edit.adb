--                                                                    --
--  package Fuzzy.Feature.Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2003       --
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

with Ada.IO_Exceptions;             use Ada.IO_Exceptions;
with Fuzzy.Feature.Domain_Floats;   use Fuzzy.Feature.Domain_Floats;
with Strings_Edit.UTF8.Maps;        use Strings_Edit.UTF8.Maps;
with Units;                         use Units;

with Fuzzy.Abstract_Edit.Intuitionistic;
with Name_Tables;

package body Fuzzy.Feature.Edit is
   use Float_Measures;
   use Fuzzy.Abstract_Edit.Intuitionistic;
   use Variable_Measures;
   use Variables;

   procedure Get_Variable
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Domain_Feature_Object'Class;
                Value      : out Variable_Measure;
                Parameters : Input_Parameters'Class
             );
   pragma Inline (Get_Variable);

   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : Input_Data;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             )  is
   begin
      Get_Range
      (  Source,
         Pointer,
         Data.Feature.all,
         Integer (From),
         Integer (To),
         Exclusive,
         Data.Parameters.all
      );
   end Get;

   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : Output_Data;
                From      : out Value_Index;
                To        : out Value_Index;
                Exclusive : out Boolean
             )  is
   begin
      raise Program_Error;
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Interval;
                Parameters : Input_Parameters'Class
             )  is
      Exclusive : Boolean;
   begin
      Get_Range
      (  Source,
         Pointer,
         Feature,
         Value.From,
         Value.To,
         Exclusive,
         Parameters
      );
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Set;
                Parameters : Input_Parameters'Class
             )  is
      Data : Input_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Get (Source, Pointer, Data, Value);
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Fuzzy.Intuitionistic.Classification;
                Parameters : Input_Parameters'Class
             )  is
      Data : Input_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Get (Source, Pointer, Data, Value, Parameters.Default);
   exception
      when End_Error =>
         if Feature in Domain_Feature_Object'Class then
            declare
               This     : Domain_Feature_Object'Class renames
                          Domain_Feature_Object'Class (Feature);
               Variable : Variable_Measure;
            begin
               Get_Variable
               (  Source,
                  Pointer,
                  This,
                  Variable,
                  Parameters
               );
               Value := Classify (This, Variable);
            end;
         else
            raise;
         end if;
   end Get;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Feature_Object'Class;
                Value      : out Fuzzy.Intuitionistic.Set;
                Parameters : Input_Parameters'Class
             )  is
      Data : Input_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Get (Source, Pointer, Data, Value, Parameters.Default);
   exception
      when End_Error =>
         if Feature in Domain_Feature_Object'Class then
            declare
               This : Domain_Feature_Object'Class renames
                         Domain_Feature_Object'Class (Feature);
               Variable : Variable_Measure;
            begin
               Get_Variable
               (  Source,
                  Pointer,
                  This,
                  Variable,
                  Parameters
               );
               Value := To_Set (This, Variable);
            end;
         else
            raise;
         end if;
   end Get;

   procedure Get_Max_Range
             (  Data    : Input_Data;
                From    : out Integer;
                To      : out Integer
             )  is
   begin
      From := 1;
      To   := Data.Feature.Cardinality;
   end Get_Max_Range;

   procedure Get_Max_Range
             (  Data    : Output_Data;
                From    : out Integer;
                To      : out Integer
             )  is
   begin
      From := 1;
      To   := Data.Feature.Cardinality;
   end Get_Max_Range;

   procedure Get_Variable
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Domain_Feature_Object'Class;
                Value      : out Variable_Measure;
                Parameters : Input_Parameters'Class
             )  is
      use Variable_Edit;
      Got_It : Boolean;
      Gain   : Variable;
      Scale  : Measure;
   begin
      Get (Source, Pointer, Gain, Parameters.Base);
      Get_Unit
      (  Source,
         Pointer,
         Scale,
         Got_It,
         Parameters
      );
      if Got_It then
         Value := Gain * Scale;
      else
         Scale := Get_Scale (Feature);
         if Parameters.Get_Units and then Scale /= Np then
            raise Unit_Error;
         else
            Value := Gain * Scale;
         end if;
      end if;
   end Get_Variable;

   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Interval;
               Parameters : Output_Parameters'Class
            )  return String is
      Size_Inc : constant := 80;
      Size     : Natural := Size_Inc;
      Data     : Output_Data;
   begin
      if Value.From > Value.To then
         raise Constraint_Error;
      end if;
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      loop
         declare
            Text    : String (1..Size);
            Pointer : Positive := 1;
         begin
            Put
            (  Text,
               Pointer,
               Data,
               Value_Index (Value.From),
               Value_Index (Value.To)
            );
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               null;
         end;
         Size := Size + Size_Inc;
      end loop;
   end Image;

   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Domain_Subset;
               Parameters : Output_Parameters'Class
            )  return String is
      Data : Set (Value'Range);
   begin
      for Index in Value'Range loop
         if Value (Index) then
            Data (Index) := Confidence'Last;
         else
            Data (Index) := Confidence'First;
         end if;
      end loop;
      return Image (Feature, Data, Parameters);
   end Image;

   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Set;
               Parameters : Output_Parameters'Class
            )  return String is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      return Image (Data, Value);
   end Image;

   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Fuzzy.Intuitionistic.Classification;
               Parameters : Output_Parameters'Class
            )  return String is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      return Image (Data, Value);
   end Image;

   function Image
            (  Feature    : Feature_Object'Class;
               Value      : Fuzzy.Intuitionistic.Set;
               Parameters : Output_Parameters'Class
            )  return String is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      return Image (Data, Value);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : Input_Data;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      raise Program_Error;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : Output_Data;
                From        : Value_Index;
                To          : Value_Index;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put_Range
      (  Destination,
         Pointer,
         Data.Feature.all,
         Integer (From),
         Integer (To),
         Data.Parameters.all,
         Field,
         Justify,
         Fill
      );
   end Put;

   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Interval is
      Data      : Input_Data;
      From, To  : Value_Index;
      Exclusive : Boolean;
      Pointer   : Integer := Source'First;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Data, From, To, Exclusive);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 or else From > To then
         raise Data_Error;
      end if;
      return (Integer (From), Integer (To));
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Set is
      Data : Input_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      return Value (Source, Data);
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Fuzzy.Intuitionistic.Classification is
      Result  : Fuzzy.Intuitionistic.Classification
                    (Feature.Cardinality);
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Feature, Result, Parameters);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value
            (  Source     : String;
               Feature    : Feature_Object'Class;
               Parameters : Input_Parameters'Class
            )  return Fuzzy.Intuitionistic.Set is
      Result  : Fuzzy.Intuitionistic.Set (Feature.Cardinality);
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Feature, Result, Parameters);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Interval;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if Value.From > Value.To then
         raise Constraint_Error;
      end if;
      Put_Range
      (  Destination => Destination,
         Pointer     => Pointer,
         Feature     => Feature,
         Parameters  => Parameters,
         From        => Value.From,
         To          => Value.To,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Domain_Subset;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : Set (Value'Range);
   begin
      for Index in Value'Range loop
         if Value (Index) then
            Data (Index) := Confidence'Last;
         else
            Data (Index) := Confidence'First;
         end if;
      end loop;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Feature     => Feature,
         Value       => Data,
         Parameters  => Parameters,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Set;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Value,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Fuzzy.Intuitionistic.Classification;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Value,
         Default     => Parameters.Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Value       : Fuzzy.Intuitionistic.Set;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : Output_Data;
   begin
      Data.Parameters := Parameters'Unchecked_Access;
      Data.Feature    := Feature'Unchecked_Access;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Value,
         Default     => Parameters.Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

end Fuzzy.Feature.Edit;
