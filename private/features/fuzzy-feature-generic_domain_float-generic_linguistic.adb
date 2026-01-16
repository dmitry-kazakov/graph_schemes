--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Domain_Float          Luebeck            --
--        Generic_Linguistic                       Winter, 2003       --
--  Implementation                                                    --
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

with Ada.Exceptions;             use Ada.Exceptions;
with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Fuzzy.Abstract_Edit.Named;  use Fuzzy.Abstract_Edit.Named;
with Name_Tables;                use Name_Tables;
with Strings_Edit;               use Strings_Edit;
with Strings_Edit.Quoted;        use Strings_Edit.Quoted;
with Units.Base;                 use Units.Base;

package body Fuzzy.Feature.Generic_Domain_Float.Generic_Linguistic is

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Feature_Object'Class,
             Feature_Object_Ptr
          );

   function Accumulate
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy.Set
            )  return Variable_Measure is
   begin
      return
      (  SI     => Feature.SI,
         Gain   => Accumulate (Feature.Domain, Value) * Feature.Gain,
         Offset => Feature.Offset
      );
   end Accumulate;

   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Classify
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end Classify;

   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return Classify (Feature.Domain, Value.Gain);
      else
         return
            Classify
            (  Feature.Domain,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end Classify;

   function Classify
            (  Feature : Linguistic_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return Classify (Feature.Domain, Value.Gain);
      else
         return
            Classify
            (  Feature.Domain,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end Classify;

   procedure Check_Names (Domain : Linguistic_Set) is
   begin
      for Index in 1..Get_Cardinality (Domain) loop
         begin
            Check_Name (Get_Name (Domain, Index));
         exception
            when Constraint_Error =>
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Illegal domain value name '"
                  &  Get_Name (Domain, Index)
                  & "'"
               )  );
         end;
      end loop;
   end Check_Names;

   function Create
            (  Name      : String;
               Domain    : Linguistic_Set;
               Dimension : String;
               Default   : Measure;
               Mode      : Code_Set := UTF8_Set
            )  return Feature_Object_Ptr is
      Cardinality : constant Natural := Get_Cardinality (Domain);
      Scale       : Measure          := Default;
   begin
      if Cardinality < 1 then
         raise Constraint_Error;
      end if;
      if Get_Cardinality (Domain) = 0 then
         raise End_Error;
      end if;
      if Dimension'Length > 0 then
         begin
            Scale := Value (Dimension, Mode);
         exception
            when others =>
               raise Unit_Error;
         end;
      end if;
      if Scale.Gain <= 0.0 then
         raise Unit_Error;
      end if;
      declare
         Result : Feature_Object_Ptr :=
                     new Linguistic_Feature_Object
                         (  Get_Cardinality (Domain),
                            Scale.SI,
                            Dimension'Length
                         );
         Object : Linguistic_Feature_Object renames
                     Linguistic_Feature_Object (Result.all);
      begin
         Object.Self := Result;
--         if Scale.Gain /= 1.0 then
--            for Index in 1..Get_Cardinality (Domain) loop
--               Add
--               (  Object.Domain.Gain,
--                  Get_Name (Domain, Index),
--                  Get (Domain, Index) * Scale.Gain
--               );
--            end loop;
--         else
--            Object.Domain.Gain := Domain;
--         end if;
         Object.Domain    := Domain;
         Object.Gain      := Scale.Gain;
         Object.Offset    := Scale.Offset;
         Object.Name      := new String'(Name);
         Object.Dimension := Dimension;
         return Result;
      exception
         when others =>
            Free (Result);
            raise;
      end;
   end Create;

   function Create
            (  Name   : String;
               Domain : String;
               Scale  : Measure := Float_Measures.Np
            )  return Feature_Handle is
       Result : Feature_Object_Ptr;
    begin
       if Scale.Gain <= 0.0 then
          raise Unit_Error;
       end if;
       Result := Create (Name, Value (Domain), "", Scale);
       return Ref (Result);
   end Create;

   function Create
            (  Name   : String;
               Domain : String;
               Scale  : String;
               Mode   : Code_Set := UTF8_Set
            )  return Feature_Handle is
       Result : Feature_Object_Ptr;
    begin
       Result :=
          Create (Name, Value (Domain), Scale, Float_Measures.Np, Mode);
       return Ref (Result);
   end Create;

   function Create
            (  Name   : String;
               Domain : Variable_Sets.Linguistic_Set;
               Scale  : Measure := Float_Measures.Np
            )  return Feature_Handle is
      Result : Feature_Object_Ptr;
   begin
      Check_Names (Domain);
      Result := Create (Name, Domain, "", Scale);
      return Ref (Result);
   end Create;

   function Create
            (  Name   : String;
               Domain : Variable_Sets.Linguistic_Set;
               Scale  : String;
               Mode   : Code_Set := UTF8_Set
            )  return Feature_Handle is
      Result : Feature_Object_Ptr;
   begin
      Check_Names (Domain);
      Result := Create (Name, Domain, Scale, Float_Measures.Np, Mode);
      return Ref (Result);
   end Create;

   function Get_Class (Feature : Linguistic_Feature_Object)
      return String is
   begin
      return Linguistic_Class;
   end Get_Class;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Linguistic_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
   begin
      Get
      (  Source,
         Pointer,
         Get_Domain (Feature.Domain).all,
         From,
         To
      );
      Exclusive := True;
   end Get_Range;

   function Get_Scale (Feature : Linguistic_Feature_Object)
      return Measure is
   begin
      return (Feature.SI, Feature.Gain, Feature.Offset);
   end Get_Scale;

   function Get_Scale_Text
            (  Feature    : Linguistic_Feature_Object;
               Parameters : Output_Parameters'Class
            )  return String is
   begin
      if Parameters.Use_SI or else Feature.Unit_Length = 0 then
         if Feature.SI = Unitless then
            return "";
         else
            if Parameters.Quote_Units then
               return
               (  '['
               &  Image
                  (  (Feature.SI, 1.0, 0.0),
                     Parameters.Mode,
                     Parameters.Use_Derived
                  )
               &  ']'
               );
            else
               return
                  Image
                  (  (Feature.SI, 1.0, 0.0),
                     Parameters.Mode,
                     Parameters.Use_Derived
                  );
            end if;
         end if;
      else
         if Parameters.Quote_Units then
            return '[' & Feature.Dimension & ']';
         else
            return Feature.Dimension;
         end if;
      end if;
   end Get_Scale_Text;

   function Get_Unit (Feature : Linguistic_Feature_Object)
      return Unit is
   begin
      return Feature.SI;
   end Get_Unit;

   function Get_Variable
            (  Feature : Linguistic_Feature_Object;
               Value   : Positive
            )  return Variable_Measure is
   begin
      return
      (  SI     => Feature.SI,
         Gain   => Get (Feature.Domain, Value) * Feature.Gain,
         Offset => Feature.Offset
      );
   end Get_Variable;

   function Is_Domain_Linguistic (Feature : Linguistic_Feature_Object)
      return Boolean is
   begin
      return True;
   end Is_Domain_Linguistic;

   function Is_Linguistic (Feature : Feature_Handle)
      return Boolean is
   begin
      return
         Feature.Is_Valid and then Is_Linguistic (Ptr (Feature).all);
   end Is_Linguistic;

   function Is_Linguistic (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Linguistic_Feature_Object'Class;
   end Is_Linguistic;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Linguistic_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination,
         Pointer,
         Get_Domain (Feature.Domain).all,
         From,
         To,
         Field,
         Justify,
         Fill
      );
   end Put_Range;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      Position : aliased Integer := Pointer;
      Domain   : Linguistic_Set;
      Name     : constant String :=
                    Get_Quoted (Source, Position'Access);
   begin
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      Get (Source, Position, Domain);
      Get (Source, Position);
      if Position > Source'Last then
         raise Data_Error;
      end if;
      if Source (Position) = '"' then
         Feature :=
            To_Deposit_Ptr
            (  Create
               (  Name,
                  Domain,
                  Get_Quoted (Source, Position'Access),
                  Float_Measures.Np,
                  UTF8_Set
             )  );
      else
         declare
            Scale : Measure;
         begin
            Get (Source, Position, Scale, ASCII_Set);
            Feature :=
               To_Deposit_Ptr
               (  Create
                  (  Name,
                     Domain,
                     "",
                     Scale,
                     UTF8_Set
                )  );
         end;
      end if;
      Pointer := Position;
   exception
      when Constraint_Error | Unit_Error | End_Error =>
         raise Data_Error;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Linguistic_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Put (Destination, Position, Feature.Domain);
      Put (Destination, Position, " ");
      Put_Dimension (Destination, Position, Feature.Dimension, Feature);
      Pointer := Position;
   end Store;

   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Interval_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      return
         To_Set
         (  Feature.Domain,
            Get_Value_As
            (  Value,
               (Feature.SI, Feature.Gain, Feature.Offset)
         )  );
   end To_Set;

   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Fuzzy_Measure
            )  return  Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return To_Set (Feature.Domain, Value.Gain);
      else
         return
            To_Set
            (  Feature.Domain,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end To_Set;

   function To_Set
            (  Feature : Linguistic_Feature_Object;
               Value   : Variable_Measure
            )  return Fuzzy.Intuitionistic.Set is
   begin
      if Feature.Gain = 1.0 and then Feature.Offset = Value.Offset then
         if Feature.SI /= Value.SI then
            raise Unit_Error;
         end if;
         return To_Set (Feature.Domain, Value.Gain);
      else
         return
            To_Set
            (  Feature.Domain,
               Get_Value_As
               (  Value,
                  (Feature.SI, Feature.Gain, Feature.Offset)
            )  );
      end if;
   end To_Set;

begin
   if not Is_Registered (Linguistic_Class) then
      Register_Class (Linguistic_Class, Restore_Ptr);
   end if;
end Fuzzy.Feature.Generic_Domain_Float.Generic_Linguistic;
