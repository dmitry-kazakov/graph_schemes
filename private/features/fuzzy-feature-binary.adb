--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Binary                        Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
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

with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;       use Fuzzy.Basic_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;
with System;                 use type System.Address;

package body Fuzzy.Feature.Binary is

   procedure Create_Constraint
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                Allowed : Boolean := True
             )  is
   begin
      Create_Constraint (Ptr (Feature.Source).all, Context, Allowed);
   end Create_Constraint;

   function Create_Data (Feature : Binary_Feature_Object)
      return Feature_Data_Ptr is
   begin
      return null;
   end Create_Data;

   function Make_Suffix (Order : Positive) return String is
      Result : String (1..16);
      Value  : Natural := Order;
   begin
      for Index in reverse Result'range loop
         Result (Index) :=
            Character'Val (Character'Pos ('0') + Value rem 10);
         Value := Value / 10;
         if Value = 0 then
            return Result (Index..Result'Last);
         end if;
      end loop;
      return Result;
   end Make_Suffix;

   function Log2 (Value : Positive) return Positive is
      Count  : Integer  := Value - 1;
      Result : Positive := 1;
   begin
      loop
         Count := Count / 2;
         exit when Count = 0;
         Result := Result + 1;
      end loop;
      return Result;
   end Log2;

   function Constructor
            (  Source : Feature_Handle;
               Bit    : Mask;
               Name   : String
            )  return Feature_Object_Ptr is
      Result  : constant Feature_Object_Ptr :=
                   new Binary_Feature_Object (2);
      Feature : Binary_Feature_Object renames
                   Binary_Feature_Object (Result.all);
   begin
      Feature.Self   := Result;
      Feature.Name   := new String'(Name);
      Feature.Source := Source;
      Feature.Bit    := Bit;
      return Result;
   end Constructor;

   function Create (Feature : Feature_Handle) return Bounded_Array is
      Name   : String renames Get_Name (Feature);
      Source : Feature_Object'Class renames Ptr (Feature).all;
      Result : Bounded_Array (1, Log2 (Source.Cardinality));
      Bit    : Mask := Get_Bit (Source);
      Binary : Feature_Object_Ptr;
   begin
      for Order in Result.First..Result.Last loop
         Binary :=
            Constructor
            (  Feature,
               Bit,
               Name & "." & Make_Suffix (Order)
            );
         Bit := Bit / 2;
         Put (Result, Order, Binary);
      end loop;
      return Result;
   end Create;

   function Create
            (  Name    : String;
               Feature : Feature_Handle
            )  return Feature_Handle is
      Source : Feature_Object'Class renames Ptr (Feature).all;
      Result : Feature_Object_Ptr;
   begin
      if Ptr (Feature).all not in Binary_Feature_Object'Class then
         Result :=
            Constructor (Feature, Get_Bit (Source), Name);
      else
         declare
            Binary : Binary_Feature_Object renames
                        Binary_Feature_Object (Source);
         begin
            Result := Constructor (Binary.Source, Binary.Bit / 2, Name);
         end;
      end if;
      return Ref (Result);
   end Create;

   function Create
            (  Name         : String;
               Feature      : Feature_Handle;
               Bit_Position : Natural
            )  return Feature_Handle is
      Binary : Feature_Object_Ptr;
   begin
      if Bit_Position >= Log2 (Get_Cardinality (Feature)) then
         raise End_Error;
      end if;
      Binary :=
         Constructor
         (  Feature,
            Get_Bit (Ptr (Feature).all) / 2 ** Bit_Position,
            Name
         );
      return Ref (Binary);
   end Create;

   function Get_Bit (Source : Feature_Object'Class) return Mask is
      Domain_Range : Natural := Source.Cardinality - 1;
      Bit          : Mask    := 1;
   begin
      loop
         Domain_Range := Domain_Range / 2;
         exit when Domain_Range = 0;
         Bit := Bit * 2;
      end loop;
      return Bit;
   end Get_Bit;

   function Get_Bit_Position (Feature : Feature_Handle)
      return Natural is
   begin
      return Get_Bit_Position (Ptr (Feature).all);
   end Get_Bit_Position;

   function Get_Bit_Position (Feature : Feature_Object'Class)
      return Natural is
      Result : Natural := 0;
      Object : Binary_Feature_Object'Class renames
                  Binary_Feature_Object'Class (Feature);
      Last   : constant Mask := Get_Bit (Ptr (Object.Source).all);
      Bit    : Mask := Object.Bit;
   begin
      while Bit < Last loop
         Result := Result + 1;
         Bit    := Bit * 2;
      end loop;
      return Result;
   end Get_Bit_Position;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Binary_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
      Lower, Upper : Integer;
      Got_It       : Boolean;
   begin
      Get (Source, Pointer, Lower, First => 0, Last => 1);
      Get_Delimiter (Source, Pointer, Got_It, Ellipsis);
      if Got_It then
         begin
            Get (Source, Pointer, Upper, First => 0, Last => 1);
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         Upper := Lower;
      end if;
      From := Upper + 1;
      To   := Lower + 1;
      Exclusive := True;
   end Get_Range;

   function Get
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Set is
      Value  : Set renames
                  Get (Ptr (Feature.Source).all, Context, Image);
      Binary : Mask := 0;
      Max_0, Max_1 : Confidence := Confidence'First;
   begin
      for Index in Value'Range loop
         if 0 = (Binary and Feature.Bit) then
            Max_0 := Max_0 or Value (Index);
         else
            Max_1 := Max_1 or Value (Index);
         end if;
         Binary := Binary + 1;
      end loop;
      return (0 => Max_0, 1 => Max_1);
   end Get;

   function Get
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence is
      Source : Feature_Object'Class renames Ptr (Feature.Source).all;
      Reset  : constant Boolean := Value = 1;
      Result : Confidence := Confidence'First;
      Binary : Mask       := 0;
   begin
      for Index in 1..Source.Cardinality loop
         if Reset = (0 = (Binary and Feature.Bit)) then
            Result := Result or Get (Source, Context, Image, Index);
         end if;
         Binary := Binary + 1;
      end loop;
      return Result;
   end Get;

   function Get_Class (Feature : Binary_Feature_Object)
      return String is
   begin
      return Binary_Class;
   end Get_Class;

   function Get_Constraint
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset is
      Source : Domain_Subset renames
                  Get_Constraint (Ptr (Feature.Source).all, Context);
      Result : Domain_Subset (1..2) := (others => False);
      Binary : Mask := 0;
   begin
      for Index in Source'Range loop
         if Source (Index) then
            if 0 = (Binary and Feature.Bit) then
               Result (1) := True;
               exit when Result (2);
            else
               Result (2) := True;
               exit when Result (1);
            end if;
         end if;
         Binary := Binary + 1;
      end loop;
      return Result;
   end Get_Constraint;

   procedure Get_Referents
             (  Feature : Binary_Feature_Object;
                List    : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Feature.Source)), False);
   end Get_Referents;

   function Get_Source (Feature : Feature_Object'Class)
      return Feature_Handle is
   begin
      return Binary_Feature_Object'Class (Feature).Source;
   end Get_Source;

   function Get_Source (Feature : Feature_Handle)
      return Feature_Handle is
   begin
      return Binary_Feature_Object'Class (Ptr (Feature).all).Source;
   end Get_Source;

   function Is_Binary (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Binary_Feature_Object'Class;
   end Is_Binary;

   function Is_Binary (Feature : Feature_Handle) return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
         This /= null and then This.all in Binary_Feature_Object'Class;
   end Is_Binary;

   function Is_Computed
            (  Feature : Binary_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is
   begin
      if Source in Binary_Feature_Object'Class then
         declare
            Other : Binary_Feature_Object'Class renames
                       Binary_Feature_Object'Class (Source);
         begin
            return
            (  Other.Source = Feature.Source
            and then
               Feature.Bit <= Other.Bit
            );
         end;
      else
         return Is_Computed (Ptr (Feature.Source).all, Source);
      end if;
   end Is_Computed;

   function Is_Defined
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Defined (Ptr (Feature.Source).all, Context, Image);
   end Is_Defined;

   function Is_Known
            (  Feature : Binary_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Known (Ptr (Feature.Source).all, Context, Image);
   end Is_Known;

   procedure Set_Constraint
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                Value   : Positive;
                Allowed : Boolean := False
             )  is
      Source  : Feature_Object'Class renames Ptr (Feature.Source).all;
      Changed : constant Boolean := Value = 1;
      Binary  : Mask := 0;
   begin
      for Index in 1..Source.Cardinality loop
         if Changed = (0 = (Binary and Feature.Bit)) then
            Set_Constraint (Source, Context, Index, Allowed);
         end if;
         Binary := Binary + 1;
      end loop;
   end Set_Constraint;

   procedure Set_Constraint_Range
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Natural;
                Allowed : Boolean := False
             )  is
   begin
      for Index in From..To loop
         Set_Constraint (Feature, Context, Index, Allowed);
      end loop;
   end Set_Constraint_Range;

   procedure Set_Range
             (  Feature : Binary_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             )  is
      Source     : Feature_Object'Class renames
                      Ptr (Feature.Source).all;
      Constraint : constant Value_Constraint_Ptr :=
                      Get_Data
                      (  Context'Unchecked_Access,
                         Source
                      ) .Constraint;
      Low, High  : Positive;
   begin
      if Constraint = null then
         Low  := 1;
         High := Source.Cardinality;
      else
         Low  := Constraint.From;
         High := Constraint.To;
      end if;
      if From = 2 then
         --
         -- Move the lower boundary up
         --
         Low :=
            Positive
            (  (Mask (Low) or Feature.Bit)
            and
               not (Feature.Bit - 1)
            );
         Set_Range (Source, Context, Low, High);
      elsif To = 1 then
         --
         -- Move the upper boundary down
         --
         High :=
            Positive
            (  (Mask (High) and Feature.Bit)
            or (Feature.Bit - 1)
            );
         Set_Range (Source, Context, Low, High);
      end if;
   end Set_Range;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Binary_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if From > To or else To > 2 then
         raise Constraint_Error;
      end if;
      if From = To then
         if From = 1 then
            Put
            (  Destination,
               Pointer,
               "0",
               Field,
               Justify,
               Fill
            );
         else
            Put
            (  Destination,
               Pointer,
               "1",
               Field,
               Justify,
               Fill
            );
         end if;
      else
         Put
         (  Destination,
            Pointer,
            "0..1",
            Field,
            Justify,
            Fill
         );
      end if;
   end Put_Range;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             )  is
      From     : Feature_Handle;
      Bit      : Integer;
      Position : aliased Integer := Pointer;
      Name     : constant String :=
                    Get_Quoted (Source, Position'Access);
   begin
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      begin
         Get (Source, Position, Bit);
      exception
         when Constraint_Error =>
            raise Data_Error;
      end;
      From := Ref (To_Feature_Object_Ptr (Get (List, 1)));
      Feature := To_Deposit_Ptr (Constructor (From, Mask (Bit), Name));
      Pointer := Position;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Binary_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      Put (Destination, Position, Integer (Feature.Bit));
      Pointer := Position;
   end Store;

begin
   Register_Class (Binary_Class, Restore'Access);
end Fuzzy.Feature.Binary;
