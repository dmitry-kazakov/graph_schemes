--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Binary.                       Luebeck            --
--        Mutually_Independent                     Winter, 2005       --
--  Interface                                                         --
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
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;
with Strings_Edit.Quoted;    use Strings_Edit.Quoted;
with System;                 use type System.Address;

package body Fuzzy.Feature.Binary.Mutually_Independent is

   function Constructor
            (  Source : Feature_Handle;
               Bit    : Mask;
               Name   : String
            )  return Feature_Object_Ptr is
      Result  : constant Feature_Object_Ptr :=
                   new Bit_Feature_Object (2);
      Feature : Bit_Feature_Object renames
                   Bit_Feature_Object (Result.all);
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
            Constructor (Feature, Bit, Name & "." & Image (Order));
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
         Result := Constructor (Feature, Get_Bit (Source), Name);
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

   function Get_Constraint
            (  Feature : Bit_Feature_Object;
               Context : not null access Context_Object'Class
            )  return Domain_Subset is
   begin
      return Get_Constraint (Feature_Object (Feature), Context);
   end Get_Constraint;

   function Get_Class (Feature : Bit_Feature_Object)
      return String is
   begin
      return Independent_Binary_Class;
   end Get_Class;

   procedure Get_Referents
             (  Feature : Bit_Feature_Object;
                List    : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Feature.Source)), False);
   end Get_Referents;

   function Get_Source (Feature : Feature_Object'Class)
      return Feature_Handle is
   begin
      return Bit_Feature_Object'Class (Feature).Source;
   end Get_Source;

   function Get_Source (Feature : Feature_Handle)
      return Feature_Handle is
   begin
      return Bit_Feature_Object'Class (Ptr (Feature).all).Source;
   end Get_Source;

   function Is_Bit (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Bit_Feature_Object'Class;
   end Is_Bit;

   function Is_Bit (Feature : Feature_Handle) return Boolean is
      This : constant Feature_Object_Ptr := Ptr (Feature);
   begin
      return
         This /= null and then This.all in Bit_Feature_Object'Class;
   end Is_Bit;

   function Is_Computed
            (  Feature : Bit_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is
   begin
      return Is_Computed (Ptr (Feature.Source).all, Source);
   end Is_Computed;

   procedure Set_Range
             (  Feature : Bit_Feature_Object;
                Context : in out Context_Object'Class;
                From    : Positive;
                To      : Positive
             )  is
   begin
      if From = 2 then
         Set_Constraint (Feature, Context, 2);
      elsif To = 1 then
         Set_Constraint (Feature, Context, 1);
      end if;
   end Set_Range;

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

begin
   Register_Class (Independent_Binary_Class, Restore'Access);
end Fuzzy.Feature.Binary.Mutually_Independent;
