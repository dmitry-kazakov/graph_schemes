--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Discrete                      Luebeck            --
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

with Ada.IO_Exceptions;          use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;           use Fuzzy.Basic_Edit;
with Object;                     use Object;
with Strings_Edit;               use Strings_Edit;
with Strings_Edit.Quoted;        use Strings_Edit.Quoted;
with Strings_Edit.UTF8.Maps;     use Strings_Edit.UTF8.Maps;
with Name_Tables;                use Name_Tables;

package body Fuzzy.Feature.Discrete is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Map     : in out Domain_Description
             );

   function Create (Name : String; Domain : Domain_Description'Class)
      return Feature_Handle is
   begin
      if Get_Cardinality (Domain) < 1 then
         raise Constraint_Error;
      end if;
      declare
         Map_Ptr : constant Domain_Description_Ptr :=
                      new Domain_Description;
         Result  : Feature_Object_Ptr;
      begin
         Increment_Count (Map_Ptr.all);
         Copy (Domain, Map_Ptr.all);
         Result :=
            new Discrete_Feature_Object (Get_Cardinality (Domain));
         declare
            Feature : Discrete_Feature_Object renames
                         Discrete_Feature_Object (Result.all);
         begin
            Feature.Self   := Result;
            Feature.Domain := Map_Ptr;
            Feature.Name   := new String'(Name);
         end;
         return Ref (Result);
      exception
         when others =>
            declare
               Ptr : Entity_Ptr := Entity_Ptr (Map_Ptr);
            begin
               Release (Ptr);
               raise;
            end;
      end;
   end Create;

   function Create (Name : String; Domain : String)
      return Feature_Handle is
      Map_Ptr : constant Domain_Description_Ptr :=
                   new Domain_Description;
      Result  : Feature_Object_Ptr;
      Pointer : Integer := Domain'First;
      Handle  : Feature_Handle;
   begin
      Increment_Count (Map_Ptr.all);
      Get (Domain, Pointer, Name_Tables.Blanks);
      Get (Domain, Pointer, Map_Ptr.all);
      Get (Domain, Pointer, Name_Tables.Blanks);
      if Pointer <= Domain'Last then
         raise Data_Error;
      end if;
      Result :=
         new Discrete_Feature_Object (Get_Cardinality (Map_Ptr.all));
      Ref (Handle, Result);
      declare
         Feature : Discrete_Feature_Object renames
                      Discrete_Feature_Object (Result.all);
      begin
         Feature.Self   := Result;
         Feature.Name   := new String'(Name);
         Feature.Domain := Map_Ptr;
      end;
      return Handle;
   exception
      when others =>
         declare
            Ptr : Entity_Ptr := Entity_Ptr (Map_Ptr);
         begin
            Release (Ptr);
            raise;
         end;
   end Create;

   procedure Finalize (Feature : in out Discrete_Feature_Object) is
      Ptr : Entity_Ptr := Entity_Ptr (Feature.Domain);
   begin
      Close (Feature);
      Release (Ptr);
      Finalize (Independent_Feature_Object (Feature));
   end Finalize;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Map     : in out Domain_Description
             )  is
      Index  : aliased Integer := Pointer;
      Got_It : Boolean         := False;
   begin
      loop
         if Got_It then
            Get_Delimiter (Source, Index, Got_It, Comma);
            exit when not Got_It;
         else
            Got_It := True;
         end if;
         begin
            Add
            (  Map,
               Get_Name (Source, Index'Access),
               Get_Cardinality (Map) + 1
            );
         exception
            when Constraint_Error =>
               raise Data_Error;
         end;
      end loop;
      if 0 = Get_Cardinality (Map) then
         raise End_Error;
      end if;
      Pointer := Index;
   end Get;

   function Get_Class (Feature : Discrete_Feature_Object)
      return String is
   begin
      return Discrete_Class;
   end Get_Class;

   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Discrete_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             )  is
   begin
      Get
      (  Source,
         Pointer,
         Feature.Domain.all,
         From,
         To
      );
      Exclusive := True;
   end Get_Range;

   function Is_Discrete (Feature : Feature_Object'Class)
      return Boolean is
   begin
      return Feature in Discrete_Feature_Object'Class;
   end Is_Discrete;

   function Is_Discrete (Feature : Feature_Handle) return Boolean is
   begin
      return
      (  Feature.Is_Valid
      and then
         Ptr (Feature).all in Discrete_Feature_Object'Class
      );
   end Is_Discrete;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object;
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
         Feature.Domain.all,
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
      Map_Ptr  : constant Domain_Description_Ptr :=
                    new Domain_Description;
      Position : aliased Integer := Pointer;
      Name     : constant String :=
                    Get_Quoted (Source, Position'Access);
   begin
      Increment_Count (Map_Ptr.all);
      Get (Source, Position);
      if Position > Source'Last or else Source (Position) /= ':' then
         raise Data_Error;
      end if;
      Position := Position + 1;
      Get (Source, Position);
      Get (Source, Position, Map_Ptr.all);
      Feature :=
         new Discrete_Feature_Object (Get_Cardinality (Map_Ptr.all));
      declare
         This : Discrete_Feature_Object renames
                   Discrete_Feature_Object (Feature.all);
      begin
         This.Self   := To_Feature_Object_Ptr (Feature);
         This.Domain := Map_Ptr;
         This.Name   := new String'(Name);
      end;
      Pointer := Position;
   exception
      when others =>
         declare
            Ptr : Entity_Ptr := Entity_Ptr (Map_Ptr);
         begin
            Release (Ptr);
            raise;
         end;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Discrete_Feature_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put_Quoted (Destination, Position, Get_Name (Feature));
      Put (Destination, Position, ": ");
      for Element in 1..Feature.Cardinality loop
         if Element /= 1 then
            Put (Destination, Position, ", ");
         end if;
         Put
         (  Destination,
            Position,
            Get_Name (Feature.Domain.all, Element)
         );
      end loop;
      Pointer := Position;
   end Store;

begin
   Register_Class (Discrete_Class, Restore'Access);
end Fuzzy.Feature.Discrete;
