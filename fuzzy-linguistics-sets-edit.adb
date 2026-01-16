--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Linguistics.Sets.Edit                 Luebeck            --
--  Implementation                                 Summer, 2003       --
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

with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Name_Tables;             use Name_Tables;
with Fuzzy.Basic_Edit;        use Fuzzy.Basic_Edit;
with Strings_Edit.Fields;     use Strings_Edit.Fields;
with Strings_Edit.UTF8.Maps;  use Strings_Edit.UTF8.Maps;

with Fuzzy.Abstract_Edit.Intuitionistic;
use  Fuzzy.Abstract_Edit.Intuitionistic;

package body Fuzzy.Linguistics.Sets.Edit is
   use Linguistic_Edit;

   Size_Inc : constant := 80;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Linguistic_Set;
                Base    : NumberBase := 10
             )  is
   --
   -- Get_Item -- Get one domain value
   --
   --    Pointer  - The current string position
   --    Got_Item - Completion flag (input / output)
   --
      procedure Get_Item
                (  Pointer  : in out Integer;
                   Got_Item : in out Boolean
                );
      Result : Linguistic_Set;
      Item   : Variable;

      procedure Get_Item
                (  Pointer  : in out Integer;
                   Got_Item : in out Boolean
                )  is
         Index : aliased Integer := Pointer;
      begin
         if Got_Item then
            Get_Delimiter (Source, Index, Got_Item, Comma);
            if not Got_Item then
               return;
            end if;
         end if;
         declare
            Name : String renames Get_Name (Source, Index'Access);
         begin
            Get (Source, Index, Name_Tables.Blanks);
            if Index > Source'Last or else Source (Index) /= '(' then
               Got_Item := False;
               return;
            end if;
            Index := Index + 1;
            Get (Source, Index, Name_Tables.Blanks);
            Get (Source, Index, Item, Base);
            Add (Result, Name, Item);
            Get (Source, Index, Name_Tables.Blanks);
            if Index > Source'Last or else Source (Index) /= ')' then
               raise Data_Error;
            end if;
            Pointer  := Index + 1;
            Got_Item := True;
         exception
            when End_Error =>
               raise Data_Error;
         end;
      exception
         when Constraint_Error =>
            raise Data_Error;
         when End_Error =>
            Got_Item := False;
      end Get_Item;

      Got_Item : Boolean := False;
      Index    : Integer := Pointer;
   begin
      Get_Item (Index, Got_Item);
      if not Got_Item then
         raise End_Error;
      end if;
      while Got_Item loop
         Get_Item (Index, Got_Item);
      end loop;
      Value   := Result;
      Pointer := Index;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Uncertain;
                Base    : NumberBase    := 10
             )  is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      begin
         Get (Source, Pointer, Ptr (Set).Domain, Value, Default);
      exception
         when End_Error =>
            declare
               Distribution : Variable;
            begin
               Get (Source, Pointer, Distribution, Base);
               Value := To_Set (Set, Distribution);
            end;
      end;
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Set     : Linguistic_Set;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Uncertain;
                Base    : NumberBase    := 10
             )  is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      begin
         Get (Source, Pointer, Ptr (Set).Domain, Value, Default);
      exception
         when End_Error =>
            declare
               Distribution : Variable;
            begin
               Get (Source, Pointer, Distribution, Base);
               Value := Classify (Set, Distribution);
            end;
      end;
   end Get;

   function Image
            (  Value    : Linguistic_Set;
               Base     : NumberBase := 10;
               RelSmall : Positive   := MaxSmall;
               AbsSmall : Integer    := -MaxSmall
            )  return String is
      Size : Natural := Size_Inc;
   begin
      begin
         loop
            declare
               Text    : String (1..Size);
               Pointer : Positive := 1;
            begin
               Put (Text, Pointer, Value, Base, RelSmall, AbsSmall);
               return Text (Text'First..Pointer - 1);
            exception
               when Layout_Error =>
                  null;
            end;
            Size := Size + Size_Inc;
         end loop;
      end;
   end Image;

   function Image
            (  Set     : Linguistic_Set;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Uncertain
            )  return String is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      return Image (Ptr (Set).Domain, Value, Default);
   end Image;

   function Image
            (  Set     : Linguistic_Set;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Uncertain
            )  return String is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      return Image (Ptr (Set).Domain, Value, Default);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Linguistic_Set;
                Base        : NumberBase := 10;
                RelSmall    : Positive   := MaxSmall;
                AbsSmall    : Integer    := -MaxSmall;
                Field       : Natural    := 0;
                Justify     : Alignment  := Left;
                Fill        : Character  := ' '
             )  is
      Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      for Item in 1..Get_Cardinality (Value) loop
         if Item /= 1 then
            Put (Text, Index, ", ");
         end if;
         Put (Text, Index, Get_Name (Value, Item));
         Put (Text, Index, '(');
         Put (Text, Index, Get (Value, Item), Base, RelSmall, AbsSmall);
         Put (Text, Index, ')');
      end loop;
      Adjust_Output_Field
      (  Destination,
         Pointer,
         Index,
         Out_Field,
         Field,
         Justify,
         Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Set         : Linguistic_Set;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Uncertain;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             )  is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Ptr (Set).Domain,
         Value       => Value,
         Default     => Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Set         : Linguistic_Set;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Uncertain;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             )  is
   begin
      if Value.Cardinality /= Get_Cardinality (Set) then
         raise Constraint_Error;
      end if;
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Ptr (Set).Domain,
         Value       => Value,
         Default     => Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Value
            (  Source : String;
               Base   : NumberBase := 10
            )  return Linguistic_Set is
      Pointer : Integer := Source'First;
      Result  : Linguistic_Set;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Result, Base);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value
            (  Source  : String;
               Set     : Linguistic_Set;
               Default : Fuzzy_Boolean := Uncertain;
               Base    : NumberBase    := 10
            )  return Fuzzy.Intuitionistic.Set is
      Pointer : Integer := Source'First;
      Result  : Fuzzy.Intuitionistic.Set (Get_Cardinality (Set));
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Set, Result, Default, Base);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value
            (  Source  : String;
               Set     : Linguistic_Set;
               Default : Fuzzy_Boolean := Uncertain;
               Base    : NumberBase    := 10
            )  return Fuzzy.Intuitionistic.Classification is
      Pointer : Integer := Source'First;
      Result  : Fuzzy.Intuitionistic.Classification
                   (Get_Cardinality (Set));
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Set, Result, Default, Base);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

end Fuzzy.Linguistics.Sets.Edit;
