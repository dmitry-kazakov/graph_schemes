--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--      Fuzzy.Generic_Edit.Intuitionistic          Luebeck            --
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

with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Confidence_Factors.Edit;  use Confidence_Factors.Edit;
with Fuzzy.Basic_Edit;         use Fuzzy.Basic_Edit;
with Strings_Edit.Fields;      use Strings_Edit.Fields;
with Strings_Edit.UTF8.Maps;   use Strings_Edit.UTF8.Maps;

with Fuzzy.Intuitionistic.Basic_Edit;
use  Fuzzy.Intuitionistic.Basic_Edit;

with Name_Tables;

package body Fuzzy.Generic_Edit.Intuitionistic is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      subtype Domain is Integer range 1..Value.Cardinality;
      type Map is array (Domain) of Boolean;
   --
   -- Get_Item -- Get range or one domain value
   --
   --    Pointer  - The current string position
   --    Got_Item - Completion flag (input / output)
   --
      procedure Get_Item
                (  Pointer  : in out Integer;
                   Got_Item : in out Boolean
                );
      Result : Fuzzy.Intuitionistic.Set :=
               (  Cardinality => Value.Cardinality,
                  Possibility => (others => Confidence'First),
                  Necessity   => (others => Confidence'First)
               );
      Given  : Map := (others => False);

      procedure Get_Item
                (  Pointer  : in out Integer;
                   Got_Item : in out Boolean
                )  is
         Index     : Integer := Pointer;
         From      : Domain;
         To        : Domain;
         Level     : Fuzzy_Boolean;
         Exclusive : Boolean;
      begin
         if Got_Item then
            Get_Delimiter (Source, Index, Got_Item, Comma);
            if not Got_Item then
               return;
            end if;
         end if;
         Get (Source, Index, Data, From, To, Exclusive);
         if From > To then
            raise Data_Error;
         end if;
         Get_Weight (Source, Index, Level, Default);
         for Point in From..To loop
            if Given (Point) then
               if Exclusive then
                  if (  Result.Possibility (Point) /= Level.Possibility
                     or else
                        Result.Necessity (Point) /= Level.Necessity
                     )
                  then
                     raise Data_Error;
                  end if;
               else
                  Result.Possibility (Point) :=
                     Result.Possibility (Point) or Level.Possibility;
                  Result.Necessity (Point) :=
                     Result.Necessity (Point) and Level.Necessity;
               end if;
            else
               Given (Point) := True;
               Result.Possibility (Point) := Level.Possibility;
               Result.Necessity   (Point) := Level.Necessity;
            end if;
         end loop;
         Got_Item := True;
         Pointer  := Index;
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
                Data    : User_Data;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Certain_True
             )  is
   begin
      Get
      (  Source  => Source,
         Pointer => Pointer,
         Data    => Data,
         Value   => Fuzzy.Intuitionistic.Set (Value),
         Default => Default
      );
   end Get;

   function Value
            (  Source      : String;
               Data        : User_Data;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Set is
      Result  : Fuzzy.Intuitionistic.Set (Cardinality);
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Data, Result, Default);
      Get (Source, Pointer, Name_Tables.Blanks );
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   function Value
            (  Source      : String;
               Data        : User_Data;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Classification is
   begin
      return
         Fuzzy.Intuitionistic.Classification
         (  Fuzzy.Intuitionistic.Set'
            (  Value
               (  Source      => Source,
                  Data        => Data,
                  Cardinality => Cardinality,
                  Default     => Default
         )  )  );
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             )  is
   begin
      declare
         Out_Field : constant Natural :=
                        Get_Output_Field (Destination, Pointer, Field);
         subtype Output is String (Pointer..Pointer + Out_Field - 1);
         Text  : Output renames
                    Destination (Pointer..Pointer + Out_Field - 1);
         This  : Fuzzy_Boolean;
         Index : Integer := Pointer;
         From  : Integer := 1;
         To    : Integer;
      begin
         loop
            This.Possibility := Value.Possibility (From);
            This.Necessity   := Value.Necessity   (From);
            if This /= Certain_False then
               To := From + 1;
               while (  To <= Value.Cardinality
                     and then
                        This.Possibility = Value.Possibility (To)
                     and then
                        This.Necessity = Value.Necessity (To)
                     )
               loop
                  To := To + 1;
               end loop;
               if Index /= Pointer then
                  Put (Text, Index, ", ");
               end if;
               Put (Text, Index, Data, From, To - 1);
               if This /= Default then
                  Put (Text, Index, ':');
                  Put (Text, Index, This.Possibility);
                  if (  This.Necessity
                     /= (This.Possibility and Default.Necessity)
                     )
                  then
                     Put (Text, Index, ':');
                     Put (Text, Index, This.Necessity);
                  end if;
               end if;
               From := To;
            else
               From := From + 1;
            end if;
            exit when From > Value.Cardinality;
         end loop;
         if Index = Pointer then
            Put (Text, Index, Data, 1, Value.Cardinality);
            Put (Text, Index, ":0");
         end if;
         Adjust_Output_Field
         (  Destination,
            Pointer,
            Index,
            Out_Field,
            Field,
            Justify,
            Fill
         );
      end;
   end Put;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural       := 0;
                Justify     : Alignment     := Left;
                Fill        : Character     := ' '
             )  is
   begin
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Fuzzy.Intuitionistic.Set (Value),
         Default     => Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Image
            (  Data    : User_Data;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
      Size_Inc : constant := 80;
      Size     : Natural := Size_Inc;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Positive := 1;
         begin
            Put (Text, Pointer, Data, Value, Default);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               null;
         end;
         Size := Size + Size_Inc;
      end loop;
   end Image;

   function Image
            (  Data    : User_Data;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
   begin
      return Image (Data, Fuzzy.Intuitionistic.Set (Value), Default);
   end Image;

end Fuzzy.Generic_Edit.Intuitionistic;
