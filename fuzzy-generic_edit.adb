--                                                                    --
--  package Fuzzy.Generic_Edit      Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2003       --
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

with Name_Tables;

package body Fuzzy.Generic_Edit is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data;
                Value   : out Set
             )  is
      subtype Domain is Integer range Value'range;
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
      Result : Set (Domain) := (others => Confidence'First);
      Given  : Map := (others => False);

      procedure Get_Item
                (  Pointer  : in out Integer;
                   Got_Item : in out Boolean
                )  is
         Index     : Integer := Pointer;
         From      : Domain;
         To        : Domain;
         Level     : Confidence;
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
         Get_Weight (Source, Index, Level);
         for Point in From..To loop
            if Given (Point) then
               if Result (Point) /= Level then
                  if Exclusive then
                     raise Data_Error;
                  else
                     Result (Point) := Result (Point) or Level;
                  end if;
               end if;
            else
               Given  (Point) := True;
               Result (Point) := Level;
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

   function Value
            (  Source : String;
               Data   : User_Data;
               First  : Integer;
               Last   : Integer
            )  return Set is
      Result  : Set (First..Last);
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Data, Result);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                Value       : Set;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      if Value'Length = 0 then
         raise Constraint_Error;
      end if;
      declare
         Out_Field : constant Natural :=
                        Get_Output_Field (Destination, Pointer, Field);
         subtype Output is String (Pointer..Pointer + Out_Field - 1);
         Text  : Output renames
                    Destination (Pointer..Pointer + Out_Field - 1);
         This  : Confidence;
         Index : Integer := Pointer;
         From  : Integer := Value'First;
         To    : Integer;
      begin
         loop
            This := Value (From);
            if Confidence'First /= This then
               To := From + 1;
               while To <= Value'Last and then This = Value (To) loop
                  To := To + 1;
               end loop;
               if Index /= Pointer then
                  Put (Text, Index, ", ");
               end if;
               Put (Text, Index, Data, From, To - 1);
               if This /= Confidence'Last then
                  Put (Text, Index, ':');
                  Put (Text, Index, This);
               end if;
               From := To;
            else
               From := From + 1;
            end if;
            exit when From > Value'Last;
         end loop;
         if Index = Pointer then
            Put (Text, Index, Data, Value'First, Value'Last);
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

   function Image (Data : User_Data; Value : Set) return String is
      Size_Inc : constant := 80;
      Size     : Natural := Size_Inc;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Positive := 1;
         begin
            Put (Text, Pointer, Data, Value);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               null;
         end;
         Size := Size + Size_Inc;
      end loop;
   end Image;

end Fuzzy.Generic_Edit;
