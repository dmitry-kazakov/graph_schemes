--                                                                    --
--  package Fuzzy.Edit              Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2000       --
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
with Fuzzy.Basic_Edit;         use Fuzzy.Basic_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.Fields;      use Strings_Edit.Fields;

package body Fuzzy.Edit is
   procedure Get
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : User_Data;
                From      : out Integer;
                To        : out Integer;
                Exclusive : out Boolean
             )  is
      Got_It : Boolean;
   begin
      Get
      (  Source  => Source,
         Pointer => Pointer,
         Value   => From,
         First   => Data.First,
         Last    => Data.Last
      );
      Get_Delimiter (Source, Pointer, Got_It, Ellipsis);
      if Got_It then
         begin
            Get
            (  Source  => Source,
               Pointer => Pointer,
               Value   => To,
               First   => Data.First,
               Last    => Data.Last
            );
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         To := From;
      end if;
      Exclusive := True;
   end Get;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Out_Field : constant Natural :=
                     Get_Output_Field (Destination, Pointer, Field);
      subtype Output is String (Pointer..Pointer + Out_Field - 1);
      Text  : Output renames
                 Destination (Pointer..Pointer + Out_Field - 1);
      Index : Integer := Pointer;
   begin
      Put (Text, Index, From);
      if From /= To then
         Put (Text, Index, "..");
         Put (Text, Index, To);
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
   end Put;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Set
             )  is
      Data : User_Data (Value'First, Value'Last);
   begin
      Set_Edit.Get (Source, Pointer, Data, Value);
   end Get;

   function Value
            (  Source : String;
               First  : Integer;
               Last   : Integer
            )  return Set is
      Data : User_Data (First, Last);
   begin
      return Set_Edit.Value (Source, Data, First, Last);
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Set;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : User_Data (Value'First, Value'Last);
   begin
      Set_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Value,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Image (Value : Set) return String is
      Data : User_Data (Value'First, Value'Last);
   begin
      return Set_Edit.Image (Data, Value);
   end Image;

end Fuzzy.Edit;
