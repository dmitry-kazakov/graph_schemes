--                                                                    --
--  package Fuzzy.Abstract_Edit     Copyright (c)  Dmitry A. Kazakov  --
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

package body Fuzzy.Abstract_Edit is

   procedure Get_Range
             (  Source    : String;
                Pointer   : in out Integer;
                Data      : IO_Data;
                From      : out Integer;
                To        : out Integer;
                Exclusive : out Boolean
             )  is
      First, Last : Value_Index;
   begin
      Get (Source, Pointer, Data.Data.all, First, Last, Exclusive);
      From := Data.From + (Integer (First) - 1);
      To   := Data.From + (Integer (Last)  - 1);
   end Get_Range;

   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : IO_Data;
                From        : Integer;
                To          : Integer;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
   begin
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data.Data.all,
         From        => Value_Index (From - Data.From + 1),
         To          => Value_Index (To   - Data.From + 1),
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put_Range;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Set
             )  is
      Param : IO_Data (Data'Unchecked_Access, Value'First, Value'Last);
   begin
      Set_Edit.Get (Source, Pointer, Param, Value);
   end Get;

   function Value
            (  Source : String;
               Data   : User_Data'Class
            )  return Set is
      First : Integer;
      Last  : Integer;
   begin
      Get_Max_Range (Data, First, Last);
      declare
         Param : IO_Data (Data'Unchecked_Access, First, Last);
      begin
         return Set_Edit.Value (Source, Param, First, Last);
      end;
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data'Class;
                Value       : Set;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Param : IO_Data (Data'Unchecked_Access, Value'First, Value'Last);
   begin
      Set_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Param,
         Value       => Value,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Image (Data : User_Data'Class; Value : Set)
      return String is
      Param : IO_Data (Data'Unchecked_Access, Value'First, Value'Last);
   begin
      return Set_Edit.Image (Param, Value);
   end Image;

end Fuzzy.Abstract_Edit;
