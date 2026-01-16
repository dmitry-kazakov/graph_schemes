--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Abstract_Edit.Intuitionistic          Luebeck            --
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

package body Fuzzy.Abstract_Edit.Intuitionistic is

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Get (Source, Pointer, Param, Value, Default);
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Data    : User_Data'Class;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Get (Source, Pointer, Param, Value, Default);
   end Get;

   function Image
            (  Data    : User_Data'Class;
               Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      return Intuitionistic_Edit.Image (Param, Value, Default);
   end Image;

   function Image
            (  Data    : User_Data'Class;
               Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      return Intuitionistic_Edit.Image (Param, Value, Default);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Data        : User_Data'Class;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Param,
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
                Data        : User_Data'Class;
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Param : IO_Data (Data'Unchecked_Access, 1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Param,
         Value       => Value,
         Default     => Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Value
            (  Source  : String;
               Data    : User_Data'Class;
               Default : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Set is
      First : Integer;
      Last  : Integer;
   begin
      Get_Max_Range (Data, First, Last);
      if First /= 1 then
         raise Constraint_Error;
      end if;
      declare
         Param : IO_Data (Data'Unchecked_Access, 1, Last);
      begin
         return Intuitionistic_Edit.Value (Source, Param, Last, Default);
      end;
   end Value;

   function Value
            (  Source  : String;
               Data    : User_Data'Class;
               Default : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Classification is
      First : Integer;
      Last  : Integer;
   begin
      Get_Max_Range (Data, First, Last);
      if First /= 1 then
         raise Constraint_Error;
      end if;
      declare
         Param : IO_Data (Data'Unchecked_Access, 1, Last);
      begin
         return Intuitionistic_Edit.Value (Source, Param, Last, Default);
      end;
   end Value;

end Fuzzy.Abstract_Edit.Intuitionistic;
