--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Edit.Intuitionistic                   Luebeck            --
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

package body Fuzzy.Edit.Intuitionistic is
   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Fuzzy.Intuitionistic.Set;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      Data : User_Data (1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Get (Source, Pointer, Data, Value, Default);
   end Get;

   procedure Get
             (  Source  : String;
                Pointer : in out Integer;
                Value   : out Fuzzy.Intuitionistic.Classification;
                Default : Fuzzy_Boolean := Certain_True
             )  is
      Data : User_Data (1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Get (Source, Pointer, Data, Value, Default);
   end Get;

   function Value
            (  Source      : String;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Set is
      Data : User_Data (1, Cardinality);
   begin
      return
         Intuitionistic_Edit.Value (Source, Data, Cardinality, Default);
   end Value;

   function Value
            (  Source      : String;
               Cardinality : Positive;
               Default     : Fuzzy_Boolean := Certain_True
            )  return Fuzzy.Intuitionistic.Classification is
      Data : User_Data (1, Cardinality);
   begin
      return
         Intuitionistic_Edit.Value (Source, Data, Cardinality, Default);
   end Value;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Value       : Fuzzy.Intuitionistic.Set;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : User_Data (1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
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
                Value       : Fuzzy.Intuitionistic.Classification;
                Default     : Fuzzy_Boolean := Certain_True;
                Field       : Natural := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             )  is
      Data : User_Data (1, Value.Cardinality);
   begin
      Intuitionistic_Edit.Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Data        => Data,
         Value       => Value,
         Default     => Default,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Image
            (  Value   : Fuzzy.Intuitionistic.Set;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
      Data : User_Data (1, Value.Cardinality);
   begin
      return Intuitionistic_Edit.Image (Data, Value, Default);
   end Image;

   function Image
            (  Value   : Fuzzy.Intuitionistic.Classification;
               Default : Fuzzy_Boolean := Certain_True
            )  return String is
      Data : User_Data (1, Value.Cardinality);
   begin
      return Intuitionistic_Edit.Image (Data, Value, Default);
   end Image;

end Fuzzy.Edit.Intuitionistic;
