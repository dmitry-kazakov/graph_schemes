--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Edit                     Luebeck            --
--  Implementation                                 Autumn, 2004       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Fuzzy.Graph.Edit;  use Fuzzy.Graph.Edit;

package body Fuzzy.Graph.Handle.Edit is

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Node       : out Node_Handle;
                Features   : Table;
                Parameters : Input_Parameters'Class := Input_Defaults;
                Factory    : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
             )  is
      Result : Graph_Node_Ptr;
   begin
      Get (Source, Pointer, Result, Features, Parameters, Factory);
      Node := Ref (Result);
   end Get;

   function Image
            (  Node       : Node_Handle;
               Parameters : Output_Parameters'Class := Output_Defaults
            )  return String is
   begin
      return Image (Ptr (Node).all, Parameters);
   end Image;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Node_Handle;
                Parameters  : Output_Parameters'Class :=
                                 Output_Defaults;
                Field       : in Natural   := 0;
                Justify     : in Alignment := Left;
                Fill        : in Character := ' '
             )  is
   begin
      Put
      (  Destination => Destination,
         Pointer     => Pointer,
         Node        => Ptr (Node).all,
         Parameters  => Parameters,
         Field       => Field,
         Justify     => Justify,
         Fill        => Fill
      );
   end Put;

   function Value
            (  Source     : String;
               Features   : Table;
               Parameters : Input_Parameters'Class := Input_Defaults;
               Factory    : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle is
      Result : constant Graph_Node_Ptr :=
                        Value (Source, Features, Parameters, Factory);
   begin
      return Ref (Result);
   end Value;

end Fuzzy.Graph.Handle.Edit;
