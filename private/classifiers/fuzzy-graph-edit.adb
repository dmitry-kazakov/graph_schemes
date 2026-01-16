--                                                                    --
--  package Fuzzy.Graph.Edit        Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Autumn, 2004       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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
with Fuzzy.Feature.Edit;         use Fuzzy.Feature.Edit;
with Fuzzy.Feature.Handle;       use Fuzzy.Feature.Handle;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Graph.Handle;         use Fuzzy.Graph.Handle;
with Integer_Intervals;          use Integer_Intervals;
with Strings_Edit.Fields;        use Strings_Edit.Fields;
with Strings_Edit.UTF8.Maps;     use Strings_Edit.UTF8.Maps;

with Fuzzy.Graph.Handle.Bounded_Arrays;
use  Fuzzy.Graph.Handle.Bounded_Arrays;

with Name_Tables;

package body Fuzzy.Graph.Edit is

   procedure Get_Node
             (  Source     : String;
                Pointer    : in out Integer;
                Parameters : Input_Parameters'Class;
                Factory    : in out Node_Factory_Ptr;
                Features   : Table;
                Node       : out Graph_Node_Ptr;
                Classes    : in out Feature_Handle
             )  is
      Feature : Feature_Handle;
   begin
      Get (Source, Pointer, Features, Feature);
      if not Feature.Is_Valid then
         raise Use_Error;
      end if;
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer > Source'Last then
         raise Data_Error;
      end if;
      if Source (Pointer) = '=' then
         --
         -- Leaf node
         --
         if Is_Valid (Classes) then
            if Feature /= Classes then
               raise Data_Error;
            end if;
         else
            Classes := Feature;
         end if;
         Pointer := Pointer + 1;
         Get (Source, Pointer, Name_Tables.Blanks);
         declare
            Distribution : Classification (Get_Cardinality (Classes));
         begin
            Get (Source, Pointer, Classes, Distribution, Parameters);
            Node := Create (Factory, Ptr (Classes).all, Distribution);
            Factory := Get_Factory (Node.all);
         exception
            when End_Error =>
               if Node.Use_Count = 0 then
                  Free (Node);
               end if;
               raise Data_Error;
         end;
      elsif Source (Pointer) = '(' then
         --
         -- Branch node
         --
         if Feature = Classes then
            raise Data_Error;
         end if;
         Pointer := Pointer + 1;
         Get (Source, Pointer, Name_Tables.Blanks);
         declare
            Domain   : Fuzzy.Set (1..Get_Cardinality (Feature)) :=
                          (others => Confidence'First);
            Children : Bounded_Array (1, Domain'Last);
            Value    : Fuzzy.Set (Domain'Range);
         begin
            loop -- For each distinct child
               begin
                  Get (Source, Pointer, Feature, Value, Parameters);
               exception
                  when End_Error =>
                     raise Data_Error;
               end;
               if Possibility (Domain, Value) /= Confidence'First then
                  raise Data_Error;
               end if;
               Get (Source, Pointer, Name_Tables.Blanks);
               if (  Pointer + 1 > Source'Last
                  or else
                     (  Source (Pointer) /= '='
                     and then
                        Source (Pointer + 1) /= '>'
                  )  )
               then
                  raise Data_Error;
               end if;
               Pointer := Pointer + 2;
               Get (Source, Pointer, Name_Tables.Blanks);
               Get_Node
               (  Source     => Source,
                  Pointer    => Pointer,
                  Parameters => Parameters,
                  Node       => Node,
                  Factory    => Factory,
                  Features   => Features,
                  Classes    => Classes
               );
               for Index in Value'Range loop
                  if Value (Index) /= Confidence'First then
                     Put (Children, Index, Node);
                  end if;
               end loop;
               if Node.Use_Count = 0 then
                  Free (Node);
               end if;
               Get (Source, Pointer, Name_Tables.Blanks);
               if (  Pointer > Source'Last
                  or else
                     (  Source (Pointer) /= ';'
                     and then
                        Source (Pointer) /= ')'
                  )  )
               then
                  raise Data_Error;
               end if;
               Or_At (Domain, Value);
               Pointer := Pointer + 1;
               exit when Source (Pointer - 1) = ')';
               Get (Source, Pointer, Name_Tables.Blanks);
            end loop;
            declare
               Successors : Node_Ptr_Array (1..Children.Last);
            begin
               for Index in Successors'Range loop
                  Successors (Index) := Get (Children, Index);
               end loop;
               Node :=
                  Create
                  (  Factory,
                     Ptr (Feature).all,
                     Successors,
                     Domain
                  );
            end;
         exception
            when End_Error =>
               raise Data_Error;
         end;
      else
         raise Data_Error;
      end if;
   end Get_Node;

   procedure Get
             (  Source     : String;
                Pointer    : in out Integer;
                Node       : out Graph_Node_Ptr;
                Features   : Table;
                Parameters : Input_Parameters'Class;
                Factory    : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
             )  is
      Default : Node_Factory_Ptr := Factory.all'Unchecked_Access;
      Classes : Feature_Handle;
      Index   : Integer := Pointer;
   begin
      Get_Node
      (  Source     => Source,
         Pointer    => Index,
         Node       => Node,
         Parameters => Parameters,
         Factory    => Default,
         Features   => Features,
         Classes    => Classes
      );
      Pointer := Index;
   end Get;

   procedure Put_Condition
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Feature_Object'Class;
                Node        : Graph_Node'Class;
                Child       : Graph_Node_Ptr;
                Position    : Positive;
                Skip        : in out Domain_Subset;
                Parameters  : Output_Parameters'Class
             )  is
      pragma Inline (Put_Condition);
      First : Boolean := True;

      procedure Flush (From, To : Integer) is
      begin
         if First then
            First := False;
         else
            Put (Destination, Pointer, ", ");
         end if;
         Put
         (  Destination,
            Pointer,
            Feature,
            Interval'(From, To),
            Parameters
         );
      end Flush;

      From     : Integer := Position;
      Range_On : Boolean := True;
   begin
      for Index in Position + 1..Skip'Last loop
         if Get_Child (Node, Index) = Child then
            Skip (Index) := True;
            if not Range_On then
               From := Index;
               Range_On := True;
            end if;
         else
            if Range_On then
               Flush (From, Index - 1);
               Range_On := False;
            end if;
         end if;
      end loop;
      if Range_On then
         Flush (From, Skip'Last);
      end if;
   end Put_Condition;

   procedure Put_Node
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Graph_Node'Class;
                Parameters  : Output_Parameters'Class
             )  is
      Feature : Feature_Object'Class renames Get_Feature (Node).all;
   begin
      Put (Destination, Pointer, Feature.Name.all);
      case Get_Type (Node) is
         when Tree_Leaf =>
            Put (Destination, Pointer, " = ");
            Put
            (  Destination,
               Pointer,
               Feature,
               Get_Distribution (Node),
               Parameters
            );
         when Tree_Branch =>
            Put (Destination, Pointer, '(');
            declare
               First : Boolean := True;
               Skip  : Domain_Subset (1..Node.Cardinality) :=
                          (others => False);
               Child : Graph_Node_Ptr;
            begin
               for Index in Skip'Range loop
                  if not Skip (Index) then
                     Child := Get_Child (Node, Index);
                     if Child /= null then
                        if First then
                           First := False;
                        else
                           Put (Destination, Pointer, "; ");
                        end if;
                        Put_Condition
                        (  Destination => Destination,
                           Pointer     => Pointer,
                           Feature     => Feature,
                           Node        => Node,
                           Child       => Child,
                           Position    => Index,
                           Skip        => Skip,
                           Parameters  => Parameters
                        );
                        Put (Destination, Pointer, " => ");
                        Put_Node
                        (  Destination => Destination,
                           Pointer     => Pointer,
                           Node        => Child.all,
                           Parameters  => Parameters
                        );
                     end if;
                  end if;
               end loop;
            end;
            Put (Destination, Pointer, ')');
         when Tree_Cluster =>
            Put (Destination, Pointer, '(');
            declare
               First  : Boolean := True;
               Value  : Classification := Get_Distribution (Node);
               Weight : Fuzzy.Set (1..Value.Cardinality);
               Child  : Graph_Node_Ptr;
            begin
               for Index in 1..Value.Cardinality loop
                  Weight (Index) := Value.Possibility (Index);
                  if Weight (Index) /= Confidence'First then
                     Child := Get_Child (Node, Index);
                     if Child /= null then
                        for Next in Index + 1..Value.Cardinality loop
                           if Child = Get_Child (Node, Next) then
                              Weight (Next) := Value.Possibility (Next);
                              Value.Possibility (Next) :=
                                 Confidence'First;
                              Value.Necessity (Next) :=
                                 Confidence'First;
                           else
                              Weight (Next) := Confidence'First;
                           end if;
                        end loop;
                        if First then
                           First := False;
                        else
                           Put (Destination, Pointer, "; ");
                        end if;
                        Put
                        (  Destination => Destination,
                           Pointer     => Pointer,
                           Feature     => Feature,
                           Value       => Weight,
                           Parameters  => Parameters
                        );
                        Put (Destination, Pointer, " => ");
                        Put_Node
                        (  Destination => Destination,
                           Pointer     => Pointer,
                           Node        => Child.all,
                           Parameters  => Parameters
                        );
                     end if;
                     Weight (Index) := Confidence'First;
                  end if;
               end loop;
            end;
            Put (Destination, Pointer, ')');
      end case;
   end Put_Node;

   procedure Put
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Graph_Node'Class;
                Parameters  : Output_Parameters'Class;
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
      Put_Node (Text, Index, Node, Parameters);
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

   function Image
            (  Node       : Graph_Node'Class;
               Parameters : Output_Parameters'Class
            )  return String is
      Size_Inc : constant := 80;
      Size     : Natural := Size_Inc;
   begin
      loop
         declare
            Text    : String (1..Size);
            Pointer : Positive := 1;
         begin
            Put (Text, Pointer, Node, Parameters);
            return Text (Text'First..Pointer - 1);
         exception
            when Layout_Error =>
               null;
         end;
         Size := Size * 2;
      end loop;
   end Image;

   function Value
            (  Source     : String;
               Features   : Table;
               Parameters : Input_Parameters'Class;
               Factory    : not null access Node_Factory'Class :=
                               Fuzzy.Graph.Memory_Resident.Factory
            )  return Graph_Node_Ptr is
      Result  : Graph_Node_Ptr;
      Pointer : Integer := Source'First;
   begin
      Get (Source, Pointer, Name_Tables.Blanks);
      Get (Source, Pointer, Result, Features, Parameters, Factory);
      Get (Source, Pointer, Name_Tables.Blanks);
      if Pointer /= Source'Last + 1 then
         raise Data_Error;
      end if;
      return Result;
   exception
      when others =>
         Free (Result);
         raise;
   end Value;

end Fuzzy.Graph.Edit;
