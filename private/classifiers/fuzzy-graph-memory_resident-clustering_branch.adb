--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Memory_Resident.                Luebeck            --
--        Clustering_Branch                        Autumn, 2006       --
--  Implementation                                                    --
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

with Ada.Exceptions;               use Ada.Exceptions;
with Ada.IO_Exceptions;            use Ada.IO_Exceptions;
with Ada.Tags;                     use Ada.Tags;
with Fuzzy.Basic_Edit;             use Fuzzy.Basic_Edit;
with Fuzzy.Graph.Node_Class_Sets;  use Fuzzy.Graph.Node_Class_Sets;
with Strings_Edit;                 use Strings_Edit;
with System;                       use System;

with Fuzzy.Feature.Classificatory.Clustering;
with Strings_Edit.Integers;
with Strings_Edit.Floats;

package body Fuzzy.Graph.Memory_Resident.Clustering_Branch is

   type Node_Ptr is access constant Graph_Node'Class;
   Class : constant String := Node_Class & "Clustering_Branch";
--
-- Create_Unchecked -- A new branch node
--
--    Header    - The graph header
--    Feature   - The node's feature
--    Successor - The outgoing arcs
--    Sequence  - The sequence number
--    Examples  - The number of
--
-- Returns :
--
--    Pointer to the node
--
   function Create_Unchecked
            (  Header    : Header_Handle;
               Feature   : Feature_Object'Class;
               Successor : Alternatives_Array;
               Sequence  : Sequence_No;
               Examples  : Positive
            )  return Graph_Node_Ptr;
--
-- Modify_Unchecked -- A branch node
--
--    Parent    - The node to modify
--    Data      - The node modification data
--    Successor - The outgoing arcs of the result
--    Updated   - The set of updated children
--    Count     - The number of updated children (denominator)
--    Exclusive - Controls new node creation
--    New_Node  - The result if a new node is created
--
   procedure Modify_Unchecked
             (  Parent     : in out Clustering_Branch_Node;
                Data       : in out Node_Modification_Data'Class;
                Successor  : in out Node_Ptr_Array;
                Updated    : Domain_Subset;
                Count      : Natural;
                Exclusive  : Boolean;
                New_Node   : in out Graph_Node_Ptr
             );
--
-- Sort -- The clusters according to their weights
--
--    Node - The node
--
   procedure Sort (Node : Clustering_Branch_Node'Class) is
      use Frequency_To_Value_Maps;
   begin
      if not Node.Sorted then
         declare
            This : Clustering_Branch_Node'Class renames
                      Clustering_Branch_Node'Class (Node.Self.all);
            List : Frequency_To_Value_Maps.Set renames
                      Ptr (This.Layer.Header.Handle).Sorter;
         begin
            Erase (List);
            for Index in Node.Successor'Range loop
               if Node.Successor (Index).Weight > 0.0 then
                  Add (List, (Node.Successor (Index).Weight, Index));
               end if;
            end loop;
            for Index in 1..Get_Size (List) loop
               This.Successor (Index).Route := Get (List, Index).Value;
            end loop;
            for Index in Get_Size (List) + 1..Node.Successor'Last loop
               This.Successor (Index).Route := 0;
            end loop;
            This.Sorted := True;
         end;
      end if;
   end Sort;

   procedure Classify
             (  Node   : Clustering_Branch_Node;
                Data   : in out Classification_Parameters'Class;
                Weight : Confidence := Confidence'Last
             )  is
      Route : Natural;
   begin
      Sort (Node);
      Route := Node.Successor (1).Route;
      if Route = 0 then
         --
         -- No nodes to follow.
         --
         Or_At (Data.Result.Possibility, Weight);
         And_At (Data.Result.Necessity, not Weight);
      else
         --
         -- Accumulate the classifications of the nodes with the highest
         -- weight.  These  are the nodes indicated by the values of the
         -- Route field of the first elements of the array Successor.
         --
         declare
            Threshold : constant Cluster_Frequency :=
                           Node.Successor (Route).Weight;
         begin
            Classify (Node.Successor (Route).Node.all, Data, Weight);
            for Index in 2..Node.Successor'Last loop
               Route := Node.Successor (Index).Route;
               exit when
                    (  Route = 0
                    or else
                       Node.Successor (Route).Weight < Threshold
                    );
               Classify (Node.Successor (Route).Node.all, Data, Weight);
            end loop;
         end;
      end if;
   end Classify;

   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array
            )  return Graph_Node_Ptr is
      Successor : Alternatives_Array (1..Feature.Cardinality);
      Sequence  : Sequence_No := 0;

      subtype Children_List is Node_Ptr_Array (Successor'Range);

      procedure Do_It (Node : Children_List) is
         pragma Inline (Do_It);
         Child : Graph_Node_Ptr;
         Count : Natural     := 0;
      begin
         for Index in Successor'Range loop
            Child := Node (Index);
            if Child /= null then
               Check_Child (Header, Child.all);
               Count := Count + 1;
               Successor (Index).Node  := Child;
               Successor (Count).Route := Index;
               Sequence := Sequence + Child.Sequence;
            end if;
         end loop;
         if Count = 0 then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Creating a branch with no children"
            );
         end if;
         declare
            Weight : constant Cluster_Frequency :=
                        1.0 / Cluster_Frequency (Count);
         begin
            for Index in 1..Count loop
               Successor (Successor (Index).Route).Weight := Weight;
            end loop;
         end;
      end Do_It;
   begin
      if Children'Length /= Feature.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the children array"
         );
      elsif not Is_Clustering (Feature) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not a clustering feature"
         );
      end if;
      Do_It (Children);
      return Create_Unchecked (Header, Feature, Successor, Sequence, 1);
   end Create;

   function Create
            (  Header   : Header_Handle;
               Feature  : Feature_Object'Class;
               Children : Node_Ptr_Array;
               Weights  : Fuzzy.Set
            )  return Graph_Node_Ptr is
      use Frequency_To_Value_Maps;
      Successor : Alternatives_Array (1..Feature.Cardinality);
      Sequence  : Sequence_No := 0;

      subtype Children_List is Node_Ptr_Array (Successor'Range);
      subtype Weights_Set is Fuzzy.Set (Successor'Range);

      procedure Do_It (Node : Children_List; Arc : Weights_Set) is
         pragma Inline (Do_It);
         List : Frequency_To_Value_Maps.Set renames
                Ptr (Header.Handle).Sorter;
         Item : Frequency_To_Value;
         Sum  : Cluster_Frequency := 0.0;
         This : Cluster_Frequency := Cluster_Frequency'First;
      begin
         Erase (List);
         for Index in Successor'Range loop
            Item.Frequency := Cluster_Frequency (Arc (Index));
            if Item.Frequency > 0.0 and then Node (Index) /= null then
               Item.Value := Index;
               Add (List, Item);
               Check_Child (Header, Children (Index).all);
            end if;
         end loop;
         if Get_Size (List) = 0 then
            Raise_Exception
            (  Constraint_Error'Identity,
               "Creating a branch with no children"
            );
         end if;
         for Index in reverse 1..Get_Size (List) loop
            Item := Get (List, Index);
            if This /= Item.Frequency then
               This := Item.Frequency - Sum;
               Sum  := Sum + Item.Frequency;
            end if;
            declare
               Arc : Alternative renames Successor (Item.Value);
            begin
               Arc :=
               (  Node   => Node (Item.Value),
                  Weight => This,
                  Route  => Index
               );
               Sequence := Sequence + Arc.Node.Sequence;
            end;
         end loop;
      end Do_It;

   begin
      if Children'Length /= Feature.Cardinality then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the children array"
         );
      elsif Feature.Cardinality /= Weights'Length then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Wrong cardinality of the distribution"
         );
      elsif not Is_Clustering (Feature) then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Not a clustering feature"
         );
      end if;
      Do_It (Children, Weights);
      return Create_Unchecked (Header, Feature, Successor, Sequence, 1);
   end Create;

   function Create_Unchecked
            (  Header    : Header_Handle;
               Feature   : Feature_Object'Class;
               Successor : Alternatives_Array;
               Sequence  : Sequence_No;
               Examples  : Positive
            )  return Graph_Node_Ptr is
      New_Ptr : Graph_Node_Ptr :=
                   new Clustering_Branch_Node (Feature.Cardinality);
      Branch  : Clustering_Branch_Node renames
                   Clustering_Branch_Node (New_Ptr.all);
   begin
      Branch.Self      := New_Ptr;
      Branch.Successor := Successor;
      Branch.Sequence  := Branch.Sequence + Sequence;
      Branch.Examples  := Examples;
      for Index in Successor'Range loop
         if Successor (Index).Node /= null then
            Increment_Count (Successor (Index).Node.all);
         end if;
      end loop;
      Attach (New_Ptr, Header, Feature);
      return New_Ptr;
   exception
      when others =>
         if New_Ptr /= null and then New_Ptr.Use_Count = 0 then
            Free (New_Ptr);
         end if;
         raise;
   end Create_Unchecked;

   procedure Finalize (Node : in out Clustering_Branch_Node) is
   begin
      Close (Node);
      for Index in Node.Successor'Range loop
         Release (Node.Successor (Index).Node);
      end loop;
      Finalize (Memory_Node (Node));
   end Finalize;

   function Find
            (  Parent : Clustering_Branch_Node;
               Child  : Graph_Node'Class;
               Index  : Positive
            )  return Natural is
      Node : constant Node_Ptr := Child'Unchecked_Access;
   begin
      for Child in Index..Parent.Cardinality loop
         if Node = Node_Ptr (Parent.Successor (Index).Node) then
            return Child;
         end if;
      end loop;
      return 0;
   end Find;

   procedure Get_Children
             (  Node     : Clustering_Branch_Node;
                Children : in out Node_Ptr_Array;
                Pointer  : in out Integer
             )  is
      Place : Integer;
   begin
      for Index in Node.Successor'Range loop
         Add
         (  Children,
            Pointer,
            Node.Successor (Index).Node,
            Place
         );
      end loop;
   end Get_Children;

   function Get_Child (Node : Clustering_Branch_Node; Index : Positive)
      return Graph_Node_Ptr is
   begin
      if Index <= Node.Cardinality then
         return Node.Successor (Index).Node;
      else
         return null;
      end if;
   end Get_Child;

   function Get_Children_Number
            (  Node      : Clustering_Branch_Node;
               Immediate : Boolean := True
            )  return Natural is
      Children : Node_Ptr_Array (0..Node.Cardinality - 1);
      Result   : Natural := Children'First;
   begin
      Get_Children (Node, Children, Result);
      if Immediate then
         return Result;
      else
         for Index in 0..Result - 1 loop
            Result :=
               (  Result
               +  Get_Children_Number (Children (Index).all, False)
               );
         end loop;
         return Result;
      end if;
   end Get_Children_Number;

   function Get_Class (Node : Clustering_Branch_Node) return String is
   begin
      return Class;
   end Get_Class;

   function To_Confidence (Value : Cluster_Frequency)
      return Confidence is
   begin
      return Confidence (Value);
   exception
      when Constraint_Error =>
         if Value > 0.5 then
            return Confidence'Last;
         else
            return Confidence'First;
         end if;
   end To_Confidence;

   function Get_Distribution (Node : Clustering_Branch_Node)
      return Classification is
      Total : Cluster_Frequency := 0.0;
   begin
      Sort (Node);
      for Index in Node.Successor'Range loop
         Total := Total + Node.Successor (Index).Weight;
      end loop;
      if Total = 0.0 then
         return
         (  Cardinality => Node.Cardinality,
            Possibility => (others => Confidence'First),
            Necessity   => (others => Confidence'Last)
         );
      end if;
      declare
         Result    : Classification (Node.Cardinality);
         Route     : Natural           := Node.Successor (1).Route;
         Level     : Confidence        := Confidence'Last;
         Threshold : Cluster_Frequency := Node.Successor (Route).Weight;
         Sum       : Cluster_Frequency := Total - Threshold;
      begin
         Result.Possibility (Route) := Level;
         Result.Necessity   (Route) := Confidence'Last;
         for Index in 2..Node.Successor'Last loop
            Route := Node.Successor (Index).Route;
            exit when Route = 0;
            if Threshold > Node.Successor (Route).Weight then
               Threshold := Node.Successor (Route).Weight;
               Level := To_Confidence (Sum / Total);
            end if;
            Sum := Sum - Threshold;
            Result.Possibility (Route) := Level;
            Result.Necessity   (Route) := Confidence'First;
            if Index = 2 then
               Result.Necessity (Node.Successor (1).Route) := not Level;
            end if;
         end loop;
         return Result;
      end;
   end Get_Distribution;

   procedure Get_Examples
             (  Node   : Clustering_Branch_Node;
                Lesson : in out Lecture_Handle;
                Image  : Image_Type;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Route : Natural;
   begin
      Sort (Node);
      Route := Node.Successor (1).Route;
      if Route /= 0 then
         --
         -- Accumulate the  examples  of  the  nodes  with  the  highest
         -- weight.  These  are the nodes indicated by the values of the
         -- Route field of the first elements of the array Successor.
         --
         declare
            Threshold : constant Cluster_Frequency :=
                           Node.Successor (Route).Weight;
         begin
            Get_Examples
            (  Node.Successor (Route).Node.all,
               Lesson,
               Image,
               Viewer
            );
            for Index in 2..Node.Successor'Last loop
               Route := Node.Successor (Index).Route;
               exit when
                    (  Route = 0
                    or else
                       Node.Successor (Route).Weight < Threshold
                    );
               Get_Examples
               (  Node.Successor (Route).Node.all,
                  Lesson,
                  Image,
                  Viewer
               );
            end loop;
         end;
      end if;
   end Get_Examples;

   procedure Get_Referents
             (  Node : Clustering_Branch_Node;
                List : in out Deposit_Container'Class
             )  is
   begin
      Add
      (  List,
         To_Deposit_Ptr (Ptr (Node.Layer.Header.Handle)),
         False
      );
      for Index in Node.Successor'Range loop
         Add
         (  List,
            To_Deposit_Ptr (Node.Successor (Index).Node),
            False
         );
      end loop;
   exception
      when Error : Constraint_Error =>
         Raise_Exception
         (  Use_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Referents;

   function Get_Type (Node : Clustering_Branch_Node) return Node_Type is
   begin
      return Tree_Cluster;
   end Get_Type;

   type Rank_Order is (Less, Equal, Greater);

   function Compare
            (  Left, Right : Alternatives_Array;
               Left_Count  : Positive;
               Right_Count : Positive;
               Equivalence : Cluster_Frequency
            )  return Rank_Order is
      pragma Inline (Compare);
   begin
      if (  Left_Count = Right_Count
         or else
            (  Equivalence
            >= (  abs Cluster_Frequency (Right_Count - Left_Count)
               /  Cluster_Frequency (Right_Count + Left_Count)
         )  )  )
      then
         declare
            Factor : constant Cluster_Frequency :=
                        (  Cluster_Frequency (Right_Count)
                        /  Cluster_Frequency (Left_Count)
                        );
            Eps    : constant Cluster_Frequency :=
                        Equivalence * Cluster_Frequency (Right_Count);
            Diff   : Cluster_Frequency;
         begin
            for Index in Left'Range loop
               declare
                  L : Alternative renames Left  (Index);
                  R : Alternative renames Right (Index);
               begin
                  if L.Node = R.Node then
                     Diff := L.Weight * Factor - R.Weight;
                     if abs Diff > Eps then
                        if Diff > 0.0 then
                           return Greater;
                        else
                           return Less;
                        end if;
                     end if;
                  elsif L.Node = null then
                     return Less;
                  elsif R.Node = null then
                     return Greater;
                  elsif L.Node.all'Address > R.Node.all'Address then
                     return Greater;
                  else
                     return Less;
                  end if;
               end;
            end loop;
            return Equal;
         end;
      else
         if Left_Count < Right_Count then
            return Less;
         else
            return Greater;
         end if;
      end if;
   end Compare;

   function Like
            (  Left  : Clustering_Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if (  Clustering_Branch_Node'Tag /= Right'Tag
         or else
            Left.Cardinality /= Right.Cardinality
         )
      then
         return False;
      end if;
      declare
         Other : Clustering_Branch_Node'Class renames
                    Clustering_Branch_Node'Class (Right);
         Equivalence : Cluster_Frequency;
      begin
         if Other.Layer /= null then
            Equivalence := Get_Equivalence (Other.Layer.Feature);
         elsif Left.Layer /= null then
            Equivalence := Get_Equivalence (Left.Layer.Feature);
         else
            Equivalence := Cluster_Equivalence;
         end if;
         return
         (  Equal
         =  Compare
            (  Left.Successor,
               Other.Successor,
               Left.Examples,
               Other.Examples,
               Equivalence
         )  );
      end;
   end Like;

   procedure Modify
             (  Node        : in out Clustering_Branch_Node;
                Data        : in out Node_Modification_Data'Class;
                Children    : Node_Ptr_Array;
                Exclusive   : Boolean;
                Combination : Node_Modification;
                New_Node    : in out Graph_Node_Ptr
             )  is
      Successor : Node_Ptr_Array (1..Node.Cardinality) := Children;
      Updated   : Domain_Subset (Successor'Range) := (others => True);
      Count     : Natural := 0;
   begin
      if Node.Layer = null then
         Raise_Exception
         (  Constraint_Error'Identity,
            "Modifying no graph"
         );
      end if;
      case Combination is
         when Union =>
            for Index in Successor'Range loop
               if Successor (Index) = null then
                  if Node.Successor (Index).Node /= null then
                     Successor (Index) := Node.Successor (Index).Node;
                  end if;
                  Updated (Index) := False;
               else
                  Check_Child (Node, Successor (Index).all);
                  Count := Count + 1;
               end if;
            end loop;
         when Intersection =>
            for Index in Successor'Range loop
               if Successor (Index) /= null then
                  if Node.Successor (Index).Node = null then
                     Successor (Index) := null;
                  else
                     Check_Child (Node, Successor (Index).all);
                     Count := Count + 1;
                  end if;
               end if;
            end loop;
         when Replacement =>
            for Index in Successor'Range loop
               if Successor (Index) /= null then
                  Check_Child (Node, Successor (Index).all);
                  Count := Count + 1;
               end if;
            end loop;
      end case;
      Modify_Unchecked
      (  Node,
         Data,
         Successor,
         Updated,
         Count,
         Exclusive,
         New_Node
      );
   end Modify;

   procedure Modify
             (  Node         : in out Clustering_Branch_Node;
                Data         : in out Node_Modification_Data'Class;
                Distribution : Classification;
                Exclusive    : Boolean;
                Combination  : Node_Modification;
                New_Node     : in out Graph_Node_Ptr
             )  is
   begin
      Raise_Exception
      (  Constraint_Error'Identity,
         "Modify was called on a branch node"
      );
   end Modify;

   procedure Modify_Unchecked
             (  Parent    : in out Clustering_Branch_Node;
                Data      : in out Node_Modification_Data'Class;
                Successor : in out Node_Ptr_Array;
                Updated   : Domain_Subset;
                Count     : Natural;
                Exclusive : Boolean;
                New_Node  : in out Graph_Node_Ptr
             )  is
      Same     : Boolean     := True;
      Examples : Natural     := Parent.Examples;
      Sequence : Sequence_No := 0;
      Result   : Alternatives_Array (Updated'Range);
   begin
      if Count > 0 then
         Examples := Examples + 1;
      end if;
      --
      -- Calculating new weights
      --
      declare
         Increment : constant Cluster_Frequency :=
                        (  1.0
                        /  Cluster_Frequency (Natural'Max (Count, 1))
                        );
         Eps       : constant Cluster_Frequency :=
                        (  Cluster_Frequency (Data.Equivalence)
                        *  Cluster_Frequency (Examples)
                        );
         Max       : Cluster_Frequency := Cluster_Frequency'First;
      begin
         for Index in Successor'Range loop
            declare
               New_Item : Alternative renames Result (Index);
               Old_Item : Alternative renames Parent.Successor (Index);
            begin
               New_Item.Node := Successor (Index);
               if Old_Item.Node /= New_Item.Node then
                  Same := False;
               end if;
               if New_Item.Node = null then
                  New_Item.Weight := 0.0;
                  if Old_Item.Weight > Eps then
                     Same := False;
                  end if;
               else
                  Sequence := Sequence + New_Item.Node.Sequence;
                  if Updated (Index) then
                     New_Item.Weight := Old_Item.Weight + Increment;
                     if Increment > Eps then
                        Same := False;
                     end if;
                  else
                     New_Item.Weight := Old_Item.Weight;
                  end if;
               end if;
            end;
         end loop;
      end;
      --
      -- If parent stays unchanged, then nothing  is  done,  except than
      -- increasing the examples number. Otherwise Successor and Weights
      -- are set.
      --
      if Same and then Parent.Sequence = Sequence then
         Parent.Examples := Examples;
         return;
      elsif Exclusive then
         declare
            Old_Node  : constant Graph_Node_Ptr := Parent.Self;
            Used_Node : Graph_Node_Ptr := Old_Node;
            Child     : Graph_Node_Ptr;
         begin
            Remove (Parent.Layer.Nodes, Used_Node);
            --
            -- Use  counts  of  new  children  are  incremented first to
            -- prevent their premature deletion  on  disconnection  when
            -- they also appear as old children of the parent.
            --
            for Index in Result'Range loop
               Child := Successor (Index);
               if Child /= null then
                  Increment_Count (Child.all);
               end if;
            end loop;
            --
            -- Now we can safely disconnect old children
            --
            for Index in Parent.Successor'Range loop
               Child := Parent.Successor (Index).Node;
               if Child /= null then
                  Release (Child);
               end if;
            end loop;
            Parent.Sorted    := False;
            Parent.Successor := Result;
            Parent.Examples  := Examples;
            Insert (Parent.Layer.Nodes, Used_Node);
            if Old_Node = Used_Node then
               Parent.Sequence := Get_Sequence_No + Sequence;
            else
               New_Node     := Used_Node;
               Parent.Layer := null;
            end if;
         end;
      else
         New_Node :=
            Create_Unchecked
            (  Parent.Layer.Header,
               Ptr (Parent.Layer.Feature).all,
               Result,
               Sequence,
               Examples
            );
      end if;
   end Modify_Unchecked;

   function Precedent
            (  Left  : Clustering_Branch_Node;
               Right : Graph_Node'Class
            )  return Boolean is
   begin
      if Left.Cardinality /= Right.Cardinality then
         return Left.Cardinality < Right.Cardinality;
      elsif Clustering_Branch_Node'Tag /= Right'Tag then
         return
         (  Clustering_Branch_Node'External_Tag
         <  External_Tag (Right'Tag)
         );
      end if;
      declare
         Other : Clustering_Branch_Node'Class renames
                    Clustering_Branch_Node'Class (Right);
         Equivalence : Cluster_Frequency;
      begin
         if Other.Layer /= null then
            Equivalence := Get_Equivalence (Other.Layer.Feature);
         elsif Left.Layer /= null then
            Equivalence := Get_Equivalence (Left.Layer.Feature);
         else
            Equivalence := Cluster_Equivalence;
         end if;
         return
         (  Less
         =  Compare
            (  Left.Successor,
               Other.Successor,
               Left.Examples,
               Other.Examples,
               Equivalence
         )  );
      end;
   end Precedent;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Node    : out Deposit_Ptr
             )  is
      use Floats;
      use Integers;
      Header   : Graph_Header_Ptr;
      Node_Ptr : Graph_Node_Ptr;
   begin
      begin
         Header := To_Graph_Header_Ptr (Get (List, 1));
         if Header = null then
            Raise_Exception
            (  Use_Error'Identity,
               "No graph for restored leaf node"
            );
         end if;
      exception
         when Error : Constraint_Error =>
            Raise_Exception
            (  Use_Error'Identity,
               Exception_Message (Error)
            );
      end;
      declare
         Cardinality : constant Positive :=
                          Get_Cardinality (Header.Classes);
         Position    : Integer := Pointer;
      begin
         Node_Ptr := new Clustering_Branch_Node (Cardinality);
         declare
            Weight : Cluster_Frequency;
            Branch : Clustering_Branch_Node renames
                        Clustering_Branch_Node (Node_Ptr.all);
            Got_It : Boolean  := False;
            Count  : Natural  := 0;
            No     : Positive := 2;
         begin
            Branch.Self := Node_Ptr;
            Get (Source, Position);
            Get (Source, Position, Branch.Examples);
            Get_Delimiter (Source, Position, Got_It, Colon);
            if not Got_It then
               raise Data_Error;
            end if;
            for Index in Branch.Successor'Range loop
               if Count < 1 then
                  Get (Source, Position, Float (Weight));
                  Get (Source, Position);
                  Count := 1;
                  if Position < Source'Last then
                     case Source (Position) is
                        when 'x' =>
                           Position := Position + 1;
                           Get (Source, Position);
                           Get (Source, Position, Count);
                        when ',' =>
                           Position := Position + 1;
                           Get (Source, Position);
                        when others =>
                           raise Data_Error;
                     end case;
                  end if;
               end if;
               Count := Count - 1;
               Branch.Successor (Index).Weight := Weight;
               if Weight > 0.0 then
                  Branch.Successor (Index).Node :=
                     To_Graph_Node_Ptr (Get (List, No));
                  Increment_Count (Branch.Successor (Index).Node.all);
                  No := No + 1;
               end if;
            end loop;
         exception
            when Constraint_Error | End_Error =>
               raise Data_Error;
         end;
         Attach
         (  Node_Ptr,
            (Ada.Finalization.Controlled with Ref (Header)),
            Ptr (Header.Classes).all
         );
         Pointer := Position;
         Node    := To_Deposit_Ptr (Node_Ptr);
         Reset_Modified (Node_Ptr.all);
      exception
         when Use_Error =>
            Free (Node_Ptr);
            Raise_Exception
            (  Use_Error'Identity,
               "Circular dependency encountered"
            );
         when others =>
            Free (Node_Ptr);
            raise;
      end;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Node        : Clustering_Branch_Node
             )  is
      use Floats;
      use Integers;
      Current  : Positive := Node.Cardinality + 1;
      Position : Integer  := Pointer;
   begin
      Put (Destination, Position, Node.Examples);
      Put (Destination, Position, ':');
      for Index in Node.Successor'Range loop
         if (  Current > Node.Cardinality
            or else
               (  Node.Successor (Index  ).Node = null
               xor
                  Node.Successor (Current).Node = null
               )
            or else
               (  Node.Successor (Current).Node /= null
               and then
                  (  Node.Successor (Index  ).Weight
                  /= Node.Successor (Current).Weight
            )  )  )
         then
            if Current <= Node.Cardinality then
               if Index - Current > 1 then
                  Put (Destination, Position, 'x');
                  Put (Destination, Position, Index - Current);
               end if;
               Put (Destination, Position, ',');
            end if;
            if Node.Successor (Index).Node = null then
               Put (Destination, Position, '0');
            else
               Put
               (  Destination,
                  Position,
                  Float (Node.Successor (Index).Weight)
               );
            end if;
            Current := Index;
         end if;
      end loop;
      if Current /= Node.Successor'Last then
         Put (Destination, Position, 'x');
         Put (Destination, Position, Node.Successor'Last + 1 - Current);
      end if;
      Pointer := Position;
   end Store;

   function Tests
            (  Node    : Clustering_Branch_Node;
               Feature : Feature_Object_Ptr
            )  return Boolean is
      Child : Graph_Node_Ptr;
   begin
      if Node.Layer /= null then
         if Equal (Ptr (Node.Layer.Feature).all, Feature.all) then
            return True;
         end if;
         for Index in Node.Successor'Range loop
            Child := Node.Successor (Index).Node;
            if Child /= null and then Tests (Child.all, Feature) then
               return True;
            end if;
         end loop;
      end if;
      return False;
   end Tests;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Graph.Memory_Resident.Clustering_Branch;
