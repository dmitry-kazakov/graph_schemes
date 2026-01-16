--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.Implementation         Luebeck            --
--  Implementation                                 Spring, 2002       --
--                                                                    --
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

with Fuzzy.Feature.Classificatory.Clustering;

package body Fuzzy.Graph.Learning.Implementation is
   use Features_Lists;
   use Fuzzy.Feature.Classificatory.Clustering;
   use Fuzzy.Feature.Sets;
--
-- Create_Branches -- Create branches of a new node
--
--    Data       - The training data
--    Children   - The children of the node
--    Weight     - The incoming weight
--    Feature    - Of the node
--    Separators - Separation hypotheses
--    Created    - At least one child was created
--
-- This  procedure creates the branches of a newly created node. Some of
-- the features in the list are computed from the feature tested by  the
-- node. Therefore no optimization is possible.
--
   procedure Create_Branches
             (  Data       : in out Graph_Training_Data'Class;
                Children   : in out Node_Ptr_Array;
                Weight     : Confidence;
                Feature    : Feature_Object'Class;
                Separators : in out Separation_Data;
                Created    : out Boolean
             )  is separate;
   pragma Inline (Create_Branches);
--
-- Create_Branches_Optimized -- Create branches of a node (optimized)
--
--    Data       - The training data
--    Children   - The children of the node
--    Weight     - The incoming weight
--    Feature    - Of the node (a pointer to)
--    Separators - Separation hypotheses
--    Created    - At least one child was created
--
-- This procedure creates the branches of  a  newly  created  node.  The
-- created  branches  can  be reused for multiple feature domain values,
-- because none  of  the remaining  features in  the list depends on the
-- constraints put on the feature values. For this reason one  can  omit
-- constraint setting.
--
   procedure Create_Branches_Optimized
             (  Data       : in out Graph_Training_Data'Class;
                Children   : in out Node_Ptr_Array;
                Weight     : Confidence;
                Feature    : Feature_Object'Class;
                Separators : in out Separation_Data;
                Created    : out Boolean
             )  is separate;
   pragma Inline (Create_Branches_Optimized);
--
-- Is_Controlling -- Check if a feature is the source of a set
--
--    Feature - The feature (potential source)
--    List    - A set of features
--
-- Returns :
--
--    True if Feature is a source of any feature from List
--
   function Is_Controlling
            (  Feature : Feature_Object'Class;
               List    : Features_Lists.Set
            )  return Boolean is
      pragma Inline (Is_Controlling);
   begin
      for Index in 1..Get_Size (List) loop
         if Is_Computed
            (  Ptr (Get (List, Index).Feature).all,
               Feature
            )
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Controlling;
--
-- Modify -- Modify one child node
--
--    Data       - The training data
--    Parent     - The parent node
--    Feature    - Tested by the node
--    Value      - Of the feature
--
-- This procedure modifies the branch of the node  Parent  specified  by
-- the field Parent.To. The weight of  the  branch  is  passed  via  the
-- parameter Value.
--
   procedure Modify
             (  Data       : in out Graph_Training_Data'Class;
                Parent     : in out Separation_Data;
                Feature    : Feature_Object'Class;
                Value      : Confidence
             )  is separate;
   pragma Inline (Modify);
--
-- Modify_Optimized -- Modify one child node (optimized)
--
--    Data    - The training data
--    Parent  - The parent node
--    Value   - Of the feature
--
-- This procedure modifies the branch of the node  Parent  specified  by
-- the field Parent.To. The value of the tested feature passed  via  the
-- parameter Value. The value is modified to remove  the  domain  values
-- for which a modification is made.
--
   procedure Modify_Optimized
             (  Data       : in out Graph_Training_Data'Class;
                Parent     : in out Separation_Data;
                Value      : in out Fuzzy.Set
             )  is separate;
   pragma Inline (Modify_Optimized);

   procedure Learn_New
             (  Node       : in out Graph_Node_Ptr;
                Data       : in out Graph_Training_Data'Class;
                Weight     : Confidence;
                Separators : in out Separation_Data
             )  is
   begin
      if Is_Empty (Data.Features) then
         --
         -- There  is no more features to test, so a leaf node has to be
         -- created.  The  distribution  of  the leaf node is one of the
         -- classes.
         --
         declare
            Distribution : Classification
                           (Get_Cardinality (Data.Classes));
         begin
            Get_Leaf (Data, Weight, Distribution);
            Node :=
               Create
               (  Data.Factory,
                  Ptr (Data.Classes).all,
                  Distribution
               );
            Data.Factory := Get_Factory (Node.all);
         end;
      else
         --
         -- Taking the first feature in the list
         --
         declare
            This    : constant Feature_Statistics_Ptr :=
                         Get (Data.Features, 1);
            Feature : Feature_Object'Class renames
                         Ptr (This.Feature).all;
         begin
            Data.Depth := Data.Depth + 1;
            Remove (Data.Features, 1);
            if Is_Known (Data'Access, Feature) then
               --
               -- A  node  testing the feature can be generated, because
               -- the feature has a defined value in the training set.
               --
               declare
                  Children : Node_Ptr_Array (1..Feature.Cardinality);
                  Created  : Boolean;
               begin
                  if Is_Controlling (Feature, Data.Features) then
                     Create_Branches
                     (  Data,
                        Children,
                        Weight,
                        Feature,
                        Separators,
                        Created
                     );
                  else
                     Create_Branches_Optimized
                     (  Data,
                        Children,
                        Weight,
                        Feature,
                        Separators,
                        Created
                     );
                  end if;
                  if Created then
                     --
                     -- There exists at least one child
                     --
                     Node := Create (Data.Factory, Feature, Children);
                     for Index in Children'Range loop
                        Release (Children (Index));
                     end loop;
                     Data.Factory := Get_Factory (Node.all);
                     if (  Data.Equivalence < Confidence'Last
                        and then
                           not Is_Clustering (Feature)
                        )
                     then
                        Add (Separators.Separators.List, Feature.Self);
                     end if;
                     Add (Data.Features, This);
                     Data.Depth := Data.Depth - 1;
                     return;
                  end if;
               exception
                  when others =>
                     for Index in Children'Range loop
                        Release (Children (Index));
                     end loop;
                     raise;
               end;
            end if;
            Learn_New (Node, Data, Weight, Separators);
            Add (Data.Features, This);
            Data.Depth := Data.Depth - 1;
         exception
            when others =>
               Add (Data.Features, This);
               Data.Depth := Data.Depth - 1;
               raise;
         end;
      end if;
   end Learn_New;

   procedure Learn_Old
             (  Node       : in out Node_Proxy;
                Data       : in out Graph_Training_Data'Class;
                Weight     : Confidence;
                Separators : in out Separation_Data
             )  is
   begin
      case Get_Type (Ptr (Node).all) is
         when Tree_Leaf =>
            --
            -- Leaf  node.  All  features  to  test  should  have   been
            -- processed before this.
            --
            if Is_Empty (Data.Features) then
               --
               -- No more features left to test. The leaf node has to be
               -- modified, because it seems that there were no features
               -- having known values before it. The  leaf  distribution
               -- is changed and the resulting node is returned.
               --
               declare
                  Distribution : Classification
		                    (Get_Cardinality (Data.Classes));
                  New_Node : Graph_Node_Ptr;
               begin
                  Get_Leaf (Data, Weight, Distribution);
                  if Combine_With_Or then
                     Modify
                     (  Ptr (Node).all,
                        Data,
                        Distribution,
                        Node.Exclusive,
                        Union,
                        New_Node
                     );
                  else
                     Modify
                     (  Ptr (Node).all,
                        Data,
                        Distribution,
                        Node.Exclusive,
                        Intersection,
                        New_Node
                     );
                  end if;
                  if New_Node /= null then
                     Node := Ref (New_Node);
                  end if;
               exception
                  when others =>
                     if (  New_Node /= null
                        and then
                           New_Node.Use_Count = 0
                        )
                     then
                        Free (New_Node);
                     end if;
                     raise;
               end;
            else
               --
               -- The  list of features is not empty. The first feature
               -- from the list is taken.
               --
               declare
                  This    : constant Feature_Statistics_Ptr :=
                               Get (Data.Features, 1);
                  Feature : Feature_Object'Class renames
                               Ptr (This.Feature).all;
               begin
                  Data.Depth := Data.Depth + 1;
                  Remove (Data.Features, 1);
                  if Is_Known (Data'Access, Feature) then
                     --
                     -- A  new  node  has to be created, which will test
                     -- the  current feature in a way that all arcs will
                     -- lead to the current node. Then  this  node  gets
                     -- learnt on.
                     --
                     declare
                        Parent : constant Graph_Node_Ptr :=
                           Create
                           (  Get_Factory (Ptr (Node).all),
                              Feature,
                              Node_Ptr_Array'
                                 (1..Feature.Cardinality => Ptr (Node))
	                     );
                     begin
                        Node := Ref (Parent);
                        Learn_Old (Node, Data, Weight, Separators);
                     end;
                  else
                     --
                     -- Ignore the feature
                     --
                     Learn_Old (Node, Data, Weight, Separators);
                  end if;
                  Add (Data.Features, This);
                  Data.Depth := Data.Depth - 1;
               exception
                  when others =>
                     Add (Data.Features, This);
                     Data.Depth := Data.Depth - 1;
                     raise;
               end;
            end if;
         when Tree_Cluster =>
            --
            -- Cluster  node. All non-clustering features to test should
            -- have been dealt with before.
            --
            if (  Is_Empty (Data.Features)
               or else
                  Is_Clustering (Get (Data.Features, 1).Feature)
               )
            then
               --
               -- No more features left to test. The clustering node can
               -- be modified.
               --
               declare
                  Feature    : Feature_Object'Class renames
   	                          Get_Feature (Ptr (Node).all).all;
                  This       : constant Feature_Statistics_Ptr :=
   	                          Get_Statistics
   		                  (  Data'Unchecked_Access,
                                     Feature
                                  );
                  Candidates : constant Separation_Hypotheses_Ptr :=
		                  Allocate (Data.Cache'Access);
                  Branch     : Separation_Data
	                       (  Feature.Cardinality,
                                  Candidates
                               );
               begin
                  Data.Depth := Data.Depth + 1;
                  Remove (Data.Features, This);
                  Modify (Branch, Node);
                  if Is_Controlling (Feature, Data.Features) then
                     declare
                        Value : Confidence;
                        Sup   : Confidence := Confidence'First;
                     begin
                        for Index in 1..Feature.Cardinality loop
                           Value :=
                              Get_Point (Data'Access, Feature, Index);
                           Sup   := Sup or Value;
                           Value := Value and Weight;
                           if Value > Data.Threshold then
                              Branch.Current := Index;
                              Modify (Data, Branch, Feature, Value);
                           end if;
                        end loop;
                        if Sup < Confidence'Last then
                           Add_Example (Data, Feature);
                        end if;
                     end;
                  else
                     --
                     -- For each feature domain point, which  confidence
                     -- of  is  greater  than  the  threshold, Modify is
                     -- called.
                     --
                     declare
                        Value : Fuzzy.Set (1..Feature.Cardinality);
                     begin
                        Get (Data, Feature, Weight, Value);
                        for Index in Value'Range loop
                           if Value (Index) > Data.Threshold then
                              Branch.Current := Index;
                              Modify_Optimized (Data, Branch, Value);
                           end if;
                        end loop;
                     end;
                  end if;
                  Commit (Data, Branch.Node);
                  Set (Node, Ptr (Branch.Node));
                  Free (Data.Cache, Candidates); -- Ignore any candidates
                  Add (Data.Features, This);
                  Data.Depth := Data.Depth - 1;
               exception
                  when others =>
                     Free (Data.Cache, Candidates);
                     Add (Data.Features, This);
                     Data.Depth := Data.Depth - 1;
                     raise;
               end;
            else
               --
               -- The list of features is not empty. The  first  feature
               -- from  the  list  is  taken  to  make sure that it will
               -- precede one of the clustering node.
               --
               declare
                  This    : constant Feature_Statistics_Ptr :=
                               Get (Data.Features, 1);
                  Feature : Feature_Object'Class renames
                               Ptr (This.Feature).all;
               begin
                  Data.Depth := Data.Depth + 1;
                  Remove (Data.Features, 1);
                  if Is_Known (Data'Access, Feature) then
                     --
                     -- A  new  node  has to be created, which will test
                     -- the  current feature in a way that all arcs will
                     -- lead to the current node. Then  this  node  gets
                     -- learnt on.
                     --
                     declare
                        Parent : constant Graph_Node_Ptr :=
                           Create
                           (  Get_Factory (Ptr (Node).all),
                              Feature,
                              Node_Ptr_Array'
                                 (1..Feature.Cardinality => Ptr (Node))
                           );
                     begin
                        Node := Ref (Parent);
                        Learn_Old (Node, Data, Weight, Separators);
                     end;
                  else
                     --
                     -- Ignore the feature
                     --
                     Learn_Old (Node, Data, Weight, Separators);
                  end if;
                  Add (Data.Features, This);
                  Data.Depth := Data.Depth - 1;
               exception
                  when others =>
                     Add (Data.Features, This);
                     Data.Depth := Data.Depth - 1;
                     raise;
               end;
            end if;
         when Tree_Branch =>
            --
            -- Here is a branch node, the  feature  and  its  order  are
            -- determined,  then the feature is removed from the list of
            -- and the branch node is modified.
            --
            declare
               Feature    : Feature_Object'Class renames
			       Get_Feature (Ptr (Node).all).all;
               This       : constant Feature_Statistics_Ptr :=
	                       Get_Statistics
                               (  Data'Unchecked_Access,
                                  Feature
                               );
               Candidates : constant Separation_Hypotheses_Ptr :=
			       Allocate (Data.Cache'Access);
               Branch     : Separation_Data
	                    (  Feature.Cardinality,
                               Candidates
                            );
            begin
               Data.Depth := Data.Depth + 1;
               Remove (Data.Features, This);
               Modify (Branch, Node);
               if Is_Controlling (Feature, Data.Features) then
                  declare
                     Value : Confidence;
                     Sup   : Confidence := Confidence'First;
                  begin
                     for Index in 1..Feature.Cardinality loop
                        Value :=
                           Get_Point (Data'Access, Feature, Index);
                        Sup   := Sup or Value;
                        Value := Value and Weight;
                        if Value > Data.Threshold then
                           Branch.Current := Index;
                           Modify (Data, Branch, Feature, Value);
                        end if;
                     end loop;
                     if Sup < Confidence'Last then
                        Add_Example (Data, Feature);
                     end if;
                  end;
               else
                  --
                  -- For  each feature domain point, confidence of which
                  -- is greater than the threshold, Modify is called.
                  --
                  declare
                     Value : Fuzzy.Set (1..Feature.Cardinality);
                  begin
                     Get (Data, Feature, Weight, Value);
                     for Index in Value'Range loop
                        if Value (Index) > Data.Threshold then
                           Branch.Current := Index;
                           Modify_Optimized (Data, Branch, Value);
                        end if;
                     end loop;
                  end;
               end if;
               Commit (Data, Branch.Node);
               if Data.Equivalence < Confidence'Last then
                  --
                  -- Check all separation hypotheses, rotate the node as
                  -- necessary, and report the leading hypotheses to the
                  -- parent.
                  --
                  Commit (Data, Separators, Branch);
               end if;
               Set (Node, Ptr (Branch.Node));
               Free (Data.Cache, Candidates);
               Add (Data.Features, This);
               Data.Depth := Data.Depth - 1;
            exception
               when others =>
                  Free (Data.Cache, Candidates);
                  Add (Data.Features, This);
                  Data.Depth := Data.Depth - 1;
                  raise;
            end;
      end case;
   end Learn_Old;

   procedure Update
             (  Node : in out Node_Handle;
                Data : in out Graph_Training_Data'Class
             )  is
      This       : Graph_Node_Ptr := Ptr (Node);
      Candidates : constant Separation_Hypotheses_Ptr :=
                      Allocate (Data.Cache'Access);
      Separators : Separation_Data (1, Candidates);
   begin
      if This = null then
         Learn_New (This, Data, Confidence'Last, Separators);
         if This /= null then
            Node := Ref (This);
         end if;
      elsif (  Data.Example_Cut > Confidence'First
            or else
               (  Current_Image /= Has_Not_Out
               and then
                  Current_Image /= Has_Out
            )  )  then
         declare
            Updated : Node_Proxy := Ref (This);
         begin
            Updated.Exclusive := This.Use_Count = 2;
            Learn_Old (Updated, Data, Confidence'Last, Separators);
            Node := Ref (Ptr (Updated));
         end;
      end if;
      Free (Data.Cache, Candidates);
   exception
      when others =>
         Free (Data.Cache, Candidates);
         raise;
   end Update;

end Fuzzy.Graph.Learning.Implementation;
