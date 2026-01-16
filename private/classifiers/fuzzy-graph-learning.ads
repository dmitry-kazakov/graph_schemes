--                                                                    --
--  package Fuzzy.Graph.Learning    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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

with Fuzzy.Feature.Handle;    use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Separators;  use Fuzzy.Graph.Separators;
with Fuzzy.Lecture;           use Fuzzy.Lecture;
with Fuzzy.Samples_Lists;     use Fuzzy.Samples_Lists;

with Fuzzy.Feature.Comparisons;
with Fuzzy.Feature.Handle.Bounded_Arrays;
with Fuzzy.Feature.Sets;
with Generic_Map;
with Generic_Set;

package Fuzzy.Graph.Learning is
   pragma Elaborate_Body (Fuzzy.Graph.Learning);

   type Separation_Hypotheses_Cache is new Quality_Cache with private;
   type Separation_Data (<>) is limited private;
--
-- Feature_Statistics -- Accumulated feature statistics
--
--    Feature   - A handle to (invalid if not trained on)
--    Up        - Number of upwards rotations made for the feature
--    Down      - Number of downwards rotations made for the feature
--    Depth     - Sum of graph depths at which the rotations were made
--    Order     - Of the feature in the training list
--    Erroneous - The list of inconsistent examples found
--
-- Items with higher order come in the list first.
--
   type Feature_Statistics is new Object.Entity with record
      Feature   : Feature_Handle;
      Up        : Natural    := 0;
      Down      : Natural    := 0;
      Depth     : Float      := 0.0;
      Order     : Divergence := 0.0;
      Erroneous : Examples_List;
   end record;
   type Feature_Statistics_Ptr is access Feature_Statistics'Class;
   function "<" (Left, Right : Feature_Statistics_Ptr) return Boolean;
   pragma Inline ("<");
--
-- Feature_Statistics_Handles -- Handles to feature statistics
--
   package Feature_Statistics_Handles is
      new Object.Handle (Feature_Statistics, Feature_Statistics_Ptr);
--
-- Feature_Statistics_Handle -- A handle to feature statistics
--
   type Feature_Statistics_Handle is
      new Feature_Statistics_Handles.Handle with null record;
--
-- Ref -- Overrides Object.Handle...
--
   function Ref (Statistics : Feature_Statistics_Ptr)
      return Feature_Statistics_Handle;
--
-- Feature_Statistics_Maps -- Feature to statistics map
--
   package Feature_Statistics_Maps is
      new Generic_Map
          (  Key_Type    => Feature_Object_Ptr,
             Object_Type => Feature_Statistics_Handle,
             "<"         => Fuzzy.Feature.Comparisons.Less
          );
--
-- Features_Lists -- Features list
--
   package Features_Lists is
      new Generic_Set
          (  Object_Type  => Feature_Statistics_Ptr,
             Null_Element => null
          );
--
-- Graph_Image_Training_Statistics -- Statistics  of training  an  image
--                                    of a graph
--
   type Graph_Image_Training_Statistics is record
      Total_Time   : Duration := 0.0;
      Current_Time : Duration := 0.0;
   end record;
   type Graph_Training_Statistics is
      array (Image_Type) of Graph_Image_Training_Statistics;
--
-- Training_Statistics -- Statistics of training
--
   type Training_Statistics is record
      Data : Graph_Training_Statistics;
   end record;
--
-- Graph_Training_Data -- Information used for training
--
--    Lesson      - The training set
--    Length      - Training set name length
--    Cardinality - Of the class feature
--    Viewer      - An indication object
--
-- This  structure  is  used  during learning too keep training data and
-- parameters. The meaning of some fields:
--
--    Cache       - The separation hypotheses cache;
--    Classes     - The class feature.
--    Class_Cut   - The   possibility   of  the  current  class  or  the
--                  possibility  of  the  current  class complement (see
--                  Pro).  This  field is ignored when not learning from
--                  Has_Out or Has_Not_Out.
--    Depth       - The depth of the current node  being  modified.  The
--                  root node has the depth 0.  This  field  is  changed
--                  during training as the graph is navigated.
--    Example_Cut - An  estimation  of  N(r|r),  the  necessity  of  the
--                  current  example under the condition of itself. Here
--                  r is the current example. This field is ignored when
--                  not learning from Has_Out or Has_Not_Out.
--    Features    - The features the graph will possibly  test.  Usually
--                  this is the set of the features used in the training
--                  set.  During  training this set does not contain the
--                  features tested by the parent nodes and one  of  the
--                  currently modified node.
--    Pro         - True, when the field Class_Cut is the possibility of
--                  the current  class.  False  when  Class_Cut  is  the
--                  possibility  of the complement of the current class.
--                  This  field  is ignored when training from Has_In or
--                  Has_Not.
--    Rotations   - The total number of rotations made during training.
--    Selected    - The   feature   selected  for  training.  All  other
--                  features are assumed to  have  images  all  1.  This
--                  field is  ignored  when  training  from  Has_In  and
--                  Has_Out images.
--    Statistics  - The statistics gathered about features.
--
   type Graph_Training_Data
        (  Lesson      : not null access Lecture_Object'Class;
           Length      : Natural;
           Cardinality : Positive;
           Viewer      : not null access Indicator_Object'Class
        )  is new Node_Modification_Data (Lesson, Length, Viewer) with
   record
      Selected    : Feature_Handle;
      Class_Cut   : Confidence;
      Example_Cut : Confidence;
      Pro         : Boolean;
      Features    : Features_Lists.Set;
      Classes     : Feature_Handle;
      Statistics  : Feature_Statistics_Maps.Map;
      Depth       : Natural := 0;
      Rotations   : Natural := 0;
      Cache       : aliased Separation_Hypotheses_Cache (Cardinality);
      Profiling   : Training_Statistics;
   end record;
   type Graph_Training_Data_Ptr is access Graph_Training_Data'Class;
--
-- Add_Example -- To the list of inconsistent examples
--
--    Data    - The training data
--    Feature - The feature
--
-- This  procedure  is  used  to  register  the   current   example   as
-- inconsistent  in Feature. It is used for diagnostic purposes. Feature
-- can be either a pointer or a handle.
--
-- Exceptions :
--
--    Constraint_Error - Invalid pointer or handle
--
   procedure Add_Example
             (  Data    : in out Graph_Training_Data;
                Feature : Feature_Handle
             );
   procedure Add_Example
             (  Data    : in out Graph_Training_Data;
                Feature : Feature_Object'Class
             );
   pragma Inline (Add_Example);
--
-- Equal -- Compare two domain points in a set
--
--    Value - The set
--    X     - The index of the first point
--    Y     - The index of another
--
-- Returns :
--
--    True if both points are same
--
   function Equal (Value : Fuzzy.Set; X, Y : Positive)
      return Boolean;
   pragma Inline (Equal);
--
-- Free -- Deallocation of training data
--
--    Data - A pointer to
--
   procedure Free (Data : in out Graph_Training_Data_Ptr);
   pragma Inline (Free);
--
-- Get_Statistics -- Associated with a feature
--
--    Data    - The training data
--    Feature - The feature (a handle to)
--
-- Returns :
--
--    The statistics object associated with or an invalid handle
--
   function Get_Statistics
            (  Data    : Graph_Training_Data;
               Feature : Feature_Handle
            )  return Feature_Statistics_Handle'Class;
--
-- Set_Features -- Set features into a graph training data object
--
--    Context  - The training data context
--    Features - The sequence of features
--
   procedure Set_Features
             (  Context  : in out Graph_Training_Data;
                Features : Bounded_Arrays.Bounded_Array
             );
   procedure Set_Features
             (  Context  : in out Graph_Training_Data;
                Features : Feature_Array
             );
--
-- Graph_Training_Indication_Data -- An instance of this type is  passed
--                                   to  the indicator object upon start
-- of training using a call to Set_Data. The indicator may use the  Data
-- field for a more detailed indication of training process.
--
   type Graph_Training_Indication_Data
        (  Data : not null access Graph_Training_Data'Class
        )  is new Indicator_Data with null record;

private
--
-- Separation_Hypotheses -- Set of features
--
-- The set is linked in a chained list. When released it is added to the
-- list of free sets in the Separation_Hypotheses_Cache. When  requested
-- it is taken from that list or else indeed allocated new.
--
   type Separation_Hypotheses;
   type Separation_Hypotheses_Ptr is access Separation_Hypotheses;
   type Separation_Hypotheses is record
      List : Fuzzy.Feature.Sets.Set;
      Next : Separation_Hypotheses_Ptr;
   end record;
--
-- Separation_Hypotheses_Cache -- Cache
--
-- The cache accumulates node  quality  estimations,  and  the  list  of
-- unused separation hypotheses.
--
   type Separation_Hypotheses_Cache is new Quality_Cache with record
      Free    : Separation_Hypotheses_Ptr;
      Scratch : Fuzzy.Feature.Sets.Set;
   end record;
--
-- Finalize -- Destructor
--
--    Cache - To be destructed
--
   procedure Finalize (Cache : in out Separation_Hypotheses_Cache);
--
-- Allocate -- New separation hypothesis
--
--    Cache - To allocate hypothesis set in
--
-- It is either taken from Cache or allocated new.
--
-- Returns :
--
--    A pointer to
--
   function Allocate
            (  Cache : not null access Separation_Hypotheses_Cache
            )  return Separation_Hypotheses_Ptr;
   pragma Inline (Allocate);
--
-- Free -- New separation hypothesis
--
--    Cache - To return hypothesis set into
--    List  - A pointer to the hypothesis set
--
   procedure Free
             (  Cache : in out Separation_Hypotheses_Cache;
                List  : Separation_Hypotheses_Ptr
             );
   pragma Inline (Free);
   package Graph_Training_Data_Handles is
      new Object.Handle (Graph_Training_Data, Graph_Training_Data_Ptr);
--
-- Node_Proxy -- Node proxy
--
--    Exclusive - The proxy has exclusive access to the node.  It  means
--                that all other handles to the node will be notified by
--                the  owner  of  the  proxy in case of node change. The
--                modifications  of  the  node are applied in-place when
--                Exclusive is True.
--
   type Node_Proxy is new Handles.Handle with record
      Exclusive : Boolean;
   end record;
--
-- Ref -- Overrides Object.Handle...
--
   function Ref (Node : Graph_Node_Ptr) return Node_Proxy;
--
-- Branch_Proxy -- Branch node proxy
--
--    Cardinality - The node cardinality
--    Children    - The children connected
--
-- Branch  nodes  when  modified are used through proxy handles, because
-- some  primitive  node operations cannot be efficiently implemented in
-- an  atomic  way. Branch_Proxy is used to esure graph consistency. The
-- primitive operations of Branch_Proxy are translated into ones defined
-- on  a  graph  node.  When  modification  of  a  node  referenced by a
-- Branch_Proxy is completed Commit is called to synchronize it with the
-- graph.
--
   type Branch_Proxy (Cardinality : Positive) is
      new Node_Proxy with
   record
      Children : Node_Ptr_Array (1..Cardinality);
   end record;
--
-- Adjust -- Copy constructor (a part of)
--
   procedure Adjust (Node : in out Branch_Proxy);
--
-- Commit -- Commit node changes
--
--    Data - Training data
--    Node - Node proxy handle
--
-- When  a node proxy is committed it applies the all its changes to the
-- graph. In particular, if the modified node is equivalent to any other
-- graph  node, the proxy will switch itself to refer that node instead.
--
   procedure Commit
             (  Data : in out Graph_Training_Data'Class;
                Node : in out Branch_Proxy
             );
--
-- Connect -- A child node to parent
--
--    Parent - To be modified, a proxy handle to
--    Child  - The child node to connect
--    Index  - The connection point
--
-- This  procedure  is  used to queue modification of a branch node. The
-- changes made are committed when Commit is called. Note that it is not
-- checked  whether  the  child  node  is  an  ancestor of Parent. It is
-- caller's  responsibility  to  ensure  that no circular dependency may
-- happen. Constraint_Error is propagated when Parent does not  refer  a
-- branch  node or Index is illegal. It is allowed not to check if Child
-- belongs to another graph. That check can be postponed until Commit.
--
-- Exceptions :
--
--    Constraint_Error - Illegal index, invalid node
--
   procedure Connect
             (  Parent : in out Branch_Proxy;
                Child  : Graph_Node_Ptr;
                Index  : Positive
             );
--
-- Finalize -- Destruction
--
   procedure Finalize (Node : in out Branch_Proxy);
--
-- Ref -- Overrides Object.Handle...
--
   function Ref (Node : Graph_Node_Ptr) return Branch_Proxy;
--
-- Separation_Data -- Concerning separators of a modified node
--
--    Separators - Separation hypotheses list
--    Current    - The set of verified arcs is 1..Current - 1
--    Splitter   - True if at least one new child node was created
--
   type Separation_Data
        (  Cardinality : Positive;
           Separators  : not null access Separation_Hypotheses
        )  is limited
   record
      Node     : Branch_Proxy (Cardinality);
      Current  : Positive := 1;
      Splitter : Boolean  := False;
   end record;
--
-- Commit -- Separation hypotheses
--
--    Data   - The training data
--    Parent - To commit the separation hypotheses
--    Child  - The current node (a proxy to)
--
-- The  hypothesis  returned  from  the  child  node  To + 1 are checked
-- against in the children 1..To and  are  added  to  the  current  list
-- specified by Child. The list hypotheses in Child becomes empty.  Note
-- that Child must have been committed before this procedure is  called.
-- Otherwise all changes made will be lost.
--
   procedure Commit
             (  Data   : in out Graph_Training_Data'Class;
                Parent : in out Separation_Data;
                Child  : in out Separation_Data
             );
--
-- Modify -- Start modifying node
--
--    Data - Separation data
--    Node - To modify
--
-- This procedure makes Data refer to Node.
--
-- Exceptions :
--
--    Contraint_Error - Wrong handle, node, incompatible cardinality
--
   procedure Modify
             (  Data : in out Separation_Data;
                Node : Node_Proxy'Class
             );
--
-- Get_Statistics -- Associated with a feature
--
--    Data    - The training data
--    Feature - The feature, it must be one of the graph
--
-- Returns :
--
--    The statistics object associated with (a pointer to)
--
   function Get_Statistics
            (  Data    : not null access Graph_Training_Data'Class;
               Feature : Feature_Object'Class
            )  return Feature_Statistics_Ptr;
--
-- Data_Handles -- To training data
--
   package Data_Handles is
      new Object.Handle (Graph_Training_Data, Graph_Training_Data_Ptr);

end Fuzzy.Graph.Learning;
