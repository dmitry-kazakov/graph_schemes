--                                                                    --
--  package Fuzzy.Graph.Separators  Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2006       --
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

with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Fuzzy.Logic;           use Fuzzy.Logic;

with Fuzzy.Feature.Comparisons;
with Fuzzy.Graph.Comparisons;
with Generic_Map;
with Generic_Segmented_Stack;
with Stack_Storage;
with System.Storage_Elements;

package Fuzzy.Graph.Separators is
   pragma Elaborate_Body (Fuzzy.Graph.Separators);
--
-- Quality_Comparison -- Node separation quality
--
   type Quality_Comparison is (Lower, Same, Higher);
   Zero : constant Divergence_Range := (0.0, 0.0);
   One  : constant Divergence_Range := (1.0, 1.0);
--
-- Compare -- Two qualities
--
--    Left      - A quality value
--    Right     - Another quality value to compare
--    Threshold - Equivalence threshold
--
-- Returns :
--
--    The comparison result
--
   function Compare
            (  Left      : Divergence_Range;
               Right     : Divergence_Range;
               Threshold : Confidence
            )  return Quality_Comparison;
--
-- Image -- Conversion to string
--
--    Value - To convert
--
-- Returns :
--
--    A text representation of Value
--
   function Image (Value : Divergence_Range) return String;
--
-- Quality_Cache -- Cached quality evaluation data
--
   type Quality_Cache (Cardinality : Positive) is
      new Ada.Finalization.Limited_Controlled with private;
--
-- Finalize -- Destruction
--
--    Cache - The object to finalize
--
   overriding
   procedure Finalize (Cache : in out Quality_Cache);
--
-- Get_Distribution -- Get node distribution
--
--    Node  - To get the distribution
--    Cache - The cache
--
-- This procedure returns the distribution of the classes accumulated by
-- the  node.  The  result  is  equivalent  to  a  classification of the
-- universal  set.  The procedure traverses the tree rooted in Node. For
-- this reason Cache is used to accumulate data and thus  speed  up  the
-- process.
--
-- Returns :
--
--    The distribution
--
   function Get_Distribution
            (  Node  : Graph_Node_Ptr;
               Cache : not null access Quality_Cache
            )  return Classification;
--
-- Get_Quality -- Estimate quality of a feature
--
--    Node    - The node to inspect
--    Feature - The feature considered as a separator
--    Cache   - The cache
--
-- The different values of the feature  domain  evaluate  to  independet
-- classifications of the universal set. For each given  combination  of
-- inequal  i  and j Cov (Ci|q, Cj|q) is evaluated for each domain value
-- of  the  feature.  The  maximal  value  of  these  contributes to the
-- quality.  The  paths that does not test Feature contribute to quality
-- as the minimum of.
--
-- Returns :
--
--    The quality of
--
   function Get_Quality
            (  Node    : Graph_Node_Ptr;
               Feature : Feature_Handle;
               Cache   : not null access Quality_Cache
            )  return Divergence_Range;
   function Get_Quality
            (  Node    : Graph_Node_Ptr;
               Feature : Feature_Object'Class;
               Cache   : not null access Quality_Cache
            )  return Divergence_Range;
--
-- To_Quality -- Fuzzy Boolean to quality conversion
--
--    Value - To convert
--
-- Returns :
--
--    The corresponding quality
--
   function To_Quality (Value : Fuzzy_Boolean) return Divergence_Range;

private
   pragma Inline (Get_Distribution);
   pragma Inline (Get_Quality);
   pragma Inline (To_Quality);

   use type System.Storage_Elements.Storage_Offset;
--
-- Classification_Ptr -- Pointer   type   to   a   classification.   All
--                       classifications   have   same   size   and  are
-- allocated  in  an  arena  pool  of  Quality_Cache  object. When freed
-- classifications are returned back to the cache and  the  pointers  to
-- are stored in a stack. So allocation/deallocation is fast.
--
   type Classification_Ptr is access all Classification;
   type Classification_Ptr_Array is
      array (Positive range <>) of Classification_Ptr;
--
-- Feature_Distribution -- Associated with a node/feature pair
--
--    Feature_Quality       - The quality of the feature in the node
--    Quality_Sequence      - The node sequence number of the quality
--    Distribution          - The classifications for each feature value
--    Distribution_Sequence - The node sequence number
--
-- Distribution  is  an array of pointers. The elements are allocated in
-- the arena pool of the cache.
--
   type Feature_Distribution (Cardinality : Positive) is record
      Feature_Quality       : Divergence_Range;
      Quality_Sequence      : Sequence_No;
      Distribution_Sequence : Sequence_No;
      Distribution          : Classification_Ptr_Array (1..Cardinality);
   end record;
   type Feature_Distribution_Ptr is access Feature_Distribution;
--
-- Classification_Sets -- Maps Feature to Feature_Distribution
--
   package Classification_Sets is
      new Generic_Map
          (  Key_Type    => Feature_Object_Ptr,
             Object_Type => Feature_Distribution_Ptr,
             "<"         => Fuzzy.Feature.Comparisons.Less
          );
--
-- Node_Classification -- Cached node classification
--
--    Node_Quality      - Estimated quality of
--    Quality_Sequence  - The node sequence number of the quality
--    Universe          - The classification given by the node
--    Universe_Sequence - The node sequence number of
--    Distributions     - Feature to Feature_Distribution map
--
-- It  is  derived  from  Backward_Link  to   get   notifications   upon
-- destruction of the node, for which it holds the classification.
--
   type Node_Classification
        (  Cardinality : Positive;
           Cache       : not null access Quality_Cache
        )  is new Backward_Link with
   record
      Node_Quality      : Divergence_Range;
      Quality_Sequence  : Sequence_No;
      Universe_Sequence : Sequence_No;
      Universe          : Classification_Ptr;
      Distributions     : Classification_Sets.Map;
   end record;
   type Node_Classification_Ptr is access Node_Classification;
   for Node_Classification_Ptr'Storage_Pool
      use Backward_Link_Ptr'Storage_Pool;
--
-- Finalize -- Destruction
--
   procedure Finalize (Link : in out Node_Classification);
--
-- To_Backward_Link_Ptr -- Upcast pointer conversion
--
   function To_Backward_Link_Ptr is
      new Ada.Unchecked_Conversion
          (  Node_Classification_Ptr,
             Backward_Link_Ptr
          );
--
-- Deleted -- Implements Object.Atchived...
--
   procedure Deleted
             (  Link  : in out Node_Classification;
                Temps : in out Deposit_Container'Class
             );
--
-- Destroyed -- Implements Object.Atchived...
--
   procedure Destroyed (Link : in out Node_Classification);

   package Classification_Ptr_Stacks is
      new Generic_Segmented_Stack
          (  Index_Type   => Positive,
             Object_Type  => Classification_Ptr,
             Null_Element => null
          );
   use Classification_Ptr_Stacks.Segmented_Stack;
--
-- Node_Classifications -- Index of node classifications
--
-- This instantiation gives a map of node to Node_Classification
--
   package Node_Classifications is
      new Generic_Map
          (  Key_Type    => Graph_Node_Ref,
             Object_Type => Node_Classification_Ptr,
             "<"         => Fuzzy.Graph.Comparisons.Less,
             "="         => Fuzzy.Graph.Comparisons.Equal
          );
--
-- Quality_Cache -- Implementation of
--
   type Quality_Cache (Cardinality : Positive) is
      new Ada.Finalization.Limited_Controlled with
   record
      Count : Positive := (Cardinality * (Cardinality - 1)) / 2;
      Map   : Node_Classifications.Map;
      Arena : Stack_Storage.Pool (1024*128, 2);
      Free  : Stack;
   end record;
--
-- Allocate -- A classification in the cache
--
--    Cache - To allocate a classification in
--    Ptr   - The result
--
   procedure Allocate
             (  Cache : in out Quality_Cache;
                Ptr   : out Classification_Ptr
             );
--
-- Free -- Return a classification back to the cache
--
--    Cache - To allocate a classification in
--    Ptr   - A pointer to the classification
--
-- The parameter Ptr can be null. After completion Ptr will  be  set  to
-- null.
--
   procedure Free
             (  Cache : in out Quality_Cache;
                Ptr   : in out Classification_Ptr
             );

   pragma Inline (Allocate);
   pragma Inline (Free);

end Fuzzy.Graph.Separators;
