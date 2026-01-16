--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Manual                   Luebeck            --
--  Interface                                      Autumn, 2002       --
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
--
--  Usually graph nodes are created in  the  result  of  learning.  This
--  package provides subroutines to create graph-schemes  manually  node
--  by node. 
--
with Fuzzy.Graph.Memory_Resident;
with Fuzzy.Graph.Handle.Bounded_Arrays;
use  Fuzzy.Graph.Handle.Bounded_Arrays;

package Fuzzy.Graph.Handle.Manual is
   pragma Elaborate_Body (Fuzzy.Graph.Handle.Manual);
--
-- Create_Branch -- Create a branch node
--
--    Feature  - The handle to the feature the node will test
--    Children - The list of arcs leading out of the node
--
-- These  functions create a branch node. The node will test the feature
-- specified by the parameter Feature. For each valid node handle of the
-- array Children it will have an arc leading  to  this  node.  Children
-- shall have the cardinality of the feature. When Children contains  no
-- nodes then Constraint_Error is propagated. The new node  will  be  of
-- the  graph  of  its children. All children nodes should belong to the
-- same  graph,  else Constraint_Error is propagated. It also propagated
-- if Feature is invalid or Children's  length  differs  from  Feature's
-- cardinality. 
--
-- Returns :
--
--    The handle to the node
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, children of different graphs
--
   function Create_Branch
            (  Feature  : Feature_Handle;
               Children : Node_Array
            )  return Node_Handle;
   function Create_Branch
            (  Feature  : Feature_Handle;
               Children : Bounded_Array
            )  return Node_Handle;
--
-- Create_Leaf -- Create a leaf node
--
--    Feature        - The handle to the feature the node will test
--    Distribution   - The classification given by the leaf
--    Factory / Node - The node factory used to create the node
--
-- These fuctions create a leaf (terminal) node. The  parameter  Feature
-- is the class-feature of  which  Distribution  is  the  classification
-- provided  by  the node. The cardinality of the feature has to be same
-- as  one  of  the  classification.  The  parameter Factory is the node
-- factory. Alternatively one can specify any node of  the  graph  where
-- the  leaf  should be created. So when several leafs of the same graph
-- are created it can be done as follows: 
--
--    Leaf_1 := Create (Class, D_1); -- This creates a new graph
--    Leaf_2 := Create (Class, D_2, Leaf_1);
--    Leaf_3 := Create (Class, D_3, Leaf_1);
--           ...
--
-- Returns :
--
--    The handle to the node
--
-- Exceptions :
--
--    Constraint_Error - Invalid feature handle or Distribution
--
   function Create_Leaf
            (  Feature      : Feature_Handle;
               Distribution : Classification;
               Factory      : not null access Node_Factory'Class :=
                                 Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle;
   function Create_Leaf
            (  Feature      : Feature_Handle;
               Distribution : Classification;
               Node         : Node_Handle
            )  return Node_Handle;

end Fuzzy.Graph.Handle.Manual;
