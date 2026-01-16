--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Learning.Implementation         Luebeck            --
--  Interface                                      Spring, 2002       --
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
--
--  This  generic  package  provides  an  implementation  learning.  Its
--  generic formal parameters:
--
--  (o)  Combine_With_Or  determines  how  the  teaching  examples   are
--       combined.  When  learning  from  possibilities it is "or", when
--       learning from necessities it is "and";
--
--  (o)  Get  is  used  to get a feature value from the teaching example
--       data. The parameter Weight determines the confidence level used
--       to cut the data;
--
--  (o)  Get_Leaf  is  used  to get the classification from the training
--       example data;
--
--  (o)  Get_Point is similar to Get but used for singular points;
--
--  (o)  Is_Known  returns  true  if  the  teaching  example defines the
--       specified feature.
--
with Fuzzy.Graph.Handle;  use Fuzzy.Graph.Handle;

generic
   with procedure Get
                  (  Data    : in out Graph_Training_Data'Class;
                     Feature : Feature_Object'Class;
                     Weight  : Confidence;
                     Result  : in out Fuzzy.Set
                  );
   with procedure Get_Leaf
                  (  Data         : in out Graph_Training_Data'Class;
                     Weight       : Confidence;
                     Distribution : in out Classification
                  );
   with function Get_Point
                 (  Data    : not null access Graph_Training_Data'Class;
                    Feature : Feature_Object'Class;
                    Value   : Positive
                 )  return Confidence;
   with function Is_Known
                 (  Data    : not null access Graph_Training_Data'Class;
                    Feature : Feature_Object'Class
                 )  return Boolean;
   Combine_With_Or : Boolean;
   Current_Image   : Image_Type;
package Fuzzy.Graph.Learning.Implementation is
--
-- Update -- Create / modify a graph from one example
--
--    Node - To be modified (a handle to, maybe invalid)
--    Data - The training data
--
-- This procedure creates or updates a graph node specified by a handle.
-- For  an invalid handle it creates a new node. It uses a learning from
-- possibilities (has-in or has-not). The parameter Separator receives a
-- handle to the separation hypothesis deduced while updating. It can be
-- an invalid handle if no hypothesis was deduced.
--
   procedure Update
             (  Node : in out Node_Handle;
                Data : in out Graph_Training_Data'Class
             );
private
--
-- Learn_Old -- Update a graph with one additional example
--
--    Node       - The root node of the graph to be updated
--    Data       - The training data
--    Weight     - Of the teaching example
--    Separators - Separation hypotheses
--
-- The updated node is either modified or replaced completely with a new
-- one. The parameter Separators is a  map  of  features  to  separation
-- hypotheses.  The  hypotheses  deduced while updating are added to the
-- map.
--
   procedure Learn_Old
             (  Node       : in out Node_Proxy;
                Data       : in out Graph_Training_Data'Class;
                Weight     : Confidence;
                Separators : in out Separation_Data
             );
--
-- Learn_New -- Create a graph from one example
--
--    Node       - The result
--    Data       - The training data
--    Weight     - Of the example
--    Separators - Separation hypotheses
--
-- Node is a pointer to the root node of the created graph or null.
--
   procedure Learn_New
             (  Node       : in out Graph_Node_Ptr;
                Data       : in out Graph_Training_Data'Class;
                Weight     : Confidence;
                Separators : in out Separation_Data
             );
end Fuzzy.Graph.Learning.Implementation;
