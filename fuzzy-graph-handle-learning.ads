--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Graph.Handle.Learning                 Luebeck            --
--  Interface                                      Spring, 2002       --
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
--  This package provides subprograms for learning from training sets.
--
with Fuzzy.Graph.Learning;  use Fuzzy.Graph.Learning;

with Fuzzy.Graph.Memory_Resident;

package Fuzzy.Graph.Handle.Learning is
   pragma Elaborate_Body (Fuzzy.Graph.Handle.Learning);
--
-- Learn -- Learn from a set of examples
--
--    Lesson      - The data context or a training set handle
--    Features    - The sequence of features to test
--    Classes     - The class feature
--    Image       - The images to learn from
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of nodes in terms of quality
--    Viewer      - A progress indication object or a handle to
--    Factory     - The defaut node factory
--
-- This  function builds a graph from the fuzzy training set Lesson. The
-- parameters  From,  To specify the range of training examples used for
-- learning.  Usually  it  is  the  whole  set.  The parameter To can be
-- greater  than  the  total  number  of  examples.  The parameter Image
-- indicates  the  image  type  to  learn  from.  The parameter Features
-- specifies  the sequence of features. The nodes of the graph will test
-- the features in the order specified by the sequence. The order has  a
-- substantial  influence  on  the resulting graph. The list may contain
-- No_Feature  elements,  which  are ignored. The parameter Classes is a
-- feature  describing  the set of classes the graph should classify to.
-- It must be a valid handle otherwise Constraint_Error  is  propagated.
-- The  parameter  Threshold  specifies  the   truncation   level.   All
-- confidence  factors  below  Threshold are treated as 0. The parameter
-- Equivalence is the Level of separation  hypotheses  quality.  If  the
-- qualities differ less than Equivalence, they are assumed  same.  When
-- Equivalence  is  Confidence'Last,  fatures  are not selected, so they
-- order  of  testing is not changed unless some features are undefined.
-- The  parameter  Factory specifies the node factory used by default to
-- create  new  nodes.  Usually it is the first node created. The result
-- might be an empty graph in which case an invalid handle is  returned.
-- Viewer can be an invalid handle.
--
-- Returns :
--
--    The built graph (a handle to the root node)
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or node
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--
   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               Image       : Image_Type;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle;
   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               Image       : Image_Type;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Node_Handle;
--
-- Learn -- Learn incrementally from a set of examples
--
--    Node        - A graph to modify (a handle to)
--    Lesson      - The data context or a teaching set handle
--    Features    - The sequence of features to test
--    Image       - The images to learn from
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of nodes in terms of quality
--    Viewer      - A progress indication object or a handle to
--
-- This  procedure  modifies  a graph according to new training examples
-- from  the  training  set  Lesson. The parameter Node is a node of the
-- graph.  It can be any node of it. The parameters From, To specify the
-- range of training examples used for learning. Usually it is the whole
-- set. The parameter To  can  be  greater  than  the  total  number  of
-- examples. The parameter Image specifies the image to learn from.  The
-- parameter Features specifies the sequence of features. The  list  may
-- contain No_Feature elements, which are ignored. The sequence  may  be
-- empty. The parameter Threshold specifies the  truncation  level.  All
-- confidence  factors  below  Threshold are treated as 0. The parameter
-- Equivalence is the Level of separation  hypotheses  quality.  If  the
-- qualities differ less than Equivalence, they are assumed  same.  When
-- Equivalence  is  Confidence'Last,  features are not selected, so they
-- order of testing is not changed, unless some features are  undefined.
-- Viewer can be an invalid handle.
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or graph
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Features would depend on the node
--
-- Notes :
--
--    Not all images are compatible in incremental learning. It is  only
--    possible  to mix Has_In with Has_Out and Has_Not with Has_Not_Out.
--    In  each pair is reasonable to start learning with the first image
--    and attune the result with learning on the second image.
--
   procedure Learn
             (  Node        : in out Node_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                Image       : Image_Type;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             );
   procedure Learn
             (  Node        : in out Node_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                Image       : Image_Type;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             );
--
-- Learn -- From training data
--
--    Node    - A graph to modify (a handle to)
--    Context - The training data
--    Image   - To learn from
--
-- This  procedure  modifies a graph Node according to teaching data. If
-- the node does not exist it will be created.
--
-- Exceptions :
--
--    Constraint_Error - An invalid graph
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Circular dependency encountered
--
   procedure Learn
             (  Node    : in out Node_Handle;
                Context : in out Graph_Training_Data'Class;
                Image   : Image_Type
             );
end Fuzzy.Graph.Handle.Learning;
