--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Handle.Factory             Luebeck            --
--  Interface                                      Spring, 2003       --
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

with Fuzzy.Classifier.Identity;
with Fuzzy.Classifier.Separator;
with Fuzzy.Graph.Memory_Resident;
with Fuzzy.Graph.Scheme;

package Fuzzy.Classifier.Handle.Factory is
--
-- Identity -- Create an identity classifier
--
--    Classes - A handle to the class-feature
--
-- This  function  creates an identity classifier. The classifier simply
-- reports  the  value  of  the  class-feature  in  the  training as the
-- classification result.
--
-- Returns :
--
--    A handle to the created object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Identity (Classes : Feature_Handle)
      return Classifier_Handle
         renames Fuzzy.Classifier.Identity.Create;
--
-- Learn -- Learn from a set of examples
--
--    Lesson      - The training set (a handle to)
--  [ Name ]      - The external name of the training set, UTF-8
--    Features    - The sequence of features to test
--    Classes     - The class feature
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of nodes in terms of quality
--    Viewer      - A progress indication object
--    Factory     - The defaut node factory or a handle to
--
-- This function builds a classifier from the fuzzy training set Lesson.
-- The parameters From and To specify the range  of  examples  used  for
-- learning.  Usually  it  is  the  whole  set.  The parameter To can be
-- greater than the total number of  examples.  The  parameter  Features
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
-- Equivalence  is  Confidence'Last, features are not selected, so their
-- order of testing is not changed. The parameter Factory specifies  the
-- node  factory  used by default to create new nodes. Usually it is the
-- first node created. Viewer can be an invalid handle.
--
-- Returns :
--
--    The built graph-scheme (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    Data_Error       - Training set I/O error
--    End_Error        - Learning was aborted
--
   function Learn
            (  Lesson      : Lecture_Handle;
               Name        : String;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access
                             Fuzzy.Graph.Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle;
   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : Indicator_Handle;
               Factory     : not null access
                             Fuzzy.Graph.Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle;
   function Learn
            (  Lesson      : Lecture_Handle;
               Name        : String;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access
                             Fuzzy.Graph.Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle
                  renames Fuzzy.Graph.Scheme.Create;
   function Learn
            (  Lesson      : Lecture_Handle;
               Features    : Feature_Array;
               Classes     : Feature_Handle;
               From        : Positive   := 1;
               To          : Positive   := Positive'Last;
               Threshold   : Confidence := Confidence'First;
               Equivalence : Confidence := Default_Equivalence;
               Viewer      : not null access Indicator_Object'Class :=
                                Negleter'Access;
               Factory     : not null access
                             Fuzzy.Graph.Node_Factory'Class :=
                                Fuzzy.Graph.Memory_Resident.Factory
            )  return Classifier_Handle
                  renames Fuzzy.Graph.Scheme.Create;
--
-- Learn -- Learn from a set of examples
--
--    Context  - The training data context
--    Features - The sequence of features to test
--    From     - The first example to learn from
--    To       - The last example to learn from
--
-- This function builds a  graph-scheme  from  the  fuzzy  training  set
-- Lesson or one associated with Context.
--
-- Returns :
--
--    The built graph-scheme (a handle to)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or context
--    Data_Error       - Training set I/O error
--    End_Error        - Learning was aborted
--
   function Learn
            (  Context  : not null access Training_Data'Class;
               Features : Feature_Array;
               From     : Positive := 1;
               To       : Positive := Positive'Last
            )  return Classifier_Handle;
--
-- Separator -- Create a constrained classifier
--
--    Classifier   - A handle to a classifier
--    Separation   - The separation level of classes
--    Completeness - The level of classes' completenes
--
-- This  function  creates  a  classifier  that separates classes. It is
-- built upon another classifier specified by the parameter  Classifier.
-- A separator classifier uses Classifier to classify classes  known  to
-- be separate and complete to the levels specified  to  the  parameters
-- Separation   and   Completeness.  The  parameter  Separation  is  the
-- confidence level of class separation. That is the possibility of  the
-- intersection of  the  classes  or  its  estimation  from  above.  The
-- parameter Completeness is the confidence level of class completeness,
-- i.e. the necessity of class union or its estimation from  below.  The
-- built  classifier  uses the knowledge about the classes to refine the
-- classifications given by the original classifier.
--
-- Returns :
--
--    A handle to the created object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Separator
            (  Classifier   : Classifier_Handle;
               Separation   : Confidence := Confidence'First;
               Completeness : Confidence := Confidence'Last
            )  return Classifier_Handle
                  renames Fuzzy.Classifier.Separator.Create;

end Fuzzy.Classifier.Handle.Factory;
