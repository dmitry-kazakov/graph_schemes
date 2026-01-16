--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Classifier.Handle                     Luebeck            --
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

with Indicator.Handle;  use Indicator.Handle;

with Deposit_Handles;

package Fuzzy.Classifier.Handle is
   pragma Elaborate_Body (Fuzzy.Classifier.Handle);
--
-- Classifier_Handle -- A handle to graph-Classifier
--
   type Classifier_Handle is tagged private;
--
-- Classify -- Classification of an example
--
--    Classifier - A handle to
--    Lesson     - A handle to
--    Example    - The example to classify
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Complement - What to classify, example vs. its complement
--
-- This  function uses the classifier specified by the handle Classifier
-- to classify one example from Lesson. The function classifies only one
-- example from the training  set.  The  example  is  specified  by  the
-- parameter  Example.  Images  not  represented  in the set are assumed
-- unknown.   The   result   of   classification  is  an  intuitionistic
-- classification pair of distributions of over the domain  set  of  the
-- class feature (see Get_Classes). The parameter Generalize advices how
-- gaps in the training set should be filled  upon  classification.  The
-- parameter  Threshold  specifies  the truncation level. All confidence
-- factors less than Threshold  are  treated  as  0.  Higher  values  of
-- Threshold may speed up classification, though make it  less  precise.
-- The parameter Complement, when True, indicates that the complement of
-- the event behind the example has to be classified.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    Data_Error       - I/O error
--
   function Classify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification;
--
-- Classify -- Classification of an example
--
--    Classifier - A handle to
--    Context    - The data context
--    Complement - What to classify, example vs. its complement
--
-- This  function uses the classifier specified by the handle Classifier
-- to classify one example from a  fuzzy  training  set.  The  parameter
-- Context is the data context the training set is associated with.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    Data_Error       - I/O error
--
   function Classify
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification;
--
-- Estimate -- An example in terms of classes
--
--    Classifier - A handle to
--    Lesson     - A handle to
--    Example    - The example to classify
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Complement - What to classify, example vs. its complement
--
-- This function uses the classifier pointed by the handle Classifier to
-- estimate one example from  a  fuzzy  training  set.  The  example  is
-- specified by the parameter Example. Images  not  represented  in  the
-- training set are assumed unknown. The result is an intuitionistic set
-- estimating  the training examples in terms of the classes. The set of
-- classes  is  the  domain  set of the class feature (see Get_Classes).
-- TThe parameter Generalize advices how gaps in the training set should
-- be filled upon classification. he parameter Threshould specifies  the
-- truncation  level.  All  confidence  factors less than Threshould are
-- treated   as   0.   Higher   values   of   Threshould  may  speed  up
-- classification,   though   make   it   less  precise.  The  parameter
-- Complement, when True, indicates that the  complement  of  the  event
-- behind the example has to be estimated.
--
-- Returns :
--
--    The estimation result
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    Data_Error       - I/O error
--
   function Estimate
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Intuitionistic.Set;
--
-- Estimate -- An example in terms of classes
--
--    Classifier - A handle to
--    Context    - The data context
--    Complement - What to classify, example vs. its complement
--
-- This function uses the classifier pointed by the handle Classifier to
-- estimate one example from a fuzzy training set. The parameter Context
-- is the data context the training set is associated with.
--
-- Returns :
--
--    The estimation result
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    Data_Error       - I/O error
--
   function Estimate
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Intuitionistic.Set;
--
-- Get_Class -- Get the classifier class
--
--    Lesson  - A handle to the classifier
--
-- Returns :
--
--    The classifier class
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Class (Classifier : Classifier_Handle) return String;
--
-- Get_Classes -- Get the class feature of a classifier
--
--    Classifier - A handle to
--
-- Each classifier  has  a  dedicated  feature  describing  the  set  of
-- classes. The result of a classification is a fuzzy set over the class
-- feature domain.
--
-- Returns :
--
--    The handle of the class feature
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--
   function Get_Classes (Classifier : Classifier_Handle)
      return Feature_Handle;
--
-- Get_Examples -- Extract a set of examples
--
--    Classifier - A handle to
--    Lesson     - The handle to a fuzzy training set
--    Viewer     - A progress indication object or a handle to
--
-- This  procedure  extracts  training  examples  from  the   classifier
-- specified   by   the  handle  Classifier.  Each  one  should  have  a
-- corresponding training set from which the classifier can be restored.
-- The extracted examples are added to the training set specified by the
-- handle Lesson. Extracting examples can be  aborted  by  the  progress
-- indication  object.  In this case Lesson may have some examples added
-- and some not. Viewer can be an invalid handle.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    End_Error        - Extracting was aborted
--
   procedure Get_Examples
             (  Classifier : Classifier_Handle;
                Lesson     : in out Lecture_Handle;
                Viewer     : Indicator_Handle
             );
   procedure Get_Examples
             (  Classifier : Classifier_Handle;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             );
--
-- Get_Features -- Get the set of features tested by a graph-Classifier
--
--    Classifier - A handle to
--
-- Returns :
--
--    The set of handles to the features tested by Classifier
--
-- Exceptions :
--
--   Constraint_Error - Invalid handle
--
   function Get_Features (Classifier : Classifier_Handle)
      return Fuzzy.Feature.Handle.Container.Set;
--
-- Get_Training_Set_From -- Get the first training example number
--
--    Classifier - A handle to
--
-- This  function  returns  the number of the first example on which the
-- classifier was trained. The result may be wrong. It should be treated
-- as a mere hint. See also Get_Training_Set_Name.
--
-- Returns :
--
--    The first example on which the classifier was trained
--
-- Exceptions :
--
--   Constraint_Error - Invalid handle
--
   function Get_Training_Set_From (Classifier : Classifier_Handle)
      return Positive;
--
-- Get_Training_Set_Length -- Get the number of training examples
--
--    Classifier - A handle to
--
-- This function returns the number of examples on which the  classifier
-- was  trained. The result may be wrong. It should be treated as a mere
-- hint. See also Get_Training_Set_Name.
--
-- Returns :
--
--    The number of examples.
--
-- Exceptions :
--
--   Constraint_Error - Invalid handle
--
   function Get_Training_Set_Length (Classifier : Classifier_Handle)
      return Natural;
--
-- Get_Training_Set_Name -- Get external name of the training set
--
--    Classifier - A handle to
--
-- This function returns the external name of the training set  used  to
-- create the classifier. The name identifies the training  set  in  the
-- external  storage.  The result is UTF-8 encoded. The result may refer
-- to wrong or no training set. It should be treated as a mere hint.
--
-- Returns :
--
--    The set name or empty string
--
-- Exceptions :
--
--   Constraint_Error - Invalid handle
--
   function Get_Training_Set_Name (Classifier : Classifier_Handle)
      return String;
--
-- Invalidate -- Detach handle from the object
--
--    Classifier - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Classifier : in out Classifier_Handle);
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Classifier - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Classifier : Classifier_Handle) return Boolean;
--
-- Learn -- Learn incrementally from a set of examples
--
--    Classifier  - A handle to
--    Lesson      - A handle to
--  [ Name ]      - The external name of the training set, UTF-8
--    Features    - The sequence of features to train on
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of classifiers in terms of quality
--    Viewer      - A progress indication object or a handle to
--
-- This procedure  modifies  the  classifier  specified  by  the  handle
-- Classifier according to new training examples from the  training  set
-- Lesson. The parameters From and To specify a range of  examples  used
-- for training. Usually it is the whole set. The parameter  To  can  be
-- greater than the total number of  examples.  The  parameter  Features
-- specifies the sequence of features. The list may  contain  No_Feature
-- elements, which are ignored. The sequence may be empty. The parameter
-- Threshold  specifies  the  truncation  level.  All confidence factors
-- below Threshold are treated as 0. The parameter  Equivalence  is  the
-- quality level by which hypotheses are separated. If the qualities  of
-- two  hypotheses  differ  less  than  Equivalence,  they  are  assumed
-- equivalent. When Equivalence is  Confidence'Last,  features  are  not
-- selected,  so their ordering intesting is not changed.  Viewer can be
-- an invalid handle.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--    Data_Error       - I/O error
--    End_Error        - Learning was aborted
--    Use_Error        - A feature depends on the classifier
--
   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Name        : String;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             );
   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : Indicator_Handle
             );
   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Name        : String;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             );
   procedure Learn
             (  Classifier  : in out Classifier_Handle;
                Lesson      : Lecture_Handle;
                Features    : Feature_Array;
                From        : Positive   := 1;
                To          : Positive   := Positive'Last;
                Threshold   : Confidence := Confidence'First;
                Equivalence : Confidence := Default_Equivalence;
                Viewer      : not null access Indicator_Object'Class :=
                                 Negleter'Access
             );
--
-- Learn -- Learn incrementally from a set of examples
--
--    Classifier - A handle to
--    Context    - The training data context
--    Features   - The sequence of features to train on
--    From       - The first example to learn from
--    To         - The last example to learn from
--
-- This procedure  modifies  the  classifier  specified  by  the  handle
-- Classifier according to new training examples from the  training  set
-- associated with Context.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, classifier or context
--    Data_Error       - I/O error
--    End_Error        - Learning was aborted
--    Use_Error        - A feature depends on the classifier
--
   procedure Learn
             (  Classifier : in out Classifier_Handle;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             );
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Classifier - A handle to
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Classifier : Classifier_Handle)
      return Classifier_Object_Ptr;
--
-- Ref -- Get handle to a graph-Classifier object
--
--    Classifier - The classifier object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Classifier : Classifier_Object_Ptr)
      return Classifier_Handle;
--
-- Ref -- Set a handle to a classifier object
--
--    Handle     - The handle to set
--    Classifier - The classifier object
--
   procedure Ref
             (  Handle     : in out Classifier_Handle;
                Classifier : Classifier_Object_Ptr
             );
--
-- To_Classifier_Handle -- Handle conversion
--
--    Classifier - A handle to
--
-- Returns :
--
--    A handle to the classifier
--
-- Exceptions :
--
--    Constraint_Error - Argument is invalid or is not a classifier
--
   function To_Classifier_Handle
            (  Classifier : Deposit_Handles.Handle
            )  return Classifier_Handle;
--
-- To_Deposit_Handle -- Handle conversion
--
--    Classifier - A handle to
--
-- Returns :
--
--    Handle to archived object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Deposit_Handle (Classifier : Classifier_Handle)
      return Deposit_Handles.Handle;
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier - The classifier (a handle to)
--    Lesson     - The test set (a handle to)
--    Result     - A training set to place the classifications
--    Report     - An intuitionistic set to place the report
--    From       - The first example (to start from)
--    To         - The last example
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Viewer     - A progress indication object or a handle to
--    Difference - Comparison function
--
-- This procedure validates a classifier against examples from  a  fuzzy
-- training  set Lesson. The function processes examples From..To of the
-- set.  The  parameter  To  may be greater than the total number of the
-- examples in the training set. Images not represented in the  set  are
-- assumed  unknown.  The  classifications of the images of the selected
-- examples  are  compared  with  the  corresponding images of the class
-- feature in the example using Difference and stored  in  the  training
-- set Result. When Result is an invalid handle  a  new  memory-resident
-- training set is created and Result is set to point to it. The mean of
-- the  differences  is returned for each class in the parameter Report.
-- The set of classes is the  domain  set  of  the  class  feature  (see
-- Get_Classes). So Report is an intuitionistic  fuzzy  set,  which  for
-- each  class  has  the  misclassification  mean.  It  must  have   the
-- cardinality  of  the  class-feature,  otherwise  Constraint_Error  is
-- propagated. The parameter Threshold specifies the  truncation  level.
-- All  confidence  factors less than Threshold are treated as 0. Higher
-- values of Threshold may speed up classification, though make it  less
-- precise.
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or classifier
--    Data_Error       - I/O error
--    End_Error        - Verification was aborted
--
   procedure Verify
             (  Classifier : Classifier_Handle;
                Lesson     : Lecture_Handle;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Generalize : Generalization_Mode := Linear;
                Threshold  : Confidence          := Confidence'First;
                Viewer     : Indicator_Handle;
                Difference : Divergence_Function := Diff'Access
             );
   procedure Verify
             (  Classifier : Classifier_Handle;
                Lesson     : Lecture_Handle;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Generalize : Generalization_Mode := Linear;
                Threshold  : Confidence          := Confidence'First;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access;
                Difference : Divergence_Function := Diff'Access
             );
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier - The classifier
--    Context    - The data context
--    Result     - A training set to place the classifications
--    Report     - An intuitionistic set to place the report
--    From       - The first example (to start from)
--    To         - The last example
--    Difference - Comparison function
--
-- This procedure validates a classifier against examples from  a  fuzzy
-- training set associated to Context.
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or classifier
--    Data_Error       - I/O error
--    End_Error        - Verification was aborted
--
   procedure Verify
             (  Classifier : Classifier_Handle;
                Context    : in out Classification_Parameters'Class;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive            := 1;
                To         : Positive            := Positive'Last;
                Difference : Divergence_Function := Diff'Access
             );
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier - The classifier (a handle to)
--    Lesson     - The training set (a handle to)
--    From       - The first example (to start from)
--    To         - The last example
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Viewer     - A progress indication object or a handle to
--    Difference - Comparison function
--
-- This function is a  variant  of  the  procedure  Verify  with  Report
-- returned as the result of.
--
-- Returns :
--
--    The distribution of the means of classification errors
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or classifier
--    Data_Error       - I/O error
--    End_Error        - Verification was aborted
--
   function Verify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : Indicator_Handle;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set;
   function Verify
            (  Classifier : Classifier_Handle;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set;
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier - The classifier
--    Context    - The data context
--    From       - The first example (to start from)
--    To         - The last example
--    Difference - Comparison function
--
-- This function is a  variant  of  the  procedure  Verify  with  Report
-- returned as the result of.
--
-- Returns :
--
--    The distribution of the means of classification errors
--
-- Exceptions :
--
--    Constraint_Error - An invalid handle or classifier
--    Data_Error       - I/O error
--    End_Error        - Verification was aborted
--
   function Verify
            (  Classifier : Classifier_Handle;
               Context    : not null access
                            Classification_Parameters'Class;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set;

private
   pragma Inline (Classify);
   pragma Inline (Estimate);
   pragma Inline (Get_Classes);
   pragma Inline (Get_Examples);
   pragma Inline (Get_Features);
   pragma Inline (Get_Training_Set_Name);
   pragma Inline (Invalidate);
   pragma Inline (Is_Valid);
   pragma Inline (Learn);
   pragma Inline (Ptr);
   pragma Inline (Ref);
   pragma Inline (To_Classifier_Handle);
   pragma Inline (To_Deposit_Handle);
   pragma Inline (Verify);

   type Classifier_Handle is new Handles.Handle with null record;

end Fuzzy.Classifier.Handle;
