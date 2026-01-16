--                                                                    --
--  package Fuzzy.Classifier        Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2003       --
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
--  The package also defines the type Classifier which integrates four
--  graphs into one unit.
--
with Fuzzy.Feature;          use Fuzzy.Feature;
with Fuzzy.Feature.Handle;   use Fuzzy.Feature.Handle;
with Fuzzy.Intuitionistic;   use Fuzzy.Intuitionistic;
with Fuzzy.Lecture;          use Fuzzy.Lecture;
with Fuzzy.Lecture.Handle;   use Fuzzy.Lecture.Handle;
with Fuzzy.Logic;            use Fuzzy.Logic;
with Indicator;              use Indicator;
with Object.Archived;        use Object.Archived;

with Ada.Unchecked_Conversion;
with Fuzzy.Feature.Context;
with Fuzzy.Feature.Handle.Container;
with Intervals.Floats;
with Object.Handle;

package Fuzzy.Classifier is
   pragma Elaborate_Body (Fuzzy.Classifier);
--
-- Classifier_Class -- The prefix of all classifier object classes
--
   Classifier_Class    : constant String := "Classifier.";
   Default_Equivalence : constant Confidence := 0.1;
--
-- Classifier -- The base type of all classifiers
--
   type Classifier_Object is abstract new Deposit with null record;
   type Classifier_Object_Ptr is access Classifier_Object'Class;
   for Classifier_Object_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
--
-- Divergence -- Confidence factors used to quantify difference of fuzzy
--               boolean values.
--
   type Divergence is new Float;
--
-- Divergence_Range -- Interval of differences
--
   package Divergence_Ranges is new Intervals.Floats (Divergence);
   subtype Divergence_Range is Divergence_Ranges.Interval;
   use type Divergence_Range;
--
-- Divergence_Vector -- Vector of differences
--
   type Divergence_Vector is
      array (Integer range <>) of Divergence_Range;
--
-- Divergence_Function -- A pointer type to a divergence function
--
--    Left, Right - Fuzzy boolean values
--
-- The  type  of functions used to evaluate difference between two fuzzy
-- boolean values.
--
-- Returns :
--
--    The divergence between Left and Right
--
   type Divergence_Function is not null access
      function (Left, Right : Fuzzy_Boolean)
         return Divergence_Range;
--
-- Generalization_Mode -- Used during classification
--
--    None    - No generalization made
--    Nearest - Nearest neighbour generalization
--    Linear  - Linear interpolation between two neighbours
--
   type Generalization_Mode is (None, Nearest, Linear);
--
-- Classification_Parameters
--
--    Lesson      - The training set
--    Cardinality - Of the classification result
--
-- This structure is used to keep classification parameters and temporal
-- results. The meaning of the fields:
--
--    Generalize - The way in which gaps in the training set are  filled
--                 upon classification.
--    Image      - This  field  specifies the type of the feature images
--                 to be classified.
--    Ready      - The  result  readiness  flag. When set to True, it is
--                 assumed that Result contains a completed result.
--    Result     - Here the classification result is accumulated.
--    Threshold  - This  field  specifies  the  truncation  level.   All
--                 confidence factors less than Threshold are treated as
--                 0.   Higher   values   of   Threshold  may  speed  up
--                 classification, though make it less precise.
--
   type Classification_Parameters
        (  Lesson      : not null access Lecture_Object'Class;
           Cardinality : Positive
        )  is new Fuzzy.Feature.Context.Lecture_Context (Lesson) with
   record
      Threshold  : Confidence;
      Image      : Image_Type;
      Generalize : Generalization_Mode;
      Ready      : Boolean;
      Result     : Classification (Cardinality);
   end record;
--
-- Training_Data -- Information used for training
--
--    Lesson - The training set
--    Length - Of the training set external name
--    Viewer - An indication object
--
-- It is used during learning too keep training data and parameters. The
-- meaning of some fields:
--
--    Threshould  - Truncation level. All confidence  factors  under  it
--                  are treated as 0;
--    Equivalence - Level  of  separation  hypotheses  quality.  If  the
--                  qualities  of  the  subclassifiers  differ less than
--                  Equivalence, they are assumed same. When Equivalence
--                  is Confidence'Last, features are  not  selected,  so
--                  their order of testing is not changed;
--    Set_Name    - The training set external name, UTF-8  encoded.  The
--                  set   name   is  optional,  used  in  the  generated
--                  classifier to  hint  the  set.  When  empty,  it  is
--                  unused.
--
   type Training_Data
        (  Lesson : not null access Lecture_Object'Class;
           Length : Natural;
           Viewer : not null access Indicator_Object'Class
        )  is new Fuzzy.Feature.Context.Lecture_Context (Lesson) with
   record
      Threshold   : Confidence := Confidence'First;
      Equivalence : Confidence := Default_Equivalence;
      Set_Name    : String (1..Length);
   end record;
--
-- Classify -- Classification of a training example
--
--    Classifier - The classifier to use
--    Lesson     - The training set (a handle to)
--    Example    - The example to classify
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Complement - What to classify: example vs. its complement
--
-- This function uses Classifier to classify one example  from  a  fuzzy
-- training set. The parameter Lesson is the training  set  to  classify
-- one example from. The example is specified by the parameter  Example.
-- Images  not represented in the set are assumed unknown. The result of
-- classification  is   an   intuitionistic   classification   pair   of
-- distributions of over the  domain  set  of  the  class  feature  (see
-- Get_Classes). The  parameter  Generalize  advices  how  gaps  in  the
-- training set should be  filled  upon  classification.  The  parameter
-- Threshold specifies the truncation level. All confidence factors less
-- than Threshold are treated as 0. Higher values of Threshold may speed
-- up  classification,  though  make  it  less  precise.  The  parameter
-- Complement, when True, indicates that the  complement  of  the  event
-- behind the example has to be classified.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier
--    Data_Error       - I/O error in the training set
--
   function Classify
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Classification;
--
-- Classify -- Classification of a training example
--
--    Classifier - The classifier to use
--    Context    - The classification data context
--    Complement - What to classify: example vs. its complement
--
-- This function has to be implemented by the derived type.
--
-- Returns :
--
--    The classification result
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier or context
--    Data_Error       - I/O error in the training set
--
   function Classify
            (  Classifier : Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Classification is abstract;
--
-- Diff -- Difference between two classifications
--
--    Left, Right - Logical values, classifications
--
-- This function evaluates difference  between  two  logical  values  or
-- classifications.  The  difference   is   evaluated   as   per   point
-- differences. Each point difference is:
--
--    Lower = min (pos(Left) xor pos(Righ), nec(Left) xor nec(Right))
--    Upper = max (pos(Left) xor pos(Righ), nec(Left) xor nec(Right))
--
-- Returns :
--
--    Difference between them
--
   function Diff (Left, Right : Fuzzy_Boolean) return Divergence_Range;
   function Diff (Left, Right : Classification)
      return Divergence_Vector;
--
-- Estimate -- A training example in terms of classes
--
--    Classifier - The classifier to use
--    Lesson     - The training set (a handle to)
--    Example    - The example to classify
--    Threshold  - The confidence threshold
--    Complement - What to estimate: example vs. its complement
--
-- This function uses Classifier to estimate one example  from  a  fuzzy
-- training  set.  The  example  is  specified by the parameter Example.
-- Images  not  represented in the training set are assumed unknown. The
-- result is an intuitionistic set estimating the training  examples  in
-- terms of the classes. The set of classes is the  domain  set  of  the
-- class feature (see Get_Classes). The parameter Generalize advices how
-- gaps in the training set should be filled  upon  classification.  The
-- parameter  Threshold  specifies  the truncation level. All confidence
-- factors less than Threshold  are  treated  as  0.  Higher  values  of
-- Threshold may speed up classification, though make it  less  precise.
-- The parameter Complement, when True, indicates that the complement of
-- the event behind the example has to be estimated.
--
-- Returns :
--
--    The estimation result
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier
--    Data_Error       - I/O error in the training set
--
   function Estimate
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               Example    : Positive;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Complement : Boolean             := False
            )  return Intuitionistic.Set;
--
-- Estimate -- A training example in terms of classes
--
--    Classifier - The classifier to use
--    Context    - The classification data context
--    Complement - What to classify: example vs. its complement
--
-- This function has to be implemented by the derived type.
--
-- Returns :
--
--    The estimation result
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier or context
--    Data_Error       - I/O error in the training set
--
   function Estimate
            (  Classifier : Classifier_Object;
               Context    : not null access
                            Classification_Parameters'Class;
               Complement : Boolean := False
            )  return Fuzzy.Intuitionistic.Set is abstract;
--
-- Get_Classes -- Get the class feature of a classifier
--
--    Classifier - The classifier
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
--    Constraint_Error - An invalid classifier
--
   function Get_Classes (Classifier : Classifier_Object)
      return Feature_Handle is abstract;
--
-- Get_Examples -- Extract a set of examples
--
--    Classifier - The classifier
--    Lesson     - The handle to a fuzzy training set
--    Viewer     - A progress indication object
--
-- This  procedure  extracts training examples from Classifier. Each one
-- should  have  a  corresponding training set from which the classifier
-- can be restored. The extracted examples are added to the training set
-- specified by the handle Lesson.
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier
--    Data_Error       - I/O error in the training set
--
   procedure Get_Examples
             (  Classifier : Classifier_Object;
                Lesson     : in out Lecture_Handle;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access
             )  is abstract;
--
-- Get_Features -- Get the set of features used
--
--    Classifier - The classifier
--
-- Returns :
--
--    The set of handles to the features used
--
   function Get_Features (Classifier : Classifier_Object)
      return Fuzzy.Feature.Handle.Container.Set is abstract;
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
   function Get_Training_Set_From (Classifier : Classifier_Object)
      return Positive is abstract;
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
   function Get_Training_Set_Length (Classifier : Classifier_Object)
      return Natural is abstract;
--
-- Get_Training_Set_Name -- Get external name of the training set
--
--    Classifier - The classifier
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
   function Get_Training_Set_Name (Classifier : Classifier_Object)
      return String is abstract;
--
-- Learn -- Learn incrementally from a set of examples
--
--    Classifier  - A classifier to modify
--    Lesson      - The training set (a handle to)
--  [ Name ]      - The external name of the training set, UTF-8
--    Features    - The sequence of features to test
--    From        - The first example to learn from
--    To          - The last example to learn from
--    Threshold   - The confidence threshold
--    Equivalence - Of classifiers in terms of quality
--    Viewer      - A progress indication object
--
-- This procedure modifies Classifier according to the training examples
-- from  the  training  set Lesson. The parameter Features specifies the
-- sequence of features. The list may contain No_Feature elements, which
-- are  ignored.  The  sequence may be empty. The parameters From and To
-- specify the range of training examples to learn from. Usually  it  is
-- the whole set. The parameter To can be greater than the total  number
-- of examples. The parameter Threshold specifies the truncation  level.
-- All  confidence  factors  below  Threshold  are  treated  as  0.  The
-- parameter Equivalence is the Level of separation hypotheses  quality.
-- If the qualities differ less than Equivalence, they are assumed same.
-- When  Equivalence  is  Confidence'Last,  fatures are not selected, so
-- they  order of testing is not changed. The parameter Viewer refers to
-- a  progress  indication  object,  which  can  be  used  for  progress
-- indication and aborting of training. Use_Error is propagated when the
-- classifier would depend on any of the features it tests.
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Circular dependency
--
   procedure Learn
             (  Classifier  : in out Classifier_Object'Class;
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
             (  Classifier  : in out Classifier_Object'Class;
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
--    Classifier - A classifier to modify
--    Context    - The training data context
--    Features   - The sequence of features to train on
--    From       - The first example to learn from
--    To         - The last example to learn from
--
-- This procedure modifies Classifier according to the training examples
-- from  the  training  set  associated  with  Context.  It  has  to  be
-- implemented by the derived type. The parameter Features specifies the
-- sequence of features. The list may contain No_Feature elements, which
-- are  ignored.  The  sequence may be empty. The parameters From and To
-- specify the range of training examples to learn from. Usually  it  is
-- the whole set. The parameter To can be greater than the total  number
-- of examples. Use_Error is propagated when the classifier would depend
-- on any of the features it tests.
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier or context
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Circular dependency
--
   procedure Learn
             (  Classifier : in out Classifier_Object;
                Context    : in out Training_Data'Class;
                Features   : Feature_Array;
                From       : Positive := 1;
                To         : Positive := Positive'Last
             )  is abstract;
--
-- To_Classifier_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a graph-Classifier
--
   function To_Classifier_Object_Ptr (Ptr : Deposit_Ptr)
      return Classifier_Object_Ptr;
--
-- To_Confidence -- Saturating conversion
--
--    Left - The argument (divergence)
--
-- Values under 0.0 result in Confidence'First.  Values over  1.0  yield
-- Confidence'Last.
--
-- Returns :
--
--    Correspinding confidence factor
--
   function To_Confidence (Left : Divergence) return Confidence;
--
-- To_Deposit_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to archived object
--
   function To_Deposit_Ptr is
      new Ada.Unchecked_Conversion
          (  Classifier_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Divergence_Range -- Divergence of a class
--
--    Value - A classification
--    Index - Position in the classification
--
-- Returns :
--
--    Divergence range correspinding to the fuzzy logical value
--
-- Exceptions :
--
--    Constraint_Error - Wrong index
--
   function To_Divergence_Range
            (  Value : Classification;
               Index : Positive
            )  return Divergence_Range;
--
-- To_Fuzzy_Boolean -- Saturating conversion
--
--    Left - The argument (divergence range)
--
-- This function uses To_Confidence per component basis.
--
-- Returns :
--
--    Correspinding fuzzy logical value
--
   function To_Fuzzy_Boolean (Left : Divergence_Range)
      return Fuzzy_Boolean;
--
-- To_Set -- Conversion to a set
--
--    Left - An array of divergences
--    Norm - Normalization factor
--
-- When Norm is 0 the result is an empty set.
--
-- Returns :
--
--    An intuitionistic set with the components divided by Norm
--
   function To_Set (Left : Divergence_Vector; Norm : Natural)
      return Intuitionistic.Set;
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier - The classifier
--    Lesson     - The training set (a handle to)
--    Result     - A training set to place the classifications
--    Report     - An intuitionistic set to place the report
--    From       - The first example (to start from)
--    To         - The last example
--    Generalize - Generalization mode
--    Threshold  - The confidence threshold
--    Viewer     - A progress indication object
--    Difference - Comparison function
--
-- This procedure validates a classifier against examples from  a  fuzzy
-- training set or a data context the training set is  associated  with.
-- The function processes examples From..To of the set. The parameter To
-- may be greater than the total number of the examples in the  training
-- set.  Images  not  represented  in  the  set are assumed unknown. The
-- classification  of  the  images of the selected examples are compared
-- with  the  corresponding  images  of the class feature in the example
-- using  Difference  and stored in the training set Result. When Result
-- is an invalid handle a new memory-resident training  set  is  created
-- and  Result  is  set  to  point to it. The mean of the differences is
-- returned for each class in the parameter Report. The set  of  classes
-- is the domain set of the class feature (see Get_Classes).  So  Report
-- is an  intuitionistic  fuzzy  set,  which  for  each  class  has  the
-- misclassification   mean.   It  must  have  the  cardinality  of  the
-- class-feature,   otherwise   Constraint_Error   is   propagated.  The
-- parameter  Threshold  specifies  the truncation level. All confidence
-- factors less than Threshold  are  treated  as  0.  Higher  values  of
-- Threshold may speed up classification, though make it less precise.
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier, handle or set cardinality
--    Data_Error       - I/O error in the training set
--    End_Error        - Verification was aborted
--
   procedure Verify
             (  Classifier : Classifier_Object'Class;
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
--    Context    - The classification data context
--    Result     - A training set to place the classifications
--    Report     - An intuitionistic set to place the report
--    From       - The first example (to start from)
--    To         - The last example
--    Viewer     - A progress indication object
--    Difference - Comparison function
--
-- This procedure is similar to above, but uses the classification  data
-- context.
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier, handle or set cardinality
--    Data_Error       - I/O error in the training set
--    End_Error        - Verification was aborted
--
   procedure Verify
             (  Classifier : Classifier_Object'Class;
                Context    : in out Classification_Parameters'Class;
                Result     : in out Lecture_Handle;
                Report     : in out Intuitionistic.Set;
                From       : Positive := 1;
                To         : Positive := Positive'Last;
                Viewer     : not null access Indicator_Object'Class :=
                                Negleter'Access;
                Difference : Divergence_Function := Diff'Access
            );
--
-- Verify -- Validate a classifier against a training set (exams)
--
--    Classifier       - The classifier
--    Context / Lesson - The data context or a training set handle
--    From             - The first example (to start from)
--    To               - The last example
--  [ Generalize       - Generalization mode
--    Threshold ]      - The confidence threshold
--    Viewer           - A progress indication object
--    Difference       - Comparison function
--
-- These  functions  are  variants  of  the procedures Verify above with
-- Report returned as the result of.
--
-- Returns :
--
--    The distribution of the means of classification errors
--
-- Exceptions :
--
--    Constraint_Error - Invalid classifier, handle or parameters
--    Data_Error       - I/O error in the training set
--    End_Error        - Verification was aborted
--
   function Verify
            (  Classifier : Classifier_Object'Class;
               Context    : not null access
                            Classification_Parameters'Class;
               From       : Positive := 1;
               To         : Positive := Positive'Last;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set;
   function Verify
            (  Classifier : Classifier_Object'Class;
               Lesson     : Lecture_Handle;
               From       : Positive            := 1;
               To         : Positive            := Positive'Last;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First;
               Viewer     : not null access Indicator_Object'Class :=
                               Negleter'Access;
               Difference : Divergence_Function := Diff'Access
            )  return Intuitionistic.Set;

private
   pragma Inline (Diff);
   pragma Inline (To_Classifier_Object_Ptr);
   pragma Inline (To_Confidence);
   pragma Inline (To_Fuzzy_Boolean);
--
-- Handles -- To Classifier_Objects
--
   package Handles is
      new Object.Handle (Classifier_Object, Classifier_Object_Ptr);

end Fuzzy.Classifier;
