--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Classificatory                Luebeck            --
--  Interface                                      Autumn, 2002       --
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

with Fuzzy.Feature.Handle;     use Fuzzy.Feature.Handle;
with Fuzzy.Classifier;         use Fuzzy.Classifier;
with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Fuzzy.Lecture.Handle;     use Fuzzy.Lecture.Handle;
with Indicator;                use Indicator;
with Indicator.Handle;         use Indicator.Handle;

with Fuzzy.Feature.Data;
with Fuzzy.Feature.Independent;

package Fuzzy.Feature.Classificatory is
   pragma Elaborate_Body (Fuzzy.Feature.Classificatory);
--
-- Classificatory_Class -- The class name
--
   Classificatory_Class : constant String :=
      Feature_Class & "Classificatory";
--
-- Create -- Create a classificatory feature
--
--    Name       - Of the feature
--    Classifier - A handle to classifier
--    Generalize - Generalization mode
--    Threshold  - For classifications
--
-- This function creates a  classificatory  feature  which  will  use  a
-- Classifier  for  classification. The domain set of the feature is the
-- set of classes of the classifier.
--
-- Returns :
--
--    Pointer to the created object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or classifier
--
   function Create
            (  Name       : String;
               Classifier : Classifier_Handle;
               Generalize : Generalization_Mode := Linear;
               Threshold  : Confidence          := Confidence'First
            )  return Feature_Handle;
--
-- Get_Classifier -- Get the classifier
--
--    Feature - Object or a handle to it
--
-- This  function  returns  a  handle  to  the  classifier  used  for  a
-- classification.  Note  that  when  the  result  of  this operation is
-- modified  it  does not necessary influenced the feature's classifier.
-- Therefore,  if  a  learning  need  to  be  applied  to  it, the Learn
-- operation should be applied directly to the feature handle.
--
-- Returns :
--
--    The classifier's handle
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or object
--
   function Get_Classifier (Feature : Feature_Handle)
      return Classifier_Handle;
   function Get_Classifier (Feature : Feature_Object'Class)
      return Classifier_Handle;
--
-- Get_Generalization -- Get the generalization mode
--
--    Feature - A feature handle or object
--
-- Returns :
--
--    The generalization mode used for classification
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or classifier
--
   function Get_Generalization (Feature : Feature_Handle)
      return Generalization_Mode;
   function Get_Generalization (Feature : Feature_Object'Class)
      return Generalization_Mode;
--
-- Get_Threshold -- Get the threshold
--
--    Feature - A feature handle or object
--
-- Returns :
--
--    Threshold used for classification
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or classifier
--
   function Get_Threshold (Feature : Feature_Handle)
      return Confidence;
   function Get_Threshold (Feature : Feature_Object'Class)
      return Confidence;
--
-- Is_Classificatory -- Check handle
--
--    Feature - Object or a handle to it
--
-- Returns :
--
--    True if Feature refers a valid classificatory feature
--
   function Is_Classificatory (Feature : Feature_Handle) return Boolean;
   function Is_Classificatory (Feature : Feature_Object'Class)
      return Boolean;
--
-- Learn -- Learn incrementally from a set of examples
--
--    Feature   - Object or a handle to classificatory feature
--    Lesson    - The training set (a handle to)
--    Features  - The sequence of features to test
--    From      - The first example to learn from
--    To        - The last example to learn from
--    Threshold - The confidence threshold
--    Viewer    - A progress indication object or a handle to
--
-- This  procedure  modifies  the classifier of the feature according to
-- new  training  examples  from the set Lesson. For further information
-- see   the   package   Fuzzy.Classifier.Handle.   Constraint_Error  is
-- propagated when Feature is not a classificatory one.  Use_Error  does
-- when Features contains Feature or on any other  circular  dependency.
-- Viewer can be an invalid handle.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or feature
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Circular dependency on itself
--
   procedure Learn
             (  Feature   : in out Feature_Handle;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : Indicator_Handle
             );
   procedure Learn
             (  Feature   : in out Feature_Object'Class;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : Indicator_Handle
             );
   procedure Learn
             (  Feature   : in out Feature_Handle;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : not null access Indicator_Object'Class :=
                               Negleter'Access
             );
   procedure Learn
             (  Feature   : in out Feature_Object'Class;
                Lesson    : Lecture_Handle;
                Features  : Feature_Array;
                From      : Positive   := 1;
                To        : Positive   := Positive'Last;
                Threshold : Confidence := Confidence'First;
                Viewer    : not null access Indicator_Object'Class :=
                               Negleter'Access
             );
--
-- Learn -- Learn incrementally from a set of examples
--
--    Feature  - Object or a handle to classificatory feature
--    Context  - The training data context
--    Features - The sequence of features to train on
--    From     - The first example to learn from
--    To       - The last example to learn from
--
-- This  procedure  modifies  the classifier of the feature according to
-- new training examples from the set associated with Context.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, feature or context
--    Data_Error       - I/O error in the training set
--    End_Error        - Learning was aborted
--    Use_Error        - Circular dependency on itself
--
   procedure Learn
             (  Feature  : in out Feature_Handle;
                Context  : in out Training_Data'Class;
                Features : Feature_Array;
                From     : Positive := 1;
                To       : Positive := Positive'Last
             );
   procedure Learn
             (  Feature  : in out Feature_Object'Class;
                Context  : in out Training_Data'Class;
                Features : Feature_Array;
                From     : Positive := 1;
                To       : Positive := Positive'Last
             );
--
-- Set_Classifier -- Set new classifier
--
--    Feature    - A feature handle or object
--    Classifier - A handle to
--
-- This  procedure  assigns  a  new  classifier  to  the  classificatory
-- feature. The new classifier shall have the same class-feature as  the
-- previous one had. Otherwise, Constraint_Error is propagated.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or classifier
--
   procedure Set_Classifier
             (  Feature    : in out Feature_Handle;
                Classifier : Classifier_Handle
             );
   procedure Set_Classifier
             (  Feature    : in out Feature_Object'Class;
                Classifier : Classifier_Handle
             );
--
-- Set_Generalization -- Set new generalization mode
--
--    Feature    - A feature handle or object
--    Generalize - The new value
--
-- This   procedure   changes   the   generalization  mode  used  during
-- classifications.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or classifier
--
   procedure Set_Generalization
             (  Feature    : in out Feature_Handle;
                Generalize : Generalization_Mode
             );
   procedure Set_Generalization
             (  Feature    : in out Feature_Object'Class;
                Generalize : Generalization_Mode
             );
--
-- Set_Threshold -- Set new threshold
--
--    Feature   - A feature handle or object
--    Threshold - The new value
--
-- This procedure changes the threshold used during classifications.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, object or classifier
--
   procedure Set_Threshold
             (  Feature   : in out Feature_Handle;
                Threshold : Confidence
             );
   procedure Set_Threshold
             (  Feature   : in out Feature_Object'Class;
                Threshold : Confidence
             );
private
   pragma Inline (Get_Classifier);
   pragma Inline (Get_Generalization);
   pragma Inline (Get_Threshold);
   pragma Inline (Is_Classificatory);
   pragma Inline (Learn);

   use Fuzzy.Feature.Independent;
   use Fuzzy.Feature.Data;
--
-- Classificatory_Feature_Object -- The classificatory feature type
--
   type Classificatory_Feature_Object is
      new Independent_Feature_Object with
   record
      Classifier : Classifier_Handle;
      Generalize : Generalization_Mode;
      Threshold  : Confidence;
      Updated    : Boolean := False;
   end record;
--
-- Get_Class -- Overrides Object.Archived...
--
   function Get_Class (Feature : Classificatory_Feature_Object)
      return String;
   pragma Inline (Get_Class);
--
-- Get_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Get_Range
             (  Source     : String;
                Pointer    : in out Integer;
                Feature    : Classificatory_Feature_Object;
                From       : out Positive;
                To         : out Positive;
                Exclusive  : out Boolean;
                Parameters : Input_Parameters'Class
             );
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Feature : Classificatory_Feature_Object;
                List    : in out Deposit_Container'Class
             );
--
-- Is_Computed -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Computed
            (  Feature : Classificatory_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean;
--
-- Is_Defined -- Overrides Fuzzy.Feature...
--
   overriding
   function Is_Defined
            (  Feature : Classificatory_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified
            (  Feature : Classificatory_Feature_Object
            )  return Boolean;
--
-- Put_Range -- Overrides Fuzzy.Feature...
--
   overriding
   procedure Put_Range
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Classificatory_Feature_Object;
                From        : Positive;
                To          : Positive;
                Parameters  : Output_Parameters'Class;
                Field       : Natural   := 0;
                Justify     : Alignment := Left;
                Fill        : Character := ' '
             );
--
-- Query -- Overrides Fuzzy.Feature.Independent...
--
   overriding
   procedure Query
             (  Feature : Classificatory_Feature_Object;
                Context : in out Context_Object'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data'Class
             );
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Feature : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Feature     : Classificatory_Feature_Object
             );
end Fuzzy.Feature.Classificatory;
