--                                                                    --
--  package Fuzzy.Lecture.Handle    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Spring, 2002       --
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
--  This package defines the public interface to fuzzy training sets.  A
--  training  set  can  be  viewed  as  a  table.  Rows of the table are
--  training  examples.  Columns  are features used in the training set.
--  Each  cell  of  the  table  is  a set of images of the corresponding
--  feature  (column)  in  the given example (row). There could be up to
--  four  different images characterizing relations of an event in terms
--  of  the  feature  domain  set  values. See Fuzzy.Feature.Image_Type.
--  Images  can  be  independently  accessed  for  read and write. If an
--  example or feature is not yet in the set when a realization  has  to
--  be written then the corresponding row or column is added.
--
--  When  Lecture_Handle  is created it initially empty. Adding training
--  examples   would   allocate   memory   necessary  to  keep  them  as
--  appropriate. If a training set with a special behavior is  required,
--  such as a data base resident set etc, then Lecture_Handle need to be
--  obtained   from   the   corresponding   training  set  factory.  See
--  Fuzzy.Lecture.Handle.factory for further information.
--
with Fuzzy.Intuitionistic;  use Fuzzy.Intuitionistic;
with Indicator.Handle;      use Indicator.Handle;

with Deposit_Handles;

package Fuzzy.Lecture.Handle is
   type Lecture_Handle is tagged private;
--
-- Copy -- Examples of a training set
--
--    Target - A handle to the training to place copied examples into
--    Source - A handle to the training set to copy
--    From   - The number of the first example to copy
--    To     - The number of the last example to copy
--    Viewer - A progress indication object (or a handle to)
--
-- All  examples  from  the  training  set  Source  are  copied into the
-- training set Target. If the target set is not empty, the examples are
-- added  to  the  end of the set. Viewer can be an invalid handle. When
-- Source is an invalid handle, the operation is void. When Target is an
-- invalid handle, a new memory-resident training  set  is  created  and
-- Target will point to it.
--
-- Exceptions :
--
--    End_Error  - Operation was aborted
--    Data_Error - I/O error
--
   procedure Copy
             (  Target : in out Lecture_Handle;
                Source : Lecture_Handle;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : Indicator_Handle
             );
   procedure Copy
             (  Target : in out Lecture_Handle;
                Source : Lecture_Handle;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- Delete -- Training set deletion request
--
--    Lesson - A handle to the training set
--
-- This procedure requests deletion of Lesson. Lesson becomes an invalid
-- handle. The training set  itself  is  deleted  if  possible.  Nothing
-- happens if Lesson is not a valid handle.
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   procedure Delete (Lesson : in out Lecture_Handle);
--
-- Get -- Get image of a teaching example
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - A handle to the feature in the example
--    Image   - To request
--
-- The result is the specified by  the  parameter  Image  image  of  the
-- training example Example for the feature Feature from the set Lesson.
-- When  Lesson is an invalid handle, the result is a fuzzy set with all
-- members possible.
--
-- Returns :
--
--    A fuzzy subset of the feature domain set.
--
-- Exceptions :
--
--    Constraint_Error - Invalid Feature
--    Data_Error       - I/O error
--
   function Get
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Set;
--
-- Get -- Get classification for a feature
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - A handle to the feature in the example
--    Value   - The value, an intuitionistic classification
--
-- This function gets an intuitionistic classification  of  the  example
-- Example for the feature Feature. A  classification  consists  of  the
-- images Has_In and Has_Not. When Lesson  is  an  invalid  handle,  the
-- result  is  a  classification with the possibilities set to 1 and the
-- necessities set to 0.
--
-- Returns :
--
--    The result
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality of Value, invalid Feature
--    Data_Error       - I/O error
--
   function Get
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle
            )  return Classification;
--
-- Get_Class -- Get the lecture class
--
--    Lesson  - A handle to a teaching set
--
-- Returns :
--
--    The lecture class
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Get_Class (Lesson : Lecture_Handle) return String;
--
-- Get_Examples_Number -- Get the number of examples in a set
--
--    Lesson - A handle of a teaching set
--
-- The result is 0 when Lesson is an invalid handle.
--
-- Returns :
--
--    The number of examples in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Examples_Number (Lesson : Lecture_Handle)
      return Natural;
--
-- Get_Feature -- Get a feature in a set
--
--    Lesson - A handle to the teaching set
--    Index  - Of the feature 1..Get_Features_Number
--
-- Returns :
--
--    The handle to the feature
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--    Data_Error       - I/O error
--
   function Get_Feature
            (  Lesson : Lecture_Handle;
               Index  : Positive
            )  return Feature_Handle;
--
-- Get_Features -- Get the features in a training set
--
--    Lesson - A handle to the teaching set
--
-- A feature is in the set if the set defines at least one image of this
-- feature.  This  function returns the array of handles to the features
-- in the set. The order of feature handles in the array reflects  their
-- order  in  the  set.  The  result is an empty array when Lesson is an
-- invalid handle.
--
-- Returns :
--
--    The features in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Features (Lesson : Lecture_Handle)
      return Bounded_Array;
--
-- Get_Features_Number -- Get the number of features in a set
--
--    Lesson - A handle of the teaching set
--
-- A feature is in the set if the set defines at least  one  realization
-- of this feature. The result is 0 when Lesson is an invalid handle.
--
-- Returns :
--
--    The number of features in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Features_Number (Lesson : Lecture_Handle)
      return Natural;
--
-- Get_Number_Of_Lectures -- Get total number of lectures
--
-- Returns :
--
--    The number of lectures existing at the moment
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Number_Of_Lectures return Natural
      renames Fuzzy.Lecture.Get_Number_Of_Lectures;
--
-- Invalidate -- Detach handle from the object
--
--    Lesson - The handle
--
-- This procedure makes handle pointing to nothing. If it was  the  last
-- reference to the object, the latter is destroyed.
--
   procedure Invalidate (Lesson : in out Lecture_Handle);
--
-- Is_Defined - Check if the set contains data
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - A handle to the feature in the example
--    Image   - To check for
--
-- This function can be used to query whether an image (parameter Image)
-- defined  by  the  training set Lesson for the example Example and the
-- feature Feature. Undefined images when requested  are  first  queried
-- from  some  external  source  by  the Get-function. But this function
-- skips the querying mechanism. See also  Query  which  is  called  for
-- unknown cells to get the value from an external source. The result is
-- False when Lesson is an invalid handle.
--
-- Returns :
--
--    True if the image is present in the lecture example
--
-- Exceptions :
--
--    Constraint_Error - Invalid Feature
--    Data_Error       - I/O error
--
   function Is_Defined
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Boolean;
--
-- Is_Known - Check image of an example
--
--    Lesson  - A teaching set
--    Example - The number of the example in the set
--    Feature - The handle of the feature in the example
--    Image   - The characteristic to check
--
-- This  function checks if the specified image is unknown, i.e. has all
-- values  of  the membership function 1. The querying mechanism is used
-- for undefined values. The result is False when Lesson is  an  invalid
-- handle.
--
-- Returns :
--
--    False if Get (Lesson, Example, Feature, Image) would return all 1
--
-- Exceptions :
--
--    Constraint_Error - Invalid Feature
--    Data_Error       - I/O error
--
   function Is_Known
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Boolean;
--
-- Is_Valid -- Check if the handle is associated with an object
--
--    Lesson - The handle
--
-- Returns :
--
--    True if the handle can de dereferenced
--
   function Is_Valid (Lesson : Lecture_Handle) return Boolean;
--
-- Ptr -- Get the pointer to the object by a handle
--
--    Lesson - The handle
--
-- Returns :
--
--    The referenced object
--
   function Ptr (Lesson : Lecture_Handle) return Lecture_Object_Ptr;
--
-- Put -- Set image for a feature
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - A handle to the feature in the example
--    Image   - The image to be set
--    Value   - Its value
--
-- This procedure sets the specified by the parameter Image image of the
-- training example Example for the feature Feature into the set Lesson.
-- The  value  of  the  image  is specified by the parameter Value. When
-- Lesson  is  an  invalid  handle,  a  memory-resident  training set is
-- created and Lesson will point to it.
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality of Value, invalid Feature
--    Data_Error       - I/O error
--
   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Image   : Fuzzy.Feature.Image_Type;
                Value   : Set
             );
--
-- Put -- Set classification or intuitionistic set for a feature
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - A handle to the feature in the example
--    Value   - The value, an intuitionistic classification
--
-- This  procedure  sets  Value  of Feature for Example into Lesson. The
-- value  is  an  intuitionistic  classification of Example in Feature's
-- domain set. A classification defines the images Has_In  and  Has_Not.
-- An intuitionistic set does the images Has_In and Has_Out. When Lesson
-- is  an  invalid handle, a memory-resident training set is created and
-- Lesson will point to it.
--
-- Exceptions :
--
--    Constraint_Error - Illegal cardinality of Value, invalid Feature
--    Data_Error       - I/O error
--
   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Value   : Classification
             );
--
-- Put -- Modify image for a feature (one point)
--
--    Lesson  - A handle to a teaching set
--    Example - The number of an example in the set
--    Feature - The handle of the feature in the example
--    Image   - The image to be modified
--    Value   - The domain point in the distribution to be set
--
-- This procedure sets only one point in the image. When a point  of  an
-- undefined  image  is  set,  other points are initialized with 0. When
-- Lesson  is  an  invalid  handle,  a  memory-resident  training set is
-- created and Lesson will point to it.
--
-- Exceptions :
--
--    Constraint_Error - Illegal Value, invalid Feature
--    Data_Error       - I/O error
--
   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Image   : Fuzzy.Feature.Image_Type;
                Value   : Positive
             );
--
-- Ref -- Get handle to a lecture object
--
--    Feature - The lecture object
--
-- Returns :
--
--    Handle to the object
--
   function Ref (Lesson : Lecture_Object_Ptr) return Lecture_Handle;
--
-- Ref -- Set a handle to a training set object
--
--    Handle - The handle to set
--    Lesson - The object
--
   procedure Ref
             (  Handle  : in out Lecture_Handle;
                Lesson : Lecture_Object_Ptr
             );
--
-- Set_Undefined -- Reset the images
--
--    Lesson    - A teaching set
--  [ Example ] - The number of the example in the set
--    Feature   - The feature in the example (a pointer to)
--
-- These procedures set the images of the feature  undefined.  When  the
-- parameter  Example  is omitted, then all examples of the training set
-- corresponding to Feature are reset. This  has  the  effect  that  the
-- training  set  will  no  more reference Feature (which can be removed
-- then, if that was the last  reference  to  it).  When  Lesson  is  an
-- invalid handle, the operation is void.
--
-- Exceptions :
--
--    Constraint_Error - Invalid Feature
--    Data_Error       - I/O error
--
   procedure Set_Undefined
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle
             );
   procedure Set_Undefined
             (  Lesson  : in out Lecture_Handle;
                Feature : Feature_Handle
             );
--
-- To_Deposit_Handle -- Pointer conversion
--
--    Lesson - A handle to
--
-- Returns :
--
--    Handle to archived object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function To_Deposit_Handle (Lesson : Lecture_Handle)
      return Deposit_Handles.Handle;
--
-- To_Lecture_Handle -- Handle conversion
--
--    Lesson - A handle to
--
-- Returns :
--
--    A handle to the training set
--
-- Exceptions :
--
--    Constraint_Error - The object is not a set, or invalid handle
--
   function To_Lecture_Handle
            (  Lesson : Deposit_Handles.Handle
            )  return Lecture_Handle;

private
   pragma Inline (Copy);
   pragma Inline (Get);
   pragma Inline (Get_Examples_Number);
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features);
   pragma Inline (Get_Features_Number);
   pragma Inline (Invalidate);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Valid);
   pragma Inline (Ptr);
   pragma Inline (Put);
   pragma Inline (Ref);
   pragma Inline (Ref);
   pragma Inline (To_Deposit_Handle);
   pragma Inline (To_Lecture_Handle);

   type Lecture_Handle is new Handles.Handle with null record;

end Fuzzy.Lecture.Handle;
