--                                                                    --
--  package Fuzzy.Lecture           Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2002       --
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
--  This  package  defines  the  type  Lecture_Object.  An  instance  of
--  Lecture_Object can be viewed as a  table.  Rows  of  the  table  are
--  training  examples.  Columns  are features used in the training set.
--  Each cell of the table is  a  value  of  the  corresponding  feature
--  (column)  in  the  given example (row). Each value consists of up to
--  four distributions of the possibilities over feature. domain.  These
--  distributions can be independently accessed for read and  write.  If
--  an  example or feature is not yet in the set when a distribution has
--  to be written then the corresponding row or  column  is  added.  The
--  type has no public methods, see the public child package Handle.
--
with Indicator;             use Indicator;
with Fuzzy.Feature;         use Fuzzy.Feature;
with Fuzzy.Feature.Handle;  use Fuzzy.Feature.Handle;
with Object.Archived;       use Object.Archived;

with Ada.Unchecked_Conversion;
with Object.Handle;

with Fuzzy.Feature.Handle.Bounded_Arrays;
use  Fuzzy.Feature.Handle.Bounded_Arrays;

package Fuzzy.Lecture is
   pragma Elaborate_Body (Fuzzy.Lecture);
--
-- Lecture_Class -- The prefix of all lecture object classes
--
   Lecture_Class : constant String := "Lecture.";

   type Lecture_Object is abstract new Deposit with private;
   type Lecture_Object_Ptr is access Lecture_Object'Class;
   for Lecture_Object_Ptr'Storage_Pool use Deposit_Ptr'Storage_Pool;
--
-- Begin_Bulk_Update -- Initiate bulk update
--
--    Lesson - A training set
--
-- This procedure initiates a bulk update of the training set. A call to
-- Begin_Bulk_Update    is    matched    but    End_Bulk_Update.    Some
-- implementations  may  take  advantage  of  knowing  about an incoming
-- massive update, for example when the training set  implementation  is
-- based on transactions. Begin_Bulk_Update would open a transaction and
-- End_Bulk_Update would commit it.  The  implementation  may  propagate
-- Use_Error   for   nested  calls  to  Begin_Bulk_Update.  The  default
-- implementation does nothing.
--
-- Exceptions :
--
--    Data_Error - I/O error
--    Use_Error  - Nested update
--
   procedure Begin_Bulk_Update (Lesson : in out Lecture_Object);
--
-- Copy -- Examples of a training set
--
--    Target - The training set to add the examples to
--    Source - The training set to copy examples from
--    From   - The first example to copy
--    To     - The last example to copy
--    Viewer - A progress indication object
--
-- This procedure copies all examples of the training  set  Source  into
-- the set Target.
--
-- Exceptions :
--
--    Data_Error - I/O error
--    End_Error  - Operation was aborted
--
   procedure Copy
             (  Target : in out Lecture_Object'Class;
                Source : Lecture_Object'Class;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             );
--
-- End_Bulk_Update -- End block update
--
--    Lesson - A training set
--
-- This  procedure  ends  block  update of the training set initiated by
-- Begin_Bulk_Update. The default implementation does nothing.
--
-- Exceptions :
--
--    Data_Error - I/O error
--    Use_Error  - No update active
--
   procedure End_Bulk_Update (Lesson : in out Lecture_Object);
--
-- Finalize -- Destructor
--
--    Lesson - The training set
--
   procedure Finalize (Lesson : in out Lecture_Object);
--
-- Generic_Restore -- A generic implementation of Object.Archived...
--
-- This   is   a  generic  (class-wide)  variant  which  works  for  any
-- implementation  of training set. It can be called from an override of
-- Object.Archived.Restore.
--
   procedure Generic_Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : in out Lecture_Object'Class
             );
--
-- Generic_Store -- A generic implementation of Object.Archived...
--
-- This   is   a  generic  (class-wide)  variant  which  works  for  any
-- implementation  of training set. It can be called from an override of
-- Object.Archived.Store.
--
   procedure Generic_Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Lecture_Object'Class
             );
--
-- Get -- Get image of an example
--
--    Lesson  - A training set
--    Example - The number of an example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to get
--
-- This  function  returns  the distribution of the possibilities of the
-- requested image. There are four characteristic images, the  parameter
-- Image  specifies  which one has to be returned. The result is a fuzzy
-- subset of the domain of Feature. The parameter Example is the example
-- number. The querying mechanism is used for undefined values.
--
-- Returns :
--
--    The image
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get
            (  Lesson  : not null access Lecture_Object'Class;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get -- Get image of an example
--
--    Lesson  - A training set
--    Example - The number of an example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to get
--
-- This function returns the requested image for Feature in the  example
-- Example.  This  function  should not be called directly. A class-wide
-- variant has to be used instead.
--
-- Returns :
--
--    The image
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get
            (  Lesson  : Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is abstract;
--
-- Get_Examples_Number -- Get the number of examples in a set
--
--    Lesson  - The training set
--
-- Returns :
--
--    The number of examples in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Examples_Number (Lesson : Lecture_Object)
      return Natural is abstract;
--
-- Get_Feature -- Get a feature in a set
--
--    Lesson - The training set
--    Index  - Of the feature 1..Get_Features_Number
--
-- Returns :
--
--    A handle to the feature
--
-- Exceptions :
--
--    Constraint_Error - Illegal index
--    Data_Error       - I/O error
--
   function Get_Feature (Lesson : Lecture_Object; Index : Positive)
      return Feature_Handle is abstract;
--
-- Get_Features -- Get the features in a set
--
--    Lesson - The training set
--
-- A feature is in the set if the set defines at least one image of this
-- feature.  This function returns the array of features in the set. The
-- order of features in the array reflects their order in  the  set  (if
-- any).
--
-- Returns :
--
--    The features in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Features (Lesson : Lecture_Object)
      return Bounded_Array is abstract;
--
-- Get_Features_Number -- Get the number of features in a set
--
--    Lesson - The training set
--
-- A feature is in the set if the set defines at least one image of this
-- feature.
--
-- Returns :
--
--    The number of features in the set
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Get_Features_Number (Lesson : Lecture_Object)
      return Natural is abstract;
--
-- Get_Number_Of_Lectures -- Get total number of lectures
--
-- Returns :
--
--    The number of lectures existing at the moment
--
   function Get_Number_Of_Lectures return Natural;
--
-- Initialize -- Constructor
--
--    Lesson - The training set
--
   procedure Initialize (Lesson : in out Lecture_Object);
--
-- Is_Defined - Check if the set contains data
--
--    Lesson  - A training set
--    Example - The number of an example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to get
--
-- This function can be used to query whether an image is defined by the
-- training  set.  Undefined  images when  requested  are  returned   as
-- "unknown" by the Get-function, i.e. as a subset with all 1. See  also
-- Query which is called for an  undefined  image  to  get  it  from  an
-- external source.
--
-- Returns :
--
--    True if the value of the feature is present in the lecture example
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Is_Defined
            (  Lesson  : Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is abstract;
--
-- Is_Known - Check image of an example (class-wide)
--
--    Lesson  - A training set
--    Example - The number of the example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to check
--
-- This  function checks if the specified image is unknown, i.e. has all
-- values  of  the membership function 1. The querying mechanism is used
-- for undefined values.
--
-- Returns :
--
--    False if Get (Lesson, Example, Feature, Image) would return all 1
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Is_Known
            (  Lesson  : not null access Lecture_Object'Class;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Check image of an example
--
--    Lesson  - A training set
--    Example - The number of the example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to check
--
-- This  function checks if the specified image is unknown, i.e. has all
-- values of the membership function 1. This function  should  never  be
-- called directly but through its class-wide proxy.
--
-- Returns :
--
--    False if Get (Lesson, Example, Feature, Image) would return all 1
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   function Is_Known
            (  Lesson  : Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is abstract;
--
-- Put -- Set an image for a feature
--
--    Lesson  - A training set
--    Example - The number of an example in the set
--    Feature - The feature in the example
--    Image   - The characteristic to set
--    Value   - The distribution to be set
--
-- This  procedure  sets  the  specified image of Feature in the example
-- Example. The image is specified by the parameter Image. Initially the
-- distribution is all 1 (unknown).
--
-- Exceptions :
--
--    Constraint_Error - Illegal Value cardinality or invalid Feature
--    Data_Error       - I/O error
--
   procedure Put
             (  Lesson  : in out Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is abstract;
--
-- Put -- Set one image point for a feature
--
--    Lesson  - A training set
--    Example - The number of an example in the set
--    Feature - The feature in the example
--    Value   - The domain point in the distribution to be set
--
-- This procedure sets only one point in the image. When a point  of  an
-- undefined image is set, other points are initialized with 0.
--
-- Exceptions :
--
--    Constraint_Error - Illegal Value or Feature is invalid
--    Data_Error       - I/O error
--
   procedure Put
             (  Lesson  : in out Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is abstract;
--
-- Query -- Value request notification
--
--    Lesson  - A training set
--    Example - The number of the example requested
--    Feature - The feature in the example
--
-- This procedure is used to query the data missing in the training set.
-- It is called when the data are missing (this does the class-wide  Get
-- and Is_Known functions). The  callee  may  get  the  data  from  some
-- external source and put it into the training set using an appropriate
-- Put-procedure. In the base  type  the  procedure  has  no  effect.  A
-- derived  type  may  override  it to provide a user dialogue or a data
-- base support.
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   procedure Query
             (  Lesson  : in out Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
--
-- Set_Undefined -- Reset the images
--
--    Lesson    - A training set
--  [ Example ] - The number of the example in the set
--    Feature   - The feature in the example
--
-- This  procedure  sets  the  images of the feature undefined. When the
-- parameter  Example  is omitted, then all examples of the training set
-- corresponding to Feature are reset. This  has  the  effect  that  the
-- training  set  will  no  more reference Feature (which can be removed
-- then, if that was the last reference to it).
--
-- Exceptions :
--
--    Data_Error - I/O error
--
   procedure Set_Undefined
             (  Lesson  : in out Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is abstract;
   procedure Set_Undefined
             (  Lesson  : in out Lecture_Object;
                Feature : Feature_Object'Class
             )  is abstract;
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
          (  Lecture_Object_Ptr,
             Deposit_Ptr
          );
--
-- To_Lecture_Object_Ptr -- Pointer conversion
--
--    Ptr - Pointer to be converted
--
-- Returns :
--
--    Pointer to feature
--
-- Exceptions :
--
--    Constraint_Error - The pointed object is not a lecture
--
   function To_Lecture_Object_Ptr (Ptr : Deposit_Ptr)
      return Lecture_Object_Ptr;
--
-- Lecture_Observer -- An observer of lecture changes
--
   type Lecture_Observer
        (  Lesson : not null access Lecture_Object'Class
        )  is abstract new Object.Entity with private;
   type Lecture_Observer_Ptr is access all Lecture_Observer'Class;
--
-- Added -- Notification of feature addition
--
--    Observer - The observer
--    Lesson   - The training set
--    Feature  - The feature in the example
--
   procedure Added
             (  Observer : in out Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is abstract;
--
-- Changed -- Notification of lecture example change
--
--    Observer - The observer
--    Lesson   - The training set
--    Example  - The number of an example in the set
--    Feature  - The feature in the example
--    Image    - The characteristic to set
--
   procedure Changed
             (  Observer : in out Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             )  is abstract;
--
-- Deleted -- Notification of lecture example or feature deletion
--
--    Observer  - The observer
--    Lesson    - The training set
--  [ Example ] - The number of an example in the set
--    Feature   - The feature in the example
--
   procedure Deleted
             (  Observer : in out Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             )  is abstract;
   procedure Deleted
             (  Observer : in out Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is abstract;
--
-- Finalize -- To be called when overridden
--
--    Observer - The observer
--
   procedure Finalize (Observer : in out Lecture_Observer);
--
-- Initialize -- To be called when overridden
--
--    Observer - The observer
--
   procedure Initialize (Observer : in out Lecture_Observer);
--
-- Notify_Changed -- Notification
--
--    Lesson  - A training set
--    Example - The number of the example in the set
--    Feature - The feature in the example
--    Image   - The characteristic set
--
-- This procedure is to be called from the implementation of Put.
--
   procedure Notify_Changed
             (  Lesson  : in out Lecture_Object'Class;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type
             );
--
-- Notify_New -- Notification
--
--    Lesson  - A training set
--    Feature - The feature newly added to the training set
--
-- This procedure is to be called from the implementations of  Put  that
-- add a new feature.
--
   procedure Notify_New
             (  Lesson  : in out Lecture_Object'Class;
                Feature : Feature_Object'Class
             );
--
-- Notify_Renamed -- Notification
--
--    Lesson   - A training set
--    Feature  - The feature renamed
--    Old_Name - The old feature name
--    New_Name - The new feature name
--
-- This   procedure   is  should  be  called  by  an  implementation  of
-- Lecture_Object.  Usually it will monitor the features in the training
-- set using a list of Feature_Link (see Object.Archived.Feature).
--
   procedure Notify_Renamed
             (  Lesson   : in out Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             );
--
-- Notify_Undefined -- Notification
--
--    Lesson    - A training set
--  [ Example ] - The number of the example in the set
--    Feature   - The feature in the example
--
-- These procedures  are  to  be  called  from  the  implementations  of
-- Set_Undefined.
--
   procedure Notify_Undefined
             (  Lesson  : in out Lecture_Object'Class;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   procedure Notify_Undefined
             (  Lesson  : in out Lecture_Object'Class;
                Feature : Feature_Object'Class
             );
--
-- Renamed -- Notification of feature renaming
--
--    Observer - The observer
--    Lesson   - The training set
--    Feature  - The feature renamed
--    Old_Name - The old feature name
--    New_Name - The new feature name
--
   procedure Renamed
             (  Observer : in out Lecture_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             )  is abstract;
private
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features_Number);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Known);
   pragma Inline (To_Lecture_Object_Ptr);

   type Lecture_Observer
        (  Lesson : not null access Lecture_Object'Class
        )  is abstract new Object.Entity with
   record
      Prev : Lecture_Observer_Ptr;
      Next : Lecture_Observer_Ptr;
   end record;

   procedure Insert (Observer : in out Lecture_Observer);
   procedure Remove (Observer : in out Lecture_Observer);

   pragma Inline (Insert);
   pragma Inline (Remove);

   type Lecture_Object is abstract new Deposit with record
      Observers : Lecture_Observer_Ptr;
   end record;

   generic
      with procedure Process (Observer : in out Lecture_Observer'Class);
   procedure Generic_Notify (Lesson : in out Lecture_Object'Class);
--
-- Handles -- To Lecture_Objects
--
   package Handles is
      new Object.Handle (Lecture_Object, Lecture_Object_Ptr);

end Fuzzy.Lecture;
