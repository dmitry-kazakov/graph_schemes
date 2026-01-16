--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Composite                     Luebeck            --
--  Interface                                      Autumn, 2010       --
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
--
--  The subrange training set selects a range of examples from the given
--  set.
--
with Fuzzy.Lecture.Connotated;  use Fuzzy.Lecture.Connotated;
with Fuzzy.Lecture.Handle;      use Fuzzy.Lecture.Handle;

package Fuzzy.Lecture.Composite is
   pragma Elaborate_Body (Fuzzy.Lecture.Composite);
--
-- Feature_To_Lecture_Maps -- Feature to training set map
--
   type Feature_To_Lecture_Pair is record
      Feature : Feature_Handle;
      Lecture : Lecture_Handle;
   end record;
   type Feature_To_Lecture_Map is
      array (Positive range <>) of Feature_To_Lecture_Pair;
--
-- Class -- Name of the class of range training sets
--
   Class : constant String := Lecture_Class & "Composite";
--
-- Create -- A training set composed of reference sets
--
--    Mapping - The list or set of features of the result set
--
-- The training set contains the examples from the sets specified in the
-- Mapping.  The parameter Mapping is an array of feature - training set
-- pairs. For each feature in the array the result set contains examples
-- from the corresponding set for this feature.
--
-- Returns :
--
--    Handle to the set
--
-- Exceptions :
--
--    Constraint_Error - Invalid lecture handle
--    Data_Error       - Not unique features
--
   function Create (Mapping : Feature_To_Lecture_Map)
      return Lecture_Handle;

private
   type Composite_Lecture_Object;
--
-- Source_Observer -- Used to monitor notifications of the reference set
--                    in order to mirror them in the subrange set
--
   type Source_Observer
        (  Composite : not null access Composite_Lecture_Object'Class;
           Lesson    : not null access Lecture_Object'Class
        )  is new Lecture_Observer (Lesson) with null record;
   type Source_Observer_Ptr is access Source_Observer;
   procedure Added
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
   procedure Changed
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             );
   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             );
   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             );
   procedure Renamed
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             );
--
-- Column_Descriptor -- The feature descriptor
--
   type Column_Descriptor is new Feature_Descriptor with record
      Source   : Lecture_Handle;
      Observer : Source_Observer_Ptr;
   end record;
   type Column_Descriptor_Ptr is access all Column_Descriptor;
   procedure Finalize (Item : in out Column_Descriptor);
--
-- Composite_Lecture_Object -- The training set type
--
   type Composite_Lecture_Object is
      new Connotated_Lecture_Object with null record;
--
-- Add_Feature -- Overrides Fuzzy.Lecture.Connotated...
--
   overriding
   function Add_Feature
            (  Lesson  : not null access Composite_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle;
--
-- Get -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Lesson : Composite_Lecture_Object) return String;
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Examples_Number (Lesson : Composite_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Lesson : Composite_Lecture_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Get_Source -- Get training set by feature
--
--    Lesson  - The composite lecture object
--    Feature - The feature object
--
-- Returns :
--
--    Source lecture object pointer or else null
--
   function Get_Source
            (  Lesson  : Composite_Lecture_Object'Class;
               Feature : Feature_Object'Class
            )  return Lecture_Object_Ptr;
--
-- Is_Defined - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Defined
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Known
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Put -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             );
--
-- Put -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             );
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             );
--
-- Set_Undefined -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Composite_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Composite_Lecture_Object
             );
   pragma Inline (Added);
   pragma Inline (Changed);
   pragma Inline (Deleted);
   pragma Inline (Get);
   pragma Inline (Get_Class);
   pragma Inline (Get_Examples_Number);
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features);
   pragma Inline (Get_Features_Number);
   pragma Inline (Get_Source);
   pragma Inline (Is_Known);
   pragma Inline (Is_Defined);
   pragma Inline (Renamed);
   pragma Inline (Set_Undefined);

end Fuzzy.Lecture.Composite;
