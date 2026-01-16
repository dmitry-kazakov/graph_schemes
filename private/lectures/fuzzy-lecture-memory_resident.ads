--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Memory_Resident               Luebeck            --
--  Interface                                      Winter, 2002       --
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
--  This package  defines  General_Lecture_Object,  the  type  of  fuzzy
--  training  set type which may hold training examples of any type. The
--  whole set  is  memory  allocated.  The  implementation  can  get  an
--  advantage if the realizations of a feature are singletons.
--
with Fuzzy.Lecture.Block;       use Fuzzy.Lecture.Block;
with Fuzzy.Lecture.Connotated;  use Fuzzy.Lecture.Connotated;

with Generic_Unbounded_Ptr_Array;

package Fuzzy.Lecture.Memory_Resident is
   pragma Elaborate_Body (Fuzzy.Lecture.Memory_Resident);
--
-- Class -- Name of the class of memory-resident training sets
--
   Class : constant String := Lecture_Class & "General";
--
-- General_Lecture_Object -- Memory-resident training set
--
   type General_Lecture_Object;
--
-- Unbounded array of pointers to Column_Block
--
   package Column_Block_Ptr_Unbounded_Array is
      new Generic_Unbounded_Ptr_Array
          (  Positive,
             Column_Block'Class,
             Column_Block_Ptr,
             Column_Block_Ptr_Array
          );
   type Column_Block_Ptr_Unbounded_Array_Ptr is
      access Column_Block_Ptr_Unbounded_Array.Unbounded_Ptr_Array;
   type Column_Block_Ptr_Unbounded_Array_Ptr_Array is
      array (Feature_Index range <>)
         of Column_Block_Ptr_Unbounded_Array_Ptr;
--
-- Unbounded array of pointers to pointers to Column_Block
--
   package Column_Ptr_Array is
      new Generic_Unbounded_Ptr_Array
          (  Feature_Index,
             Column_Block_Ptr_Unbounded_Array.Unbounded_Ptr_Array,
             Column_Block_Ptr_Unbounded_Array_Ptr,
             Column_Block_Ptr_Unbounded_Array_Ptr_Array
          );
   type Characteristics is
      array (Fuzzy.Feature.Image_Type) of
         Column_Ptr_Array.Unbounded_Ptr_Array;

   type Lecture_Self_Ptr is access all General_Lecture_Object'Class;
--
-- General_Lecture_Object -- The training set type
--
   type General_Lecture_Object is
      new Connotated_Lecture_Object with
   record
      Self : Lecture_Self_Ptr :=
                General_Lecture_Object'Unchecked_Access;
      Rows : Natural := 0;
      Data : Characteristics;
   end record;
--
-- Add_Feature -- Overrides Fuzzy.Lecture.Connotated...
--
   overriding
   function Add_Feature
            (  Lesson  : not null access General_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle;
--
-- Get -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Lesson : General_Lecture_Object) return String;
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Examples_Number (Lesson : General_Lecture_Object)
      return Natural;
--
-- Is_Defined - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Defined
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Defined -- A version that distinguishes undefined and uncertain
--
--    Lesson  - The training set
--    Example - To set unknown
--    Feature - The feature
--    Image   - Of the example
--
-- Returns :
--
--    Status of the value
--
   function Is_Defined
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Value_Status;
--
-- Is_Known - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Known
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Put -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out General_Lecture_Object;
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
             (  Lesson  : in out General_Lecture_Object;
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
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   overriding
   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Set_Undefined -- Set whole example undefined
--
--    Lesson   - The training set
--    Example  - To set unknown
--  [ Feature  - The feature
--  [ Image ]] - The image
--    Status   - Uncertain or Undefined
--
   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Status  : Undefined_Status
             );
   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Status  : Undefined_Status
             );
   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Status  : Undefined_Status
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : General_Lecture_Object
             );
private
   pragma Inline (Get_Class);
   pragma Inline (Get_Examples_Number);

end Fuzzy.Lecture.Memory_Resident;
