--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Subrange                      Luebeck            --
--  Interface                                      Autumn, 2010       --
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
--  The subrange training set selects a range of examples from the given
--  set.
--
with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;

package Fuzzy.Lecture.Subrange is
   pragma Elaborate_Body (Fuzzy.Lecture.Subrange);
--
-- Class -- Name of the class of range training sets
--
   Class : constant String := Lecture_Class & "Subrange";
--
-- Create -- A training set, which is a subrange of a reference set
--
--    Source - The reference set
--    From   - The first example number to include
--    To     - The last example number to include
--
-- The  training  set  contains  the  examples  From..To  of Source. The
-- parameter To may be less than From to indicate empty  range.  It  can
-- also be greater than the total examples number  of  Source.  Updating
-- the  training  set also does the source set. An attempt to modify the
-- Source set outside  the examples range  causes  Use_Error  exception.
-- Examples outside  the  range  when  read  considered  undefined.  The
-- training subrange set are enumerated from 1.
--
-- Returns :
--
--    Handle to the set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create
            (  Source : Lecture_Handle;
               From   : Positive;
               To     : Natural := Positive'Last
            )  return Lecture_Handle;

private
   type Subrange_Lecture_Object;
--
-- Source_Observer -- Used to monitor notifications of the reference set
--                    in order to mirror them in the subrange set
--
   type Source_Observer
        (  Subrange : not null access Subrange_Lecture_Object'Class;
           Lesson   : not null access Lecture_Object'Class
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
-- Subrange_Lecture_Object -- The teaching set type
--
   type Subrange_Lecture_Object
        (  Offset : Natural;
           Size   : Natural
        )  is new Lecture_Object with
   record
      Source   : Lecture_Handle;
      Observer : Source_Observer_Ptr;
   end record;
   type Subrange_Lecture_Object_Ptr is access all Subrange_Lecture_Object;
--
-- Finalize -- Destruction
--
   overriding
   procedure Finalize (Lesson : in out Subrange_Lecture_Object);
--
-- Get -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Lesson : Subrange_Lecture_Object) return String;
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Examples_Number (Lesson : Subrange_Lecture_Object)
      return Natural;
--
-- Get_Feature -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Feature
            (  Lesson : Subrange_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle;
--
-- Get_Feature -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features (Lesson : Subrange_Lecture_Object)
      return Bounded_Array;
--
-- Get_Features_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features_Number (Lesson : Subrange_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Lesson : Subrange_Lecture_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Is_Defined - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Defined
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Known
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Modified - Object.Archived...
--
   overriding
   function Is_Modified (Lesson : Subrange_Lecture_Object)
      return Boolean;
--
-- Put -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out Subrange_Lecture_Object;
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
             (  Lesson  : in out Subrange_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             );
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Lesson : in out Subrange_Lecture_Object);
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
             (  Lesson  : in out Subrange_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Subrange_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Subrange_Lecture_Object
             );

   pragma Inline (Added);
   pragma Inline (Changed);
   pragma Inline (Deleted);
   pragma Inline (Get);
   pragma Inline (Get_Examples_Number);
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features);
   pragma Inline (Get_Features_Number);
   pragma Inline (Get_Class);
   pragma Inline (Is_Known);
   pragma Inline (Is_Defined);
   pragma Inline (Renamed);
   pragma Inline (Set_Undefined);

end Fuzzy.Lecture.Subrange;
