--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Empty                         Luebeck            --
--  Interface                                      Autumn, 2006       --
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
--  This  package  provides  empty  training sets. No example can be put
--  into an empty set. 
--
package Fuzzy.Lecture.Empty is
   pragma Elaborate_Body (Fuzzy.Lecture.Empty);
--
-- Class -- Name of the class of memory-resident training sets
--
   Class : constant String := Lecture_Class & "Empty";
--
-- Empty_Lecture_Object -- The teaching set type
--
   type Empty_Lecture_Object is new Lecture_Object with null record;
--
-- Empty_Set -- Get an empty training set
--
-- Returns :
--
--    Pointer to the object
--
   function Empty_Set return Lecture_Object_Ptr;
--
-- Get -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class (Lesson : Empty_Lecture_Object) return String;
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Examples_Number (Lesson : Empty_Lecture_Object)
      return Natural;
--
-- Get_Feature -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Feature
            (  Lesson : Empty_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle;
--
-- Get_Features -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features (Lesson : Empty_Lecture_Object)
      return Bounded_Array;
--
-- Get_Features_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features_Number (Lesson : Empty_Lecture_Object)
      return Natural;
--
-- Is_Defined - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Defined
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known - Overrides Fuzzy.Lecture...
--
   overriding
   function Is_Known
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Lesson : Empty_Lecture_Object)
      return Boolean;
--
-- Put -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out Empty_Lecture_Object;
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
             (  Lesson  : in out Empty_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             );
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Lesson : in out Empty_Lecture_Object);
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
             (  Lesson  : in out Empty_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Empty_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Empty_Lecture_Object
             );
private
   pragma Inline (Empty_Set);
   pragma Inline (Get);
   pragma Inline (Get_Class);
   pragma Inline (Get_Examples_Number);
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features_Number);
   pragma Inline (Is_Defined);
   pragma Inline (Is_Known);
   pragma Inline (Is_Modified);
   pragma Inline (Put);
   pragma Inline (Put);
   pragma Inline (Reset_Modified);
   pragma Inline (Restore);
   pragma Inline (Set_Undefined);
   pragma Inline (Store);
   
end Fuzzy.Lecture.Empty;
