--                                                                    --
--  package Test_Caching_Lecture    Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Summer, 2010       --
--                                                                    --
--                                Last revision :  12:48 30 Aug 2010  --
--                                                                    --
--  This  library  is  free software; you can redistribute it and/or  --
--  modify it under the terms of  the  GNU  Library  General  Public  --
--  License  as  published  by  the Free Software Foundation; either  --
--  version 2 of the License, or (at your option) any later version.  --
--                                                                    --
--  This  library is distributed in the hope that it will be useful,  --
--  but WITHOUT ANY WARRANTY; without even the implied  warranty  of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU  --
--  Library General Public License for more details.                  --
--                                                                    --
--  You  should  have  received  a  copy  of the GNU Library General  --
--  Public  License  along  with  this library; if not, write to the  --
--  Free Software Foundation, Inc.,  675  Mass  Ave,  Cambridge,  MA  --
--  02139, USA.                                                       --
--____________________________________________________________________--

with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Lecture.Cache;            use Fuzzy.Lecture.Cache;
with Fuzzy.Lecture.Connotated;       use Fuzzy.Lecture.Connotated;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Memory_Resident;  use Fuzzy.Lecture.Memory_Resident;

package Test_Caching_Lecture is

   type Test_Caching is new Caching_Lecture_Object with record
      Store : General_Lecture_Object;
   end record;

   function Create (Size : Positive) return Lecture_Handle;
   function Add_Feature
            (  Lesson  : access Test_Caching;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle;
   function Get_Class (Object : Test_Caching) return String;
   function Get_Examples_Number (Lesson : Test_Caching) return Natural;
   function Raw_Get
            (  Lesson  : Test_Caching;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set;
   function Raw_Is_Defined
            (  Lesson  : Test_Caching;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
   procedure Raw_Put
             (  Lesson  : in out Test_Caching;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             );
   procedure Raw_Set_Undefined
             (  Lesson  : in out Test_Caching;
                Feature : Feature_Object'Class
             );
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Test_Caching
             );
   procedure Drop (Lesson : Lecture_Handle);
   procedure Write (Lesson : Lecture_Handle);

end Test_Caching_Lecture;
