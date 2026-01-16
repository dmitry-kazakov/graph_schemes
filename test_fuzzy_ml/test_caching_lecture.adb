--                                                                    --
--  package Test_Caching_Lecture    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Summer, 2010       --
--                                                                    --
--                                Last revision :  18:58 25 Jul 2018  --
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

package body Test_Caching_Lecture is

   function Add_Feature
            (  Lesson  : access Test_Caching;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      Result : Feature_Descriptor_Handles.Handle;
   begin
      return Result;
   end Add_Feature;

   function Create (Size : Positive) return Lecture_Handle is
      Result : constant Lecture_Handle :=
                        Ref (new Test_Caching (Cache_Index (Size)));
   begin
      return Result;
   end Create;

   procedure Drop (Lesson : Lecture_Handle) is
   begin
      Drop (Test_Caching (Ptr (Lesson).all));
   end Drop;

   function Get_Class (Object : Test_Caching) return String is
   begin
      return Get_Class (Object.Store);
   end Get_Class;

   function Get_Examples_Number (Lesson : Test_Caching)
      return Natural is
   begin
      return Get_Examples_Number (Lesson.Store);
   end Get_Examples_Number;

   function Raw_Get
            (  Lesson  : Test_Caching;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set is
   begin
      return Get (Lesson.Store, Example, Feature, Image);
   end Raw_Get;

   function Raw_Is_Defined
            (  Lesson  : Test_Caching;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Defined (Lesson.Store, Example, Feature, Image);
   end Raw_Is_Defined;

   procedure Raw_Put
             (  Lesson  : in out Test_Caching;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             )  is
   begin
      Put (Lesson.Store, Example, Feature, Image, Value);
   end Raw_Put;

   procedure Raw_Set_Undefined
             (  Lesson  : in out Test_Caching;
                Feature : Feature_Object'Class
             )  is
   begin
      Set_Undefined (Lesson.Store, Feature);
   end Raw_Set_Undefined;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Object      : Test_Caching
             )  is
   begin
      Store (Destination, Pointer, Object.Store);
   end Store;

   procedure Write (Lesson : Lecture_Handle) is
   begin
      Write (Test_Caching (Ptr (Lesson).all));
   end Write;

end Test_Caching_Lecture;

