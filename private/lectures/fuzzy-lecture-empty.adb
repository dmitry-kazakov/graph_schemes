--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Empty                         Luebeck            --
--  Implementation                                 Autumn, 2006       --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Fuzzy.Lecture.Handle;  use Fuzzy.Lecture.Handle;

package body Fuzzy.Lecture.Empty is

   Object : constant Lecture_Object_Ptr := new Empty_Lecture_Object;
   Handle : Lecture_Handle := Ref (Object);

   function Empty_Set return Lecture_Object_Ptr is
   begin
      return Object;
   end Empty_Set;

   function Get
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
   begin
      return (1..Feature.Cardinality => Confidence'Last);
   end Get;

   function Get_Class (Lesson : Empty_Lecture_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : Empty_Lecture_Object)
      return Natural is
   begin
      return 0;
   end Get_Examples_Number;

   function Get_Feature
            (  Lesson : Empty_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle is
   begin
      raise Constraint_Error;
      return No_Feature;
   end Get_Feature;

   function Get_Features (Lesson : Empty_Lecture_Object)
      return Bounded_Array is
      Result : Bounded_Array (1, 0);
   begin
      return Result;
   end Get_Features;

   function Get_Features_Number (Lesson : Empty_Lecture_Object)
      return Natural is
   begin
      return 0;
   end Get_Features_Number;

   function Is_Defined
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return False;
   end Is_Defined;

   function Is_Known
            (  Lesson  : Empty_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return False;
   end Is_Known;

   function Is_Modified (Lesson : Empty_Lecture_Object)
      return Boolean is
   begin
      return False;
   end Is_Modified;

   procedure Put
             (  Lesson  : in out Empty_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
   begin
      Raise_Exception
      (  Data_Error'Identity,
         "Writing an empty training sets"
      );
   end Put;

   procedure Put
             (  Lesson  : in out Empty_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
   begin
      Raise_Exception
      (  Data_Error'Identity,
         "Writing an empty training sets"
      );
   end Put;

   procedure Reset_Modified (Lesson : in out Empty_Lecture_Object) is
   begin
      null;
   end Reset_Modified;

   procedure Set_Undefined
             (  Lesson  : in out Empty_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
   begin
      null;
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out Empty_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
   begin
      null;
   end Set_Undefined;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             )  is
   begin
      Lesson := To_Deposit_Ptr (Object);
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Empty_Lecture_Object
             )  is
   begin
      null;
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Lecture.Empty;
