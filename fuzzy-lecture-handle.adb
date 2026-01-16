--                                                                    --
--  package Fuzzy.Lecture.Handle    Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
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

with Fuzzy.Feature;                  use Fuzzy.Feature;
with Fuzzy.Lecture.Memory_Resident;  use Fuzzy.Lecture.Memory_Resident;

package body Fuzzy.Lecture.Handle is

   procedure Copy
             (  Target : in out Lecture_Handle;
                Source : Lecture_Handle;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      This : Lecture_Object_Ptr := Ptr (Target);
      That : constant Lecture_Object_Ptr := Ptr (Source);
   begin
      if That = null then
         return;
      end if;
      if This = null then
         This := new General_Lecture_Object;
         Set (Target, This);
      end if;
      Copy (This.all, That.all, From, To, Viewer);
   end Copy;

   procedure Copy
             (  Target : in out Lecture_Handle;
                Source : Lecture_Handle;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : Indicator_Handle
             )  is
      This : Lecture_Object_Ptr := Ptr (Target);
      That : constant Lecture_Object_Ptr := Ptr (Source);
   begin
      if That = null then
         return;
      end if;
      if This = null then
         This := new General_Lecture_Object;
         Set (Target, This);
      end if;
      if Is_Valid (Viewer) then
         Copy (This.all, That.all, From, To, Ptr (Viewer));
      else
         Copy (This.all, That.all, From, To);
      end if;
   end Copy;

   procedure Delete (Lesson : in out Lecture_Handle) is
   begin
      if Is_Valid (Lesson) then
         Object.Archived.Delete (Ptr (Lesson).all);
         Invalidate (Lesson);
      end if;
   end Delete;

   function Get
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Fuzzy.Set is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if Lecture = null then
         return (1..Get_Cardinality (Feature) => Confidence'Last);
      else
         return Get (Lecture.all, Example, Ptr (Feature).all, Image);
      end if;
   end Get;

   function Get
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle
            )  return Classification is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
      Object  : Feature_Object'Class renames Ptr (Feature).all;
   begin
      if Lecture = null then
         return
         (  Cardinality => Object.Cardinality,
            Possibility => (others => Confidence'Last),
            Necessity   => (others => Confidence'First)
         );
      else
         return
         (  Cardinality => Get_Cardinality (Feature),
            Possibility => Get (Lecture, Example, Object, Has_In),
            Necessity   => not Get (Lecture, Example, Object, Has_Not)
         );
      end if;
   end Get;

   function Get_Class (Lesson : Lecture_Handle) return String is
   begin
      return Get_Class (Ptr (Lesson).all);
   end Get_Class;

   function Get_Examples_Number (Lesson : Lecture_Handle)
      return Natural is
      This : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if This = null then
         return 0;
      else
         return Get_Examples_Number (This.all);
      end if;
   end Get_Examples_Number;

   function Get_Feature
            (  Lesson : Lecture_Handle;
               Index  : Positive
            )  return Feature_Handle is
   begin
      return Get_Feature (Ptr (Lesson).all, Index);
   end Get_Feature;

   function Get_Features (Lesson : Lecture_Handle)
      return Bounded_Array is
      This : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if This = null then
         declare
            Result : Bounded_Array (1, 0);
         begin
            return Result;
         end;
      else
         return Get_Features (Ptr (Lesson).all);
      end if;
   end Get_Features;

   function Get_Features_Number (Lesson : Lecture_Handle)
      return Natural is
      This : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if This = null then
         return 0;
      else
         return Get_Features_Number (This.all);
      end if;
   end Get_Features_Number;

   procedure Invalidate (Lesson : in out Lecture_Handle) is
   begin
      Handles.Invalidate (Handles.Handle (Lesson));
   end Invalidate;

   function Is_Defined
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Boolean is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      return
      (  Lecture /= null
      and then
         Is_Defined (Lecture.all, Example, Ptr (Feature).all, Image)
      );
   end Is_Defined;

   function Is_Known
            (  Lesson  : Lecture_Handle;
               Example : Positive;
               Feature : Feature_Handle;
               Image   : Fuzzy.Feature.Image_Type
            )  return Boolean is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      return
      (  Lecture /= null
      and then
         Is_Known (Lecture, Example, Ptr (Feature).all, Image)
      );
   end Is_Known;

   function Is_Valid (Lesson : Lecture_Handle) return Boolean is
   begin
      return Handles.Is_Valid (Handles.Handle (Lesson));
   end Is_Valid;

   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Image   : Fuzzy.Feature.Image_Type;
                Value   : Fuzzy.Set
             )  is
      Lecture : Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if Lecture = null then
         if not Feature.Is_Valid then
            raise Constraint_Error;
         end if;
         Lecture := new General_Lecture_Object;
         Set (Lesson, Lecture);
      end if;
      Put (Lecture.all, Example, Ptr (Feature).all, Image, Value);
   end Put;

   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Value   : Classification
             )  is
      Lecture : Lecture_Object_Ptr := Ptr (Lesson);
      Object  : Feature_Object'Class renames Ptr (Feature).all;
   begin
      if Lecture = null then
         Lecture := new General_Lecture_Object;
         Set (Lesson, Lecture);
      end if;
      Put
      (  Lecture.all,
         Example,
         Object,
         Has_In,
         Value.Possibility
      );
      Put
      (  Lecture.all,
         Example,
         Object,
         Has_Not,
         not Value.Necessity
      );
   end Put;

   procedure Put
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle;
                Image   : Fuzzy.Feature.Image_Type;
                Value   : Positive
             )  is
      Lecture : Lecture_Object_Ptr := Ptr (Lesson);
      Object  : Feature_Object'Class renames Ptr (Feature).all;
   begin
      if Lecture = null then
         Lecture := new General_Lecture_Object;
         Set (Lesson, Lecture);
      end if;
      Put (Lecture.all, Example, Object, Image, Value);
   end Put;

   function Ptr (Lesson : Lecture_Handle)
      return Lecture_Object_Ptr is
   begin
      return Handles.Ptr (Handles.Handle (Lesson));
   end Ptr;

   function Ref (Lesson : Lecture_Object_Ptr) return Lecture_Handle is
   begin
      return (Handles.Ref (Lesson) with null record);
   end Ref;

   procedure Ref
             (  Handle : in out Lecture_Handle;
                Lesson : Lecture_Object_Ptr
             )  is
   begin
      Handles.Set (Handles.Handle (Handle), Lesson);
   end Ref;

   procedure Set_Undefined
             (  Lesson  : in out Lecture_Handle;
                Example : Positive;
                Feature : Feature_Handle
             )  is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if Lecture /= null then
         Set_Undefined (Lecture.all, Example, Ptr (Feature).all);
      end if;
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out Lecture_Handle;
                Feature : Feature_Handle
             )  is
      Lecture : constant Lecture_Object_Ptr := Ptr (Lesson);
   begin
      if Lecture /= null then
         Set_Undefined (Lecture.all, Ptr (Feature).all);
      end if;
   end Set_Undefined;

   function To_Lecture_Handle
            (  Lesson : Deposit_Handles.Handle
            )  return Lecture_Handle is
   begin
      return
         Ref
         (  To_Lecture_Object_Ptr
            (  Deposit_Handles.Ptr (Lesson)
         )  );
   end To_Lecture_Handle;

   function To_Deposit_Handle (Lesson : Lecture_Handle)
      return Deposit_Handles.Handle is
   begin
      return Deposit_Handles.Ref (To_Deposit_Ptr (Ptr (Lesson)));
   end To_Deposit_Handle;

end Fuzzy.Lecture.Handle;
