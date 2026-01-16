--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Subrange                      Luebeck            --
--  Implementation                                 Autumn, 2010       --
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

with Ada.Exceptions;         use Ada.Exceptions;
with Ada.IO_Exceptions;      use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;       use Fuzzy.Basic_Edit;
with Strings_Edit;           use Strings_Edit;
with Strings_Edit.Integers;  use Strings_Edit.Integers;

with Ada.Unchecked_Deallocation;

package body Fuzzy.Lecture.Subrange is

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Source_Observer,
             Source_Observer_Ptr
          );

   procedure Added
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
   begin
      Notify_New (Observer.Subrange.all, Feature);
   end Added;

   procedure Changed
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             )  is
   begin
      if (  Example > Observer.Subrange.Offset
         and then
            Example - Observer.Subrange.Offset <= Observer.Subrange.Size
         )
      then
         Notify_Changed
         (  Observer.Subrange.all,
            Example - Observer.Subrange.Offset,
            Feature,
            Image
         );
      end if;
   end Changed;

   function Create
            (  Source : Lecture_Handle;
               From   : Positive;
               To     : Natural := Positive'Last
            )  return Lecture_Handle is
      Result : constant Lecture_Handle :=
                  Ref
                  (  new Subrange_Lecture_Object'
                         (  Lecture_Object
                         with
                            Offset   => From - 1,
                            Size     => Natural'Max (0, To - From + 1),
                            Source   => Source,
                            Observer => null
                  )      );
      Lesson : Subrange_Lecture_Object renames
                  Subrange_Lecture_Object (Ptr (Result).all);
   begin
      Lesson.Observer :=
         new Source_Observer
             (  Lesson   => Ptr (Source),
                Subrange => Lesson'Unchecked_Access
             );
      return Result;
   end Create;

   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             )  is
   begin
      if (  Example > Observer.Subrange.Offset
         and then
            Example - Observer.Subrange.Offset <= Observer.Subrange.Size
         )
      then
         Notify_Undefined
         (  Observer.Subrange.all,
            Example - Observer.Subrange.Offset,
            Feature
         );
      end if;
   end Deleted;

   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
   begin
      Notify_Undefined (Observer.Subrange.all, Feature);
   end Deleted;

   procedure Finalize (Lesson : in out Subrange_Lecture_Object) is
   begin
      Free (Lesson.Observer);
      Finalize (Lecture_Object (Lesson));
   end Finalize;

   function Get
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
   begin
      if Example <= Lesson.Size then
         return
            Get
            (  Ptr (Lesson.Source).all,
               Example + Lesson.Offset,
               Feature,
               Image
            );
      else
         return (1..Feature.Cardinality => Confidence'Last);
      end if;
   end Get;

   function Get_Class (Lesson : Subrange_Lecture_Object)
      return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : Subrange_Lecture_Object)
      return Natural is
   begin
      return
          Natural'Min
          (  Lesson.Size,
             Get_Examples_Number (Lesson.Source) - Lesson.Offset
          );
   end Get_Examples_Number;

   function Get_Feature
            (  Lesson : Subrange_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle is
   begin
      return Get_Feature (Lesson.Source, Index);
   end Get_Feature;

   function Get_Features (Lesson : Subrange_Lecture_Object)
      return Bounded_Array is
   begin
      return Get_Features (Lesson.Source);
   end Get_Features;

   function Get_Features_Number (Lesson : Subrange_Lecture_Object)
      return Natural is
   begin
      return Get_Features_Number (Lesson.Source);
   end Get_Features_Number;

   procedure Get_Referents
             (  Lesson : Subrange_Lecture_Object;
                List   : in out Deposit_Container'Class
             )  is
   begin
      Add (List, To_Deposit_Ptr (Ptr (Lesson.Source)), False);
   end Get_Referents;

   function Is_Defined
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      if Example <= Lesson.Size then
         return
            Is_Defined
            (  Ptr (Lesson.Source).all,
               Example + Lesson.Offset,
               Feature,
               Image
            );
      else
         return False;
      end if;
   end Is_Defined;

   function Is_Known
            (  Lesson  : Subrange_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      if Example <= Lesson.Size then
         return
            Is_Known
            (  Ptr (Lesson.Source).all,
               Example + Lesson.Offset,
               Feature,
               Image
            );
      else
         return False;
      end if;
   end Is_Known;

   function Is_Modified (Lesson : Subrange_Lecture_Object)
      return Boolean is
   begin
      return Is_Modified (Ptr (Lesson.Source).all);
   end Is_Modified;

   procedure Put
             (  Lesson  : in out Subrange_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
   begin
      if Example <= Lesson.Size then
         Put
         (  Ptr (Lesson.Source).all,
            Example + Lesson.Offset,
            Feature,
            Image,
            Value
         );
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Putting example out of range of the training set"
         );
      end if;
   end Put;

   procedure Put
             (  Lesson  : in out Subrange_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
   begin
      if Example <= Lesson.Size then
         Put
         (  Ptr (Lesson.Source).all,
            Example + Lesson.Offset,
            Feature,
            Image,
            Value
         );
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Putting example out of range of the training set"
         );
      end if;
   end Put;

   procedure Renamed
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             )  is
   begin
      Notify_Renamed
      (  Observer.Subrange.all,
         Feature,
         Old_Name,
         New_Name
      );
   end Renamed;

   procedure Reset_Modified (Lesson : in out Subrange_Lecture_Object) is
   begin
      Reset_Modified (Ptr (Lesson.Source).all);
   end Reset_Modified;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             )  is
      Reference : Lecture_Handle;
      Offset    : Positive;
      Size      : Natural;
      Position  : aliased Integer := Pointer;
      Got_It    : Boolean         := False;
   begin
      begin
         Get (Source, Position, Offset, First => 1);
      exception
         when others =>
            raise Data_Error;
      end;
      Get_Delimiter (Source, Position, Got_It, Colon);
      if not Got_It then
         raise Data_Error;
      end if;
      begin
         Get (Source, Position, Size, First => 0);
      exception
         when others =>
            raise Data_Error;
      end;
      begin
         Reference := Ref (To_Lecture_Object_Ptr (Get (List, 1)));
      exception
         when Constraint_Error | End_Error =>
            raise Use_Error;
      end;
      Lesson :=
         new Subrange_Lecture_Object'
             (  Lecture_Object with Offset, Size, Reference, null
             );
      declare
         Result : Subrange_Lecture_Object renames
                     Subrange_Lecture_Object (Lesson.all);
      begin
         Result.Observer :=
            new Source_Observer
                (  Lesson   => Ptr (Result.Source),
                   Subrange => Result'Unchecked_Access
                );
      end;
      Pointer := Position;
   end Restore;

   procedure Set_Undefined
             (  Lesson  : in out Subrange_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
   begin
      if Example <= Lesson.Size then
         Set_Undefined
         (  Ptr (Lesson.Source).all,
            Example + Lesson.Offset,
            Feature
         );
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Setting example out of range of the training set"
         );
      end if;
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out Subrange_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
      Last : Natural := Get_Examples_Number (Lesson.Source);
   begin
      if Last - Lesson.Offset > Lesson.Size then
         Last := Lesson.Offset + Lesson.Size - 1;
      end if;
      for Example in Lesson.Offset + 1..Last loop
         Set_Undefined (Ptr (Lesson.Source).all, Example, Feature);
      end loop;
   end Set_Undefined;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Subrange_Lecture_Object
             )  is
      Position : Integer := Pointer;
   begin
      Put (Destination, Position, Lesson.Offset);
      Put (Destination, Position, ": ");
      Put (Destination, Position, Lesson.Size);
      Pointer := Position;
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Lecture.Subrange;
