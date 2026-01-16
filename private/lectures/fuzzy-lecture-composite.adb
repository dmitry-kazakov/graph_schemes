--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Composite                     Luebeck            --
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
with Fuzzy.Lecture.Handle.Container;

package body Fuzzy.Lecture.Composite is
   use Feature_Descriptor_Handles;
   use ID_To_Index_Map;
   use Index_To_Feature_Map;

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
      null;
   end Added;

   function Add_Feature
            (  Lesson  : not null access Composite_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      Descriptor : constant Feature_Descriptor_Ptr :=
                      new Column_Descriptor (Lesson);
   begin
      return Ref (Descriptor);
   end Add_Feature;

   procedure Changed
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class;
                Image    : Image_Type
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Observer.Composite.all, Feature);
   begin
      if Source /= null then
         Observer.Composite.Updated := True;
         Notify_Changed
         (  Observer.Composite.all,
            Example,
            Feature,
            Image
         );
      end if;
   end Changed;

   function Create (Mapping : Feature_To_Lecture_Map)
      return Lecture_Handle is
      Result : constant Lecture_Handle :=
                  Ref (new Composite_Lecture_Object);
      Lesson : Composite_Lecture_Object renames
                  Composite_Lecture_Object (Ptr (Result).all);
   begin
      for Index in Mapping'Range loop
         declare
            Feature : Feature_Object'Class renames
                         Ptr (Mapping (Index).Feature).all;
         begin
            if Get_Source (Lesson, Feature) /= null then
               Raise_Exception
               (  Data_Error'Identity,
                  "Duplicated feature"
               );
            end if;
            declare
               Column : Column_Descriptor renames
                           Column_Descriptor
                           (  Get
                              (  Lesson.To_Feature,
                                 Get_Feature
                                 (  Lesson'Access,
                                    Feature,
                                    Has_In
                              )  ) .all
                           );
            begin
               Column.Source := Mapping (Index).Lecture;
               if not Is_Valid (Column.Source) then
                  Raise_Exception
                  (  Constraint_Error'Identity,
                     "Invalid training set"
                  );
               end if;
               Column.Observer :=
                  new Source_Observer
                      (  Lesson    => Ptr (Column.Source),
                         Composite => Lesson'Unchecked_Access
                      );
            end;
         end;
      end loop;
      return Result;
   end Create;

   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Example  : Positive;
                Feature  : Feature_Object'Class
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Observer.Composite.all, Feature);
   begin
      if Source /= null then
         Observer.Composite.Updated := True;
         Notify_Undefined
         (  Observer.Composite.all,
            Example,
            Feature
         );
      end if;
   end Deleted;

   procedure Deleted
             (  Observer : in out Source_Observer;
                Lesson   : Lecture_Object'Class;
                Feature  : Feature_Object'Class
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Observer.Composite.all, Feature);
   begin
      if Source /= null then
         Set_Undefined (Observer.Composite.all, Feature);
      end if;
   end Deleted;

   procedure Finalize (Item : in out Column_Descriptor) is
   begin
      Free (Item.Observer);
      Finalize (Feature_Descriptor (Item));
   end Finalize;

   function Get
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
      Source : constant Lecture_Object_Ptr := Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         return Get (Source.all, Example, Feature, Image);
      else
         return (1..Feature.Cardinality => Confidence'Last);
      end if;
   end Get;

   function Get_Class (Lesson : Composite_Lecture_Object)
      return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : Composite_Lecture_Object)
      return Natural is
      use Fuzzy.Lecture.Handle.Container;
      Result  : Natural := 0;
      Lessons : Fuzzy.Lecture.Handle.Container.Set;
   begin
      for Index in 1..Lesson.Columns loop
         Add
         (  Lessons,
            Column_Descriptor
            (  Get (Lesson.To_Feature, Index).all
            ) .Source
         );
      end loop;
      for Index in 1..Get_Size (Lessons) loop
         Result :=
            Natural'Max
            (  Result,
               Get_Examples_Number (Get (Lessons, Index).all)
            );
      end loop;
      return Result;
   end Get_Examples_Number;

   procedure Get_Referents
             (  Lesson : Composite_Lecture_Object;
                List   : in out Deposit_Container'Class
             )  is
   begin
      for Index in 1..Lesson.Columns loop
         declare
            Column : Column_Descriptor renames
                        Column_Descriptor
                        (  Get (Lesson.To_Feature, Index).all
                        );
         begin
            Add
            (  List,
               To_Deposit_Ptr (Ptr (Column.Feature)),
               True
            );
            Add
            (  List,
               To_Deposit_Ptr (Ptr (Column.Source)),
               True
            );
         end;
      end loop;
   end Get_Referents;

   function Get_Source
            (  Lesson  : Composite_Lecture_Object'Class;
               Feature : Feature_Object'Class
            )  return Lecture_Object_Ptr is
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         declare
            Index : constant Feature_Index :=
                       Get (Lesson.To_Index, Feature.ID);
         begin
            if Index /= 0 then
               return
                  Ptr
                  (  Column_Descriptor
                     (  Get (Lesson.To_Feature, Index).all
                     ) .Source
                  );
            end if;
         end;
      end if;
      return null;
   end Get_Source;

   function Is_Defined
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         return Is_Defined (Source.all, Example, Feature, Image);
      else
         return False;
      end if;
   end Is_Defined;

   function Is_Known
            (  Lesson  : Composite_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         return Is_Known (Source.all, Example, Feature, Image);
      else
         return False;
      end if;
   end Is_Known;

   procedure Put
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         Put (Source.all, Example, Feature, Image, Value);
         Lesson.Updated := True;
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Putting example of a feature unsupported by the set"
         );
      end if;
   end Put;

   procedure Put
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         Put (Source.all, Example, Feature, Image, Value);
         Lesson.Updated := True;
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Putting example of a feature unsupported by the set"
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
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Observer.Composite.all, Feature);
   begin
      if Source /= null then
         Notify_Renamed
         (  Observer.Composite.all,
            Feature,
            Old_Name,
            New_Name
         );
      end if;
   end Renamed;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             )  is
      Size     : Natural;
      Index    : Positive := 1;
      Position : aliased Integer := Pointer;
   begin
      begin
         Get (Source, Position, Size, First => 0);
      exception
         when others =>
            raise Data_Error;
      end;
      Lesson := new Composite_Lecture_Object;
      declare
         Result : Composite_Lecture_Object renames
                     Composite_Lecture_Object (Lesson.all);
      begin
         for Column_No in 1..Size loop
            declare
               Feature : Feature_Object_Ptr;
               Source  : Lecture_Object_Ptr;
            begin
               Feature := To_Feature_Object_Ptr (Get (List, Index));
               Index   := Index + 1;
               Source  := To_Lecture_Object_Ptr (Get (List, Index));
               Index   := Index + 1;
               declare
                  Column : Column_Descriptor renames
                           Column_Descriptor
                           (  Get
                              (  Result.To_Feature,
                                 Get_Feature
                                 (  Result'Access,
                                    Feature.all,
                                    Has_In
                              )  ) .all
                           );
               begin
                  Column.Source := Ref (Source);
                  Column.Observer :=
                     new Source_Observer
                         (  Lesson => Source,
                            Composite =>
                               Composite_Lecture_Object
                               (  Lesson.all
                               ) 'Unchecked_Access
                        );
               end;
            exception
               when Constraint_Error | End_Error =>
                  raise Use_Error;
            end;
         end loop;
      end;
      Pointer := Position;
   exception
      when others =>
         Free (Lesson);
         raise;
   end Restore;

   procedure Set_Undefined
             (  Lesson  : in out Composite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
      Source : constant Lecture_Object_Ptr :=
                  Get_Source (Lesson, Feature);
   begin
      if Source /= null then
         Set_Undefined (Source.all, Example, Feature);
         Lesson.Updated := True;
         Notify_Undefined (Lesson, Example, Feature);
      else
         Raise_Exception
         (  Use_Error'Identity,
            "Setting example out of range of the training set"
         );
      end if;
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out Composite_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         declare
            Index : constant Feature_Index :=
                       Get (Lesson.To_Index, Feature.ID);
         begin
            if Index /= 0 then
               Delete_Feature (Lesson, Index);
               Lesson.Updated := True;
               Notify_Undefined (Lesson, Feature);
            end if;
         end;
      end if;
   end Set_Undefined;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Composite_Lecture_Object
             )  is
   begin
      Put (Destination, Pointer, Natural (Lesson.Columns));
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Lecture.Composite;
