--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Connotated                    Luebeck            --
--  Implementation                                 Spring, 2002       --
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

package body Fuzzy.Lecture.Connotated is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;

   function To_Backward_Link_Ptr is
      new Ada.Unchecked_Conversion
          (  Feature_Descriptor_Ptr,
             Backward_Link_Ptr
          );

   procedure Deleted
             (  Item  : in out Feature_Descriptor;
                Temps : in out Deposit_Container'Class
             )  is
      Feature : Feature_Object'Class renames Ptr (Item.Feature).all;
      Lesson  : Connotated_Lecture_Object'Class renames Item.Lesson.all;
   begin
      Set_Undefined (Lesson, Feature);
      if 0 /= Get (Lesson.To_Index, Feature.ID) then
         raise Constraint_Error;
      end if;
   end Deleted;

   procedure Delete_Feature
             (  Lesson  : in out Connotated_Lecture_Object'Class;
                Feature : Feature_Index
             )  is
      To_Index : ID_To_Index_Array renames Lesson.To_Index.Vector.all;
      ID       : constant Feature_ID :=
                    Ptr (Get (Lesson.To_Feature, Feature).Feature).ID;
   begin
      for ID in To_Index'Range loop
         if To_Index (ID) > Feature then
            To_Index (ID) := To_Index (ID) - 1;
         end if;
      end loop;
      To_Index (ID) := 0;
      for Index in Feature..Lesson.Columns - 1 loop
         Put
         (  Lesson.To_Feature,
            Index,
            Get (Lesson.To_Feature, Index + 1)
         );
      end loop;
      Put (Lesson.To_Feature, Lesson.Columns, null);
      Lesson.Columns := Lesson.Columns - 1;
      Lesson.Updated := True;
   end Delete_Feature;

   procedure Destroyed (Item : in out Feature_Descriptor) is
   begin
      null;
   end Destroyed;

   procedure Finalize (Lesson : in out Connotated_Lecture_Object) is
   begin
      Erase (Lesson.To_Feature); -- Make sure all descriptors gone
      Finalize (Lecture_Object (Lesson));
   end Finalize;

   function Get_Feature
            (  Lesson  : not null access
                         Connotated_Lecture_Object'Class;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Index is
      Descriptor : Feature_Descriptor_Ptr;
      Handle     : Feature_Descriptor_Handles.Handle;
      Object_Ptr : constant Deposit_Ptr :=
                      To_Deposit_Ptr (Feature.Self);
      Result     : Feature_Index;
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Result := Get (Lesson.To_Index, Feature.ID);
         if Result /= 0 then
            return Result;
         end if;
      end if;
      Handle := Add_Feature (Lesson, Feature, Image);
      Descriptor := Ptr (Handle);
      Attach (To_Backward_Link_Ptr (Descriptor), Object_Ptr);
      Descriptor.Feature := Ref (To_Feature_Object_Ptr (Object_Ptr));
      Lesson.Columns := Lesson.Columns + 1;
      Lesson.Updated := True;
      Put (Lesson.To_Index, Feature.ID, Lesson.Columns);
      Put (Lesson.To_Feature, Lesson.Columns, Descriptor);
      Notify_New (Lesson.all, Feature);
      return Lesson.Columns;
   end Get_Feature;

   function Get_Feature
            (  Lesson : Connotated_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle is
   begin
      return Get (Lesson.To_Feature, Feature_Index (Index)).Feature;
   end Get_Feature;

   function Get_Features (Lesson : Connotated_Lecture_Object)
      return Bounded_Array is
      Result : Bounded_Array (1, Integer (Lesson.Columns));
   begin
      for Index in 1..Lesson.Columns loop
         Put
         (  Result,
            Integer (Index),
            Get (Lesson.To_Feature, Index).Feature
         );
      end loop;
      return Result;
   end Get_Features;

   function Get_Features_Number (Lesson : Connotated_Lecture_Object)
      return Natural is
   begin
      return Natural (Lesson.Columns);
   end Get_Features_Number;

   procedure Get_Referents
             (  Lecture : Connotated_Lecture_Object;
                List    : in out Deposit_Container'Class
             )  is
   begin
      for Column in 1..Lecture.Columns loop
         Add
         (  List,
            To_Deposit_Ptr
            (  Ptr
               (  Get (Lecture.To_Feature, Column).Feature
            )  ),
            True
         );
      end loop;
   end Get_Referents;

   function Is_Modified (Lesson : Connotated_Lecture_Object)
      return Boolean is
   begin
      return Lesson.Updated;
   end Is_Modified;

   procedure Renamed
             (  Link     : in out Feature_Descriptor;
                Old_Name : String;
                New_Name : String
             )  is
   begin
      Notify_Renamed
      (  Link.Lesson.all,
         To_Feature_Object_Ptr (This (Link)).all,
         Old_Name,
         New_Name
      );
   end Renamed;

   procedure Reset_Modified
             (  Lesson : in out Connotated_Lecture_Object
             )  is
   begin
      Lesson.Updated := False;
   end Reset_Modified;

end Fuzzy.Lecture.Connotated;
