--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Memory_Resident               Luebeck            --
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

with Fuzzy.Feature;               use Fuzzy.Feature;
with Fuzzy.Lecture.Block.General; use Fuzzy.Lecture.Block.General;
with Fuzzy.Lecture.Block.Dotted;  use Fuzzy.Lecture.Block.Dotted;

with Ada.Unchecked_Deallocation;

package body Fuzzy.Lecture.Memory_Resident is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;
   use Column_Block_Ptr_Unbounded_Array;
   use Column_Ptr_Array;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Column_Block'Class,
             Column_Block_Ptr
          );
--
-- Get_Column_Index -- Decode feature
--
--      Lesson  - The teaching set
--      Example - The example number
--      Feature - The feature
--      Image   - The requested image
--
-- This function creates examples and features as necessary. So it never
-- fails if no corresponding row or column is in the set.
--
-- Returns :
--
--      Index of the column
--
   function Get_Column_Index
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Index;
   pragma Inline (Get_Column_Index);
--
-- Get_Column_Ptr -- Get column
--
--      Lesson - The teaching set
--      Index  - The column index
--      Image  - The requested image
--
-- This function creates the feature column as necessary.  So  it  never
-- fails.
--
-- Returns :
--
--      Column pointer
--
   function Get_Column_Ptr
            (  Lesson : General_Lecture_Object;
               Index  : Feature_Index;
               Image  : Image_Type
            )  return Column_Block_Ptr_Unbounded_Array_Ptr;
   pragma Inline (Get_Column_Ptr);
--
-- Get_Distribution -- Get one value of a feature
--
--    Lesson      - The lecture
--    Example     - To get a value from (shall be valid)
--    Feature     - Indicates the feature (shall be valid)
--    Cardinality - Of the feature
--    Image       - The requested image
--
-- Returns :
--
--    The value
--
   function Get_Distribution
            (  Lesson      : General_Lecture_Object;
               Example     : Positive;
	       Feature     : Feature_Index;
               Cardinality : Positive;
               Image       : Image_Type
            )  return Set;
   pragma Inline (Get_Distribution);

   function Add_Feature
            (  Lesson  : not null access General_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      use Feature_Descriptor_Handles;
      Descriptor : constant Feature_Descriptor_Ptr :=
                      new Feature_Descriptor (Lesson);
      Result : constant Feature_Descriptor_Handles.Handle :=
                  Ref (Descriptor);
   begin
      Put
      (  Lesson.Data (Image),
         Lesson.Columns + 1,
         new Column_Block_Ptr_Unbounded_Array.Unbounded_Ptr_Array
      );
      return Result;
   end Add_Feature;

   function Get_Class (Lesson : General_Lecture_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Is_Defined
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Value_Status is
      Column_Index : Feature_Index;
      ID           : constant Feature_ID := Feature.ID;
   begin
      if (  Example <= Lesson.Rows
         and then
            Lesson.To_Index.Vector /= null
         and then
            ID in Lesson.To_Index.Vector'Range
         )
      then
         Column_Index := Get (Lesson.To_Index, ID);
         if Column_Index /= 0 then
            declare
               Major  : constant Positive := Example / Block_Size   + 1;
               Minor  : constant Subindex := Example rem Block_Size + 1;
               Column : Column_Block_Ptr_Unbounded_Array_Ptr;
               Block  : Column_Block_Ptr;
            begin
               Column := Get (Lesson.Data (Image), Column_Index);
               if Column /= null then
                  Block := Get (Column.all, Major);
                  if Block /= null then
                     return Is_Defined (Block.all, Minor);
                  end if;
               end if;
            end;
         end if;
      end if;
      return Uncertain;
   end Is_Defined;

   function Is_Defined
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return Is_Defined (Lesson, Example, Feature, Image) = Defined;
   end Is_Defined;

   function Is_Known
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      ID : constant Feature_ID := Feature.ID;
      Column_Index : Feature_Index;
   begin
      if (  Example <= Lesson.Rows
         and then
            Lesson.To_Index.Vector /= null
         and then
            ID in Lesson.To_Index.Vector'range
         )
      then
         Column_Index := Get (Lesson.To_Index, ID);
         if Column_Index /= 0 then
            declare
               Major  : constant Positive := Example / Block_Size   + 1;
               Minor  : constant Subindex := Example rem Block_Size + 1;
               Column : Column_Block_Ptr_Unbounded_Array_Ptr;
               Block  : Column_Block_Ptr;
            begin
               Column := Get (Lesson.Data (Image), Column_Index);
               if Column /= null then
                  Block := Get (Column.all, Major);
                  if Block /= null then
                     return Is_Known (Block.all, Minor);
                  end if;
               end if;
            end;
         end if;
      end if;
      return False;
   end Is_Known;

   function Get_Distribution
            (  Lesson       : General_Lecture_Object;
               Example      : Positive;
	       Feature      : Feature_Index;
               Cardinality  : Positive;
               Image        : Image_Type
            )  return Set is
      Major  : constant Positive := Example / Block_Size   + 1;
      Minor  : constant Subindex := Example rem Block_Size + 1;
      Column : Column_Block_Ptr_Unbounded_Array_Ptr;
      Block  : Column_Block_Ptr;
   begin
      Column := Get (Lesson.Data (Image), Feature);
      if Column /= null then
         Block := Get (Column.all, Major);
         if Block /= null then
            return Get (Block.all, Minor);
         end if;
      end if;
      return (1..Cardinality => Confidence'Last);
   end Get_Distribution;

   function Get
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
      Column_Index : Feature_Index;
   begin
      if (  Example <= Lesson.Rows
         and then
            Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'range
         )
      then
         Column_Index := Get (Lesson.To_Index, Feature.ID);
         if Column_Index /= 0 then
            return
               Get_Distribution
               (  Lesson,
                  Example,
                  Column_Index,
                  Get_Cardinality
                  (  Get
                     (  Lesson.To_Feature,
                        Column_Index
                     ) .Feature
                  ),
                  Image
               );
         end if;
      end if;
      return (1..Feature.Cardinality => Confidence'Last);
   end Get;

   function Get_Examples_Number (Lesson : General_Lecture_Object)
      return Natural is
   begin
      return Lesson.Rows;
   end Get_Examples_Number;

   function Get_Column_Index
            (  Lesson  : General_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Index is
   begin
      if Example > Lesson.Rows then
         Lesson.Self.Rows := Example;
      end if;
      return Get_Feature (Lesson.Self, Feature, Image);
   end Get_Column_Index;

   function Get_Column_Ptr
            (  Lesson : General_Lecture_Object;
               Index  : Feature_Index;
               Image  : Image_Type
            )  return Column_Block_Ptr_Unbounded_Array_Ptr is
      Column : Column_Block_Ptr_Unbounded_Array_Ptr;
   begin
      Column := Get (Lesson.Data (Image), Index);
      if Column = null then
         Column :=
            new Column_Block_Ptr_Unbounded_Array.Unbounded_Ptr_Array;
         Put
         (  General_Lecture_Object (Lesson.Self.all).Data (Image),
            Index,
            Column
         );
      end if;
      return Column;
   end Get_Column_Ptr;

   procedure Put
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
      Column_Index : Feature_Index;
   begin
      if Value > Feature.Cardinality then
         raise Constraint_Error;
      end if;
      Column_Index :=
         Get_Column_Index (Lesson, Example, Feature, Image);
      declare
         Major  : constant Positive := Example / Block_Size   + 1;
         Minor  : constant Subindex := Example rem Block_Size + 1;
         Column : Column_Block_Ptr_Unbounded_Array_Ptr;
         Block  : Column_Block_Ptr;
      begin
         Column := Get_Column_Ptr (Lesson, Column_Index, Image);
         Block  := Get (Column.all, Major);
         if Block = null then
            Block := new Dot_Column_Block (Feature.Cardinality);
            Put (Column.all, Major, Block);
         elsif Is_Defined (Block.all, Minor) = Defined then
            --
            -- There  is  a  block  for  this example and it has a value
            -- defined
            --
            if Get (Block.all, Minor, Value) = Confidence'Last then
               --
               -- It is the required value, nothing to do
               --
               return;
            end if;
            if Block.all in Dot_Column_Block then
               --
               -- Replace a dotted column block with a fully capable set
               -- column block
               --
               declare
                  New_Block : constant Column_Block_Ptr :=
                     new Set_Column_Block (Feature.Cardinality);
               begin
                  for Index in Subindex loop
                     if Is_Defined (Block.all, Index) = Defined then
                        Put
                        (  New_Block.all,
                           Index,
                           Get (Block.all, Index)
                        );
                     end if;
                  end loop;
                  Block := New_Block;
                  Put (Column.all, Major, Block);
               end;
            end if;
         end if;
         Put (Block.all, Minor, Value);
         Lesson.Updated := True;
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Put;

   procedure Put
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
      Column_Index : Feature_Index;
   begin
      if Value'Length /= Feature.Cardinality then
         raise Constraint_Error;
      end if;
      Column_Index :=
         Get_Column_Index (Lesson, Example, Feature, Image);
      declare
         Major  : constant Positive := Example / Block_Size   + 1;
         Minor  : constant Subindex := Example rem Block_Size + 1;
         Column : Column_Block_Ptr_Unbounded_Array_Ptr;
         Block  : Column_Block_Ptr;
      begin
         Column := Get_Column_Ptr (Lesson, Column_Index, Image);
         Block  := Get (Column.all, Major);
         if Block = null then
            Block := new Set_Column_Block (Feature.Cardinality);
            Put (Column.all, Major, Block);
         else
            if Block.all in Dot_Column_Block then
               --
               -- Replace a dotted column block with a fully capable set
               -- column block
               --
               declare
                  New_Block : constant Column_Block_Ptr :=
                     new Set_Column_Block (Feature.Cardinality);
               begin
                  for Index in Subindex loop
                     if Is_Defined (Block.all, Index) = Defined then
                        Put
                        (  New_Block.all,
                           Index,
                           Get (Block.all, Index)
                        );
                     end if;
                  end loop;
                  Block := New_Block;
                  Put (Column.all, Major, Block);
               end;
            end if;
         end if;
         Put (Block.all, Minor, Value);
         Lesson.Updated := True;
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Put;

   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Status  : Undefined_Status
             )  is
      Column_Index : Feature_Index;
   begin
      if (  Example <= Lesson.Rows
         and then
            Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Column_Index := Get (Lesson.To_Index, Feature.ID);
         if Column_Index /= 0 then
            declare
               Major  : constant Positive := Example / Block_Size   + 1;
               Minor  : constant Subindex := Example rem Block_Size + 1;
               Column : Column_Block_Ptr_Unbounded_Array_Ptr;
               Block  : Column_Block_Ptr;
            begin
               for Image in Image_Type loop
                  Column := Get (Lesson.Data (Image), Column_Index);
                  if Column /= null then
                     Block := Get (Column.all, Major);
                     if Block /= null then
                        Set_Undefined (Block.all, Minor, Status);
                     end if;
                  end if;
               end loop;
            end;
            Lesson.Updated := True;
         end if;
      end if;
      Notify_Undefined (Lesson, Example, Feature);
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Status  : Undefined_Status
             )  is
      Column_Index : Feature_Index;
   begin
      if (  Example <= Lesson.Rows
         and then
            Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Column_Index := Get (Lesson.To_Index, Feature.ID);
         if Column_Index /= 0 then
            declare
               Major  : constant Positive := Example / Block_Size   + 1;
               Minor  : constant Subindex := Example rem Block_Size + 1;
               Column : Column_Block_Ptr_Unbounded_Array_Ptr;
               Block  : Column_Block_Ptr;
            begin
               Column := Get (Lesson.Data (Image), Column_Index);
               if Column /= null then
                  Block := Get (Column.all, Major);
                  if Block /= null then
                     Set_Undefined (Block.all, Minor, Status);
                  end if;
               end if;
            end;
            Lesson.Updated := True;
         end if;
      end if;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
   begin
      Set_Undefined (Lesson, Example, Feature, Undefined);
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
      ID           : constant Feature_ID := Get_ID (Feature);
      Column_Index : Feature_Index;
      procedure Remove_Column
                (  Columns : in out Column_Ptr_Array.Unbounded_Ptr_Array
                )  is
         use Column_Ptr_Array.Object_Ptr_Array;
         procedure Free is
            new Ada.Unchecked_Deallocation
                (  Column_Block_Ptr_Unbounded_Array.Unbounded_Ptr_Array,
                   Column_Block_Ptr_Unbounded_Array_Ptr
                );
      begin
         if Columns.Vector /= null then
            declare
               Vector : Column_Block_Ptr_Unbounded_Array_Ptr_Array
                           renames Columns.Vector.all;
            begin
               if Column_Index in Vector'Range then
                  Free (Vector (Column_Index));
                  Vector (Column_Index..Vector'Last - 1) :=
                     Vector (Column_Index + 1..Vector'Last);
                  Vector (Vector'Last) := null;
               end if;
            end;
         end if;
      end Remove_Column;
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            ID in Lesson.To_Index.Vector'Range
         )
      then
         Column_Index := Lesson.To_Index.Vector (ID);
         if Column_Index /= 0 then
            Remove_Column (Lesson.Data (Has_In));
            Remove_Column (Lesson.Data (Has_Out));
            Remove_Column (Lesson.Data (Has_Not));
            Remove_Column (Lesson.Data (Has_Not_Out));
            Delete_Feature (Lesson, Column_Index);
         end if;
      end if;
      Notify_Undefined (Lesson, Feature);
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out General_Lecture_Object;
                Example : Positive;
                Status  : Undefined_Status
             )  is
      Column_Index : Feature_Index;
   begin
      if Example > Lesson.Rows or else Lesson.To_Index.Vector = null
      then
         return;
      end if;
      for Index in Lesson.To_Index.Vector'Range loop
         Column_Index := Lesson.To_Index.Vector (Index);
         if Column_Index /= 0 then
            declare
               Major  : constant Positive := Example / Block_Size   + 1;
               Minor  : constant Subindex := Example rem Block_Size + 1;
               Column : Column_Block_Ptr_Unbounded_Array_Ptr;
               Block  : Column_Block_Ptr;
            begin
               for Image in Image_Type loop
                  Column := Get (Lesson.Data (Image), Column_Index);
                  if Column /= null then
                     Block := Get (Column.all, Major);
                     if Block /= null then
                        Set_Undefined (Block.all, Minor, Status);
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end loop;
      for Index in 1..Lesson.Columns loop
         Notify_Undefined
         (  Lesson,
            Example,
            Ptr (Get (Lesson.To_Feature, Index).Feature).all
         );
      end loop;
   end Set_Undefined;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             )  is
      Result : Deposit_Ptr := new General_Lecture_Object;
   begin
      Generic_Restore
      (  Source,
         Pointer,
         Class,
         List,
         Lecture_Object'Class (Result.all)
      );
      Lesson := Result;
   exception
      when others =>
         Free (Result);
         raise;
   end Restore;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : General_Lecture_Object
             )  is
   begin
      Generic_Store (Destination, Pointer, Lesson);
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Fuzzy.Lecture.Memory_Resident;
