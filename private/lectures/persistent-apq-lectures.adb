--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.APQ.Lectures                    Luebeck            --
--  Imaplementation                                Winter, 2002       --
--                                                                    --
--                                Last revision :  21:30 10 Nov 2009  --
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

with Ada.IO_Exceptions;              use Ada.IO_Exceptions;
with Confidence_Factors;             use Confidence_Factors;
with Fuzzy;                          use Fuzzy;
with APQ.Common;                     use APQ.Common;
with APQ.Keys.Sets;                  use APQ.Keys.Sets;
with APQ.Lectures;                   use APQ.Lectures;
with Persistent.Data_Bank.Reference; use Persistent.Data_Bank.Reference;

with Ada.Unchecked_Deallocation;
with APQ.Links;
with Fuzzy.Lecture.General;

package body Persistent.APQ.Lectures is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;
--
-- Prepare_Tables -- Creating all things necessary
--
--    Lesson  - The training set
--
   procedure Prepare_Tables (Lesson : in out APQ_Lecture_Object);

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  String,
             String_Ptr
          );

   function Add_Feature
            (  Lesson  : access APQ_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      use Feature_Descriptor_Handles;
      Handle : Deposit_Handle := Ref (To_Deposit_Ptr (Feature.Self));
      Link   : Feature_Descriptor_Ptr :=
                  new APQ_Feature_Descriptor (Lesson);
      Result : Feature_Descriptor_Handles.Handle := Ref (Link);
   begin
      if not Lesson.Restoring then
         --
         -- Store the feature, if necessary
         --
         Put (Lesson.Storage.all, Handle);
      end if;
      --
      -- Get the feature's ID
      --
      APQ_Feature_Descriptor (Link.all).ID :=
         Object_Key (Get_Key (Lesson.Storage.all, Handle)).ID;
      if not Lesson.Restoring then
         --
         -- Update the lecture object in the storage
         --
         Put
         (  Lesson.Storage.all,
            Object_Key'(Persistent_Key with Lesson.ID),
            Lesson.all
         );
         Reset_Modified (Lesson.all);
      end if;
      return Result;
   end Add_Feature;

   procedure Prepare_Tables (Lesson : in out APQ_Lecture_Object) is
      Storage   : Data_Base_Object'Class renames Lesson.Storage.all;
      Key_Image : constant String :=
                     Image
                     (  Lesson.Storage.all,
                        Object_Key'(Persistent_Key with Lesson.ID)
                     );
   begin
      Lesson.Table (Has_In)  := new String'(Has_In_Table  & Key_Image);
      Lesson.Table (Has_Out) := new String'(Has_Out_Table & Key_Image);
      Lesson.Table (Has_Not) := new String'(Has_Not_Table & Key_Image);
      Lesson.Table (Has_Not_Out) :=
         new String'(Has_Not_Out_Table & Key_Image);
   end Prepare_Tables;

   function Create
            (  Storage : Storage_Handle;
               Name    : Wide_String
            )  return Lecture_Handle is
      Where  : Data_Base_Object'Class renames
                  Data_Base_Object'Class (Ptr (Storage).all);
      Key    : Object_Key;
      Empty  : Deposit_Set;
      Object : Deposit_Handle;
      Result : Deposit_Ptr := new APQ_Lecture_Object (Where'Access);
      This   : APQ_Lecture_Object
                  renames APQ_Lecture_Object (Result.all);
   begin
      Object := Ref (Result);
      This.Reference := Storage;
      declare
         Mutex : Write_Mutex (Where'Unchecked_Access);
      begin
         Create
         (  Storage        => Where,
            Object         => Object,
            ID             => Key,
            Name           => Name,
            Class          => Class,
            Data           => "",
            Parameters     => "@",
            Direct_Links   => Empty,
            Backward_Links => Empty
         );
         Commit (Mutex);
      end;
      This.ID := Key.ID;
      Prepare_Tables (This);
      for Index in This.Table'Range loop
         Drop (Where.Data_Base, This.Table (Index).all);
      end loop;
      declare
         Mutex : Write_Mutex (Where'Unchecked_Access);
      begin
         for Index in This.Table'Range loop
            Create_Table
            (  Where.Data_Base,
               This.Table (Index).all
            );
         end loop;
         Commit (Mutex);
      end;
      return Ref (To_Lecture_Object_Ptr (Result));
   end Create;

   procedure Finalize (Lesson : in out Apq_Lecture_Object) is
      Delete : Boolean;
   begin
      declare
         Key   : Object_Key := (Persistent_Key with Lesson.ID);
         Mutex : Read_Mutex (Lesson.Storage);
      begin
         Delete :=
            not
            (  Is_Named (Lesson.Storage, Key)
            or else
               Has_Dependants (Lesson.Storage, Key)
            );
         Commit (Mutex);
      end;
      if Delete then
         for Image in Lesson.Table'Range loop
            if Lesson.Table (Image) /= null then
               Drop
               (  Lesson.Storage.Data_Base,
                  Lesson.Table (Image).all
               );
            end if;
         end loop;
      end if;
      Close (Lesson);
      for Image in Image_Type loop
         Free (Lesson.Table (Image));
      end loop;
      Finalize (Connotated_Lecture_Object (Lesson));
   exception
      when others =>
         for Image in Image_Type loop
            Free (Lesson.Table (Image));
         end loop;
         Finalize (Connotated_Lecture_Object (Lesson));
         raise;
   end Finalize;

   function Get
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set is
      Index  : Feature_Index;
      Result : Fuzzy.Set (1..Feature.Cardinality);
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Index := Get (Lesson.To_Index, Feature.ID);
         if Index /= 0 then
            declare
               Mutex : Read_Mutex (Lesson.Storage);
            begin
               Get
               (  Lesson.Storage.Data_Base,
                  Lesson.Table (Image).all,
                  Example,
                  APQ_Feature_Descriptor
                  (  Get (Lesson.To_Feature, Index).all
                  ) .ID,
                  Result
               );
               Commit (Mutex);
            end;
         else
            Result := (others => Confidence'Last);
         end if;
      else
         Result := (others => Confidence'Last);
      end if;
      return Result;
   end Get;

   function Get_Class (Lesson : APQ_Lecture_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : APQ_Lecture_Object)
      return Natural is
      Result : Natural := 0;
      Mutex  : Read_Mutex (Lesson.Storage);
   begin
      for Image in Image_Type loop
         Result :=
            Natural'Max
            (  Get_Examples_No
               (  Lesson.Storage.Data_Base,
                  Lesson.Table (Image).all
               ),
               Result
            );
      end loop;
      Commit (Mutex);
      return Result;
   end Get_Examples_Number;

   procedure Get_Referents
             (  Lesson : APQ_Lecture_Object;
                List   : in out Deposit_Container'Class
             )  is
   begin
      Add
      (  List,
         Lesson.Reference,
         Object_Key'(Persistent_Key with Lesson.ID)
      );
      Get_Referents
      (  Connotated_Lecture_Object (Lesson),
         List
      );
   end Get_Referents;

   function Is_Defined
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Index  : Feature_Index;
      Result : Boolean := False;
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Index := Get (Lesson.To_Index, Feature.ID);
         if Index /= 0 then
            declare
               Mutex : Read_Mutex (Lesson.Storage);
            begin
               Result :=
                  Is_Defined
                  (  Lesson.Storage.Data_Base,
                     Lesson.Table (Image).all,
                     Example,
                     APQ_Feature_Descriptor
                     (  Get (Lesson.To_Feature, Index).all
                     ) .ID
                  );
               Commit (Mutex);
            end;
         end if;
      end if;
      return Result;
   end Is_Defined;

   function Is_Known
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return
      (  Necessity (Get (Lesson, Example, Feature, Image))
      /= Confidence'Last
      );
   end Is_Known;

   procedure Put
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
               Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             )  is
      Mutex : Write_Mutex (Lesson.Storage);
   begin
      Put
      (  Lesson.Storage.Data_Base,
         Lesson.Table (Image).all,
         Example,
         APQ_Feature_Descriptor
         (  Get
            (  Lesson.To_Feature,
               Get_Feature
               (  Lesson'Unchecked_Access,
                  Feature,
                  Image
            )  ) .all
         ) .ID,
         Value
      );
      Commit (Mutex);
      Notify_Changed (Lesson, Example, Feature, Image);
   end Put;

   procedure Put
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
      Mutex : Write_Mutex (Lesson.Storage);
   begin
      Put
      (  Lesson.Storage.Data_Base,
         Lesson.Table (Image).all,
         Example,
         APQ_Feature_Descriptor
         (  Get
            (  Lesson.To_Feature,
               Get_Feature
               (  Lesson'Unchecked_Access,
                  Feature,
                  Image
            )  ) .all
         ) .ID,
         Value,
         Feature.Cardinality
      );
      Commit (Mutex);
      Notify_Changed (Lesson, Example, Feature, Image);
   end Put;

   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             )  is
      Result : Deposit_Ptr;
   begin
      if Pointer = Source'Last + 1 then
         declare
            Reference : Self_Reference'Class renames
               Self_Reference'Class (Get (List, 1).all);
            Data_Bank : Data_Bank_Object'Class renames
               Data_Bank_Object'Class (Ptr (Reference.Storage).all);
            Storage   : Data_Base_Object'Class renames
               Data_Base_Object'Class (Data_Bank);
            Key : Object_Key renames Object_Key (Reference.Key.all);
         begin
            Result :=
               new APQ_Lecture_Object (Storage'Unchecked_Access);
            declare
               Index    : Feature_Index;
               Lesson   : APQ_Lecture_Object renames
                             APQ_Lecture_Object (Result.all);
               Features : Keys.Sets.Set;
            begin
               Standard.APQ.Links.Get_References
               (  Data_Base  => Storage.Data_Base,
                  Table_Name => Backward_Links_Table,
                  Dependant  => Key.ID,
                  Referents  => Features
               );
               Lesson.Reference := Reference.Storage;
               Lesson.ID        := Key.ID;
               Lesson.Restoring := True;
               for Item in 1..Get_Size (Features) loop
                  Index :=
                     Get_Feature
                     (  Lesson'Unchecked_Access,
                        To_Feature_Object_Ptr
                        (  Ptr
                           (  Get
                              (  Data_Bank'Access,
                                 Object_Key'
                                 (  Persistent_Key
                                 with
                                    Get (Features, Item)
                        )  )  )  ) .all,
                        Has_In
                     );
               end loop;
               Lesson.Restoring := False;
               Prepare_Tables (Lesson);
               Reset_Modified (Lesson);
            end;
         exception
            when Constraint_Error =>
               raise Ada.IO_Exceptions.Data_Error;
         end;
      else
         Create
         (  Source,
            Pointer,
            Fuzzy.Lecture.General.Class,
            List,
            Result
         );
      end if;
      Lesson := Result;
   exception
      when others =>
         Free (Result);
         raise;
   end Restore;

   procedure Set_Undefined
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
      Index : Feature_Index;
      ID    : Object_ID;
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Index := Get (Lesson.To_Index, Feature.ID);
         if Index /= 0 then
             ID :=
               APQ_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            declare
               Mutex : Write_Mutex (Lesson.Storage);
            begin
               for Image in Image_Type'Range loop
                  Set_Undefined
                  (  Lesson.Storage.Data_Base,
                     Lesson.Table (Image).all,
                     Example,
                     ID
                  );
               end loop;
               Commit (Mutex);
            end;
         end if;
      end if;
      Notify_Undefined (Lesson, Example, Feature, Image);
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out APQ_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
      Index : Feature_Index;
      ID    : Object_ID;
   begin
      if (  Lesson.To_Index.Vector /= null
         and then
            Feature.ID in Lesson.To_Index.Vector'Range
         )
      then
         Index := Get (Lesson.To_Index, Feature.ID);
         if Index /= 0 then
            ID :=
               APQ_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            declare
               Mutex : Write_Mutex (Lesson.Storage);
            begin
               for Image in Image_Type'Range loop
                  Set_Undefined
                  (  Lesson.Storage.Data_Base,
                     Lesson.Table (Image).all,
                     ID
                  );
               end loop;
               Delete_Feature (Lesson, Index);
               Put
               (  Lesson.Storage.all,
                  Object_Key'(Persistent_Key with Lesson.ID),
                  Lesson
               );
               Reset_Modified (Lesson);
               Commit (Mutex);
            end;
         end if;
      end if;
      Notify_Undefined (Lesson, Feature, Image);
   end Set_Undefined;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : APQ_Lecture_Object
             )  is
   begin
      Generic_Store (Destination, Pointer, Lesson);
   end Store;

begin
   Register_Class (Class, Restore'Access);
end Persistent.APQ.Lectures;
