--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File.Lectures             Luebeck            --
--  Imaplementation                                Autumn, 2014       --
--                                                                    --
--                                Last revision :  13:17 01 May 2021  --
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
with Ada.Streams;                    use Ada.Streams;
with Confidence_Factors;             use Confidence_Factors;
with Fuzzy;                          use Fuzzy;
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with Interfaces;                     use Interfaces;
with Object;                         use Object;
with Persistent.Single_File;         use Persistent.Single_File;
with Strings_Edit.Streams.Naturals;  use Strings_Edit.Streams.Naturals;

with Ada.Unchecked_Deallocation;
with Fuzzy.Lecture.General;
with Fuzzy.Stream_IO;
with Persistent.Blocking_Files.Text_IO;
with Persistent.Single_File_Keys.Sets;

package body Persistent.Single_File.Lectures is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;
   use Persistent.Single_File_Keys.Sets;
   use Persistent.Blocking_Files.Text_IO;

   function "<" (Left, Right : Example_Key) return Boolean is
   begin
      return
      (  Left.Example < Right.Example
      or else
         (  Left.Example = Right.Example
         and then
            (  Left.Feature < Right.Feature
            or else
               (  Left.Feature = Right.Feature
               and then
                  (  Image_Type'Pos (Left.Image)
                  <  Image_Type'Pos (Right.Image)
      )  )  )  )  );
   end "<";

   function Input (Stream : access Root_Stream_Type'Class)
      return Example_Key is
      Result : Example_Key;
   begin
      Result.Example := Input (Stream);
      Result.Feature := Object_ID (Natural'(Input (Stream)));
      Result.Image   := Image_Type'Val (Natural'(Input (Stream)));
      return Result;
   end Input;

   function Input (Stream : access Root_Stream_Type'Class)
      return Fuzzy.Set is
      use Fuzzy.Stream_IO;
      Length : constant Natural := Input (Stream);
      Data   : Stream_Element_Array (1..Stream_Element_Offset (Length));
      Last   : Stream_Element_Offset;
   begin
      Read (Stream.all, Data, Last);
      if Last /= Data'Last then
         raise Data_Error;
      end if;
      return Value (Data);
   end Input;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Example_Key
             )  is
   begin
      Output (Stream, Value.Example);
      Output (Stream, Natural (Value.Feature));
      Output (Stream, Natural (Image_Type'Pos (Value.Image)));
   end Output;

   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Set    : Fuzzy.Set
             )  is
      use Fuzzy.Stream_IO;
      Data : constant Stream_Element_Array := Image (Set);
   begin
      Output (Stream, Natural (Data'Length));
      Write (Stream.all, Data);
   end Output;

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   function Add_Feature
            (  Lesson  : not null access Single_File_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      use Feature_Descriptor_Handles;
      Handle : constant Deposit_Handle :=
                  Ref (To_Deposit_Ptr (Feature.Self));
      Link   : constant Feature_Descriptor_Ptr :=
                  new Single_File_Feature_Descriptor (Lesson);
      Result : constant Feature_Descriptor_Handles.Handle := Ref (Link);
   begin
      if not Lesson.Restoring then
         --
         -- Store the feature, if necessary
         --
         Store_Object
         (  Lesson.Storage.all,
            To_Deposit_Ptr (Feature.Self)
         );
      end if;
      --
      -- Get the feature's ID
      --
      declare
         Key : constant Object_Key :=
               Object_Key (Get_Key (Lesson.Storage, Handle));
      begin
         Single_File_Feature_Descriptor (Link.all).ID := Key.ID;
         Replace (Lesson.By_Key, Key, Link);
      end;
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

   procedure Begin_Bulk_Update
             (  Lesson : in out Single_File_Lecture_Object
             )  is
   begin
      Seize_Write (Lesson.Storage.all);
   end Begin_Bulk_Update;

   function Create
            (  Storage : Storage_Handle;
               Size    : Positive := Default_Cache_Size
            )  return Lecture_Handle is
      Where  : Data_Base_Object'Class renames
                  Data_Base_Object'Class (Ptr (Storage).all);
      Key     : Object_Key;
      Empty   : Deposit_Set;
      Lecture : Lecture_Handle;
      Result  : constant Deposit_Ptr :=
                     new Single_File_Lecture_Object
                         (  Where'Unchecked_Access,
                            Cache_Index (Size)
                         );
      This    : Single_File_Lecture_Object
                   renames Single_File_Lecture_Object (Result.all);
      Mutex   : Write_Mutex (Where'Unchecked_Access);
   begin
      Lecture := Ref (To_Lecture_Object_Ptr (Result));
      This.Reference := Storage;
      Create
      (  Storage        => Where,
         Object         => Ref (Result),
         ID             => Key,
         Class          => Class,
         Data           => "",
         Parameters     => "@0@",
         Direct_Links   => Empty,
         Backward_Links => Empty
      );
      This.ID := Key.ID;
      This.Data := new B_Tree (Where.Pool);
      Commit (Mutex);
      return Lecture;
   end Create;

   function Create
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory;
               Size    : Positive       := Default_Cache_Size
            )  return Lecture_Handle is
      Where   : Data_Base_Object'Class renames
                   Data_Base_Object'Class (Ptr (Storage).all);
      Key     : Object_Key;
      Empty   : Deposit_Set;
      Lecture : Lecture_Handle;
      Result  : constant Deposit_Ptr :=
                     new Single_File_Lecture_Object
                         (  Where'Unchecked_Access,
                            Cache_Index (Size)
                         );
      This    : Single_File_Lecture_Object
                   renames Single_File_Lecture_Object (Result.all);
      Mutex   : Write_Mutex (Where'Unchecked_Access);
   begin
      Lecture := Ref (To_Lecture_Object_Ptr (Result));
      This.Reference := Storage;
      Create
      (  Storage        => Where,
         Object         => Ref (Result),
         ID             => Key,
         Name           => Name,
         Class          => Class,
         Data           => "",
         Parameters     => "@0@",
         Parent         => Parent,
         Direct_Links   => Empty,
         Backward_Links => Empty
      );
      This.ID   := Key.ID;
      This.Data := new B_Tree (Where.Pool);
      Commit (Mutex);
      return Lecture;
   end Create;

   function Create
            (  Source  : String;
               Storage : Storage_Handle;
               Key     : Persistent_Key'Class
            )  return Deposit_Handle is
      Result : constant Deposit_Handle := Ref (new Lecture_Reference);
      This   : Lecture_Reference renames
               Lecture_Reference (Ptr (Result).all);
   begin
      This.Storage := Storage;
      This.Key     := new Persistent_Key'Class'(Key);
      This.Tree    := Value (Source);
      return Result;
   end Create;

   procedure End_Bulk_Update
             (  Lesson : in out Single_File_Lecture_Object
             )  is
   begin
      Commit (Lesson.Storage.all);
   end End_Bulk_Update;

   procedure Finalize (Link : in out Single_File_Feature_Descriptor) is
      Lesson : Single_File_Lecture_Object renames
               Single_File_Lecture_Object (Link.Lesson.all);
   begin
      Remove (Lesson.By_Key, Object_Key'(Persistent_Key with Link.ID));
      Finalize (Feature_Descriptor (Link));
   end Finalize;

   procedure Finalize (Lesson : in out Single_File_Lecture_Object) is
      procedure Free is
         new Ada.Unchecked_Deallocation (B_Tree, B_Tree_Ptr);
      Key : constant Object_Key := (Persistent_Key with Lesson.ID);
   begin
      declare
         Mutex   : Write_Mutex (Lesson.Storage);
         Deleted : Boolean;
      begin
         Deleted :=
            not
            (  Is_Named (Lesson.Storage, Key)
            or else
               Has_Dependants (Lesson.Storage, Key, True)
            );
         if Deleted then
            Erase (Lesson.Data.all);
            Drop (Lesson);
         else
            Write (Lesson);
         end if;
         Commit (Mutex);
      end;
      Close (Lesson);
      Finalize (Caching_Lecture_Object (Lesson));
      Free (Lesson.Data);
   exception
      when others =>
         Drop (Lesson);
         Finalize (Caching_Lecture_Object (Lesson));
         Free (Lesson.Data);
   end Finalize;

   function Get_Class
            (  Lesson : Single_File_Lecture_Object
            )  return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : Single_File_Lecture_Object)
      return Natural is
      Result : Natural := Cache_Get_Examples_Number (Lesson);
      Mutex  : Read_Mutex (Lesson.Storage);
      This   : constant Item_Ptr := Get_Last (Lesson.Data.all);
   begin
      if This /= No_Item then
         Result := Natural'Max (Get_Key (This).Example, Result);
      end if;
      Commit (Mutex);
      return Result;
   end Get_Examples_Number;

   procedure Get_Referents
             (  Lesson : Single_File_Lecture_Object;
                List   : in out Deposit_Container'Class
             )  is
   begin
      declare
         Self : Deposit_Ptr := new Lecture_Reference;
         This : Lecture_Reference renames Lecture_Reference (Self.all);
      begin
         This.Storage := Lesson.Reference;
         This.Key     := new Object_Key'(Persistent_Key with Lesson.ID);
         if Lesson.Data = null then
            This.Tree := 0;
         else
            This.Tree := Get_Root_Address (Lesson.Data.all);
         end if;
         Add (List, Self, False);
      exception
         when others =>
            Free (Self);
            raise;
      end;
      Get_Referents (Connotated_Lecture_Object (Lesson), List);
   end Get_Referents;

   function Image (Object : Lecture_Reference) return String is
   begin
      return Image (Object.Tree);
   end Image;

   function Raw_Get
            (  Lesson  : Single_File_Lecture_Object;
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
               This  : constant Item_Ptr :=
                       Find
                       (  Lesson.Data.all,
                          (  Example,
                             Single_File_Feature_Descriptor
                             (  Get (Lesson.To_Feature, Index).all
                             ) .ID,
                             Image
                       )  );
            begin
               if This = No_Item then
                  Result := (others => Confidence'Last);
               else
                  Result := Get_Value (This);
               end if;
               Commit (Mutex);
            end;
         else
            Result := (others => Confidence'Last);
         end if;
      else
         Result := (others => Confidence'Last);
      end if;
      return Result;
   end Raw_Get;

   function Raw_Is_Defined
            (  Lesson  : Single_File_Lecture_Object;
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
               This  : constant Item_Ptr :=
                       Find
                       (  Lesson.Data.all,
                          (  Example,
                             Single_File_Feature_Descriptor
                             (  Get (Lesson.To_Feature, Index).all
                             ) .ID,
                             Image
                       )  );
            begin
               Result := This /= No_Item;
               Commit (Mutex);
            end;
         end if;
      end if;
      return Result;
   end Raw_Is_Defined;

   function Raw_Is_Known
            (  Lesson  : Single_File_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      return
      (  Necessity (Raw_Get (Lesson, Example, Feature, Image))
      /= Confidence'Last
      );
   end Raw_Is_Known;

   procedure Raw_Put
             (  Lesson  : in out Single_File_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             )  is
   begin
      declare
         Mutex : Write_Mutex (Lesson.Storage);
      begin
         Store_Object
         (  Lesson.Storage.all,
            To_Deposit_Ptr (Feature.Self)
         );
         Replace
         (  Lesson.Data.all,
            (  Example,
               Single_File_Feature_Descriptor
               (  Get
                  (  Lesson.To_Feature,
                     Get_Feature
                     (  Lesson'Unchecked_Access,
                        Feature,
                        Image
                     )
                  ) .all
               ) .ID,
               Image
            ),
            Value
         );
         Commit (Mutex);
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Raw_Put;

   procedure Raw_Put
             (  Lesson  : in out Single_File_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
      Set : Fuzzy.Set (1..Feature.Cardinality);
   begin
      declare
         Mutex : Write_Mutex (Lesson.Storage);
         This  : Item_Ptr;
         ID    : Object_ID;
      begin
         Store_Object
         (  Lesson.Storage.all,
            To_Deposit_Ptr (Feature.Self)
         );
         ID := Single_File_Feature_Descriptor
               (  Get
                  (  Lesson.To_Feature,
                     Get_Feature
                     (  Lesson'Unchecked_Access,
                        Feature,
                        Image
                     )
                  ) .all
               ) .ID;
         This := Find (Lesson.Data.all, (Example, ID, Image));
         if This = No_Item then
            Set := (others => Confidence'First);
         end if;
         Set (Value) := Confidence'Last;
         Replace (This, Set);
         Commit (Mutex);
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Raw_Put;

   procedure Raw_Set_Undefined
             (  Lesson  : in out Single_File_Lecture_Object;
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
               Single_File_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            declare
               Mutex : Write_Mutex (Lesson.Storage);
            begin
               for Image in Image_Type'Range loop
                  Remove (Lesson.Data.all, (Example, ID, Image));
               end loop;
               Commit (Mutex);
            end;
         end if;
      end if;
      Notify_Undefined (Lesson, Example, Feature);
   end Raw_Set_Undefined;

   procedure Raw_Set_Undefined
             (  Lesson  : in out Single_File_Lecture_Object;
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
               Single_File_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            if Lesson.Restoring then
               --
               -- The object is under restoring, so there is no need  to
               -- change anything in the database.
               --
               Delete_Feature (Lesson, Index);
            else
               declare
                  Mutex : Write_Mutex (Lesson.Storage);
                  Count : Natural := 0;
                  This  : constant Item_Ptr :=
                          Get_Last (Lesson.Data.all);
               begin
                  if This /= No_Item then
                     Count :=
                        Natural'Max (Get_Key (This).Example, Count);
                  end if;
                  for No in 1..Count loop
                     for Image in Image_Type'Range loop
                        Remove (Lesson.Data.all, (No, ID, Image));
                     end loop;
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
      end if;
      Notify_Undefined (Lesson, Feature);
   end Raw_Set_Undefined;

   procedure Read
             (  Lesson  : in out Single_File_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             )  is
      Mutex   : Read_Mutex (Lesson.Storage);
      This    : Item_Ptr;
   begin
      This := Sup (Lesson.Data.all, (Example, 0, Image_Type'Val (0)));
      if This /= No_Item then
         loop
            declare
               Key : constant Example_Key := Get_Key (This);
            begin
               exit when Key.Example /= Example;
               begin
                  Cache_Put
                  (  Lesson,
                     Index,
                     Ptr
                     (  Get
                        (  Lesson.By_Key,
                           (Persistent_Key with Key.Feature)
                        ) .Feature
                     ) .all,
                     Key.Image,
                     Get_Value (This)
                  );
               end;
            exception
               when End_Error =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     (  "Lecture object "
                     &  Image (Lesson.ID)
                     &  " is corrupted"
                  )  );
               when Data_Error =>
                  raise;
               when Error : others =>
                  Raise_Exception
                  (  Data_Error'Identity,
                     Exception_Message (Error)
                  );
            end;
            This := Get_Next (This);
         end loop;
      end if;
      Commit (Mutex);
   end Read;

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
            Reference : Lecture_Reference'Class renames
               Lecture_Reference'Class (Get (List, 1).all);
            Data_Bank : Data_Bank_Object'Class renames
               Data_Bank_Object'Class (Ptr (Reference.Storage).all);
            Storage : Data_Base_Object'Class renames
               Data_Base_Object'Class (Data_Bank);
            Key : Object_Key renames Object_Key (Reference.Key.all);
         begin
            Result :=
               new Single_File_Lecture_Object
                   (  Storage'Unchecked_Access,
                      Cache_Index (Default_Cache_Size)
                   );
            declare
               Index    : Feature_Index;
               Lesson   : Single_File_Lecture_Object renames
                             Single_File_Lecture_Object (Result.all);
            begin
               Lesson.Restoring := True;
               Lesson.Reference := Reference.Storage;
               Lesson.ID        := Key.ID;
               for Item in 1..Get_Size (List) loop
                  declare
                     This : constant Deposit_Ptr := Get (List, Item);
                  begin
                     if (  Is_Backward (List, This)
                        and then
                           This.all in Feature_Object'Class
                        )  -- Have a feature
                     then
                        declare
                           Feature : Feature_Object'Class renames
                                     Feature_Object'Class (This.all);
                        begin
                           Index :=
                              Get_Feature
                              (  Lesson'Unchecked_Access,
                                 Feature,
                                 Has_In
                              );
                        end;
                     end if;
                  end;
               end loop;
               Lesson.Data := new B_Tree (Storage.Pool);
               if Reference.Tree > 0 then
                  Set_Root_Address (Lesson.Data.all, Reference.Tree);
               end if;
               Lesson.Restoring := False;
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

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Single_File_Lecture_Object
             )  is
   begin
      Generic_Store (Destination, Pointer, Lesson);
   end Store;

   procedure Write (Lesson : in out Single_File_Lecture_Object) is
      Mutex : Write_Mutex (Lesson.Storage);
   begin
      Write (Caching_Lecture_Object (Lesson));
      Commit (Mutex);
   end Write;

begin
   Register_Class (Class, Restore'Access);
   Register (Class, Create'Access);
end Persistent.Single_File.Lectures;
