--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Native_ODBC.Lectures             Luebeck            --
--  Imaplementation                                Autumn, 2012       --
--                                                                    --
--                                Last revision :  12:36 01 May 2021  --
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
with Fuzzy.Feature.Handle;           use Fuzzy.Feature.Handle;
with ODBC.API.Lectures;              use ODBC.API.Lectures;
with Object;                         use Object;
with Persistent.Data_Bank.Reference; use Persistent.Data_Bank.Reference;

with Ada.Unchecked_Deallocation;
with Fuzzy.Edit;
with Fuzzy.Lecture.General;
with ODBC.API.Links;
with Strings_Edit.Integers;

package body Persistent.Native_ODBC.Lectures is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;
--
-- Prepare_Tables -- Creating all things necessary
--
--    Lesson        - The training set
--    Create_Tables - Creating set tables
--
   procedure Prepare_Tables
             (  Lesson        : in out ODBC_Lecture_Object;
                Create_Tables : Boolean := False
             );

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   function Add_Feature
            (  Lesson  : not null access ODBC_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      use Feature_Descriptor_Handles;
      Handle : constant Deposit_Handle :=
                  Ref (To_Deposit_Ptr (Feature.Self));
      Link   : constant Feature_Descriptor_Ptr :=
                  new ODBC_Feature_Descriptor (Lesson);
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
         ODBC_Feature_Descriptor (Link.all).ID := Key.ID;
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

   procedure Prepare_Tables
             (  Lesson        : in out ODBC_Lecture_Object;
                Create_Tables : Boolean := False
             )  is
      Storage   : Data_Base_Object'Class renames Lesson.Storage.all;
      Command   : ODBC_Command'Class renames Storage.Command.all;
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
      if Create_Tables then
         for Index in Lesson.Table'Range loop
            Create_Table
            (  Command          => Command,
               Table_Name       => Lesson.Table (Index).all,
               Integer_SQL_Type =>
                  To_String (Lesson.Storage.Integer_SQL_Type.Name),
               ID_SQL_Type =>
                  To_String (Lesson.Storage.ID_SQL_Type.Name),
               Data_SQL_Type =>
                  To_String (Lesson.Storage.Data_SQL_Type.Name)
            );
         end loop;
      end if;
   end Prepare_Tables;

   function Create
            (  Storage : Storage_Handle;
               Size    : Positive := Default_Cache_Size
            )  return Lecture_Handle is
      Where  : Data_Base_Object'Class renames
                  Data_Base_Object'Class (Ptr (Storage).all);
      Key    : Object_Key;
      Empty  : Deposit_Set;
      Object : Deposit_Handle;
      Result : constant Deposit_Ptr :=
                    new ODBC_Lecture_Object
                        (  Where'Unchecked_Access,
                           Cache_Index (Size)
                        );
      This   : ODBC_Lecture_Object
                  renames ODBC_Lecture_Object (Result.all);
      Mutex  : Write_Mutex (Where'Unchecked_Access);
   begin
      Object := Ref (Result);
      This.Reference := Storage;
      Create
      (  Storage        => Where,
         Object         => Object,
         ID             => Key,
         Class          => Class,
         Data           => "",
         Parameters     => "@@",
         Direct_Links   => Empty,
         Backward_Links => Empty
      );
      This.ID := Key.ID;
      Prepare_Tables (This, True);
      Commit (Mutex);
      return Ref (To_Lecture_Object_Ptr (Result));
   end Create;

   function Create
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory;
               Size    : Positive       := Default_Cache_Size
            )  return Lecture_Handle is
      Where  : Data_Base_Object'Class renames
                  Data_Base_Object'Class (Ptr (Storage).all);
      Key    : Object_Key;
      Empty  : Deposit_Set;
      Object : Deposit_Handle;
      Result : constant Deposit_Ptr :=
                    new ODBC_Lecture_Object
                        (  Where'Unchecked_Access,
                           Cache_Index (Size)
                        );
      This   : ODBC_Lecture_Object
                  renames ODBC_Lecture_Object (Result.all);
      Mutex  : Write_Mutex (Where'Unchecked_Access);
   begin
      Object := Ref (Result);
      This.Reference := Storage;
      Create
      (  Storage        => Where,
         Object         => Object,
         ID             => Key,
         Name           => Name,
         Class          => Class,
         Data           => "",
         Parameters     => "@@",
         Parent         => Parent,
         Direct_Links   => Empty,
         Backward_Links => Empty
      );
      This.ID := Key.ID;
      Prepare_Tables (This, True);
      Commit (Mutex);
      return Ref (To_Lecture_Object_Ptr (Result));
   end Create;

   procedure Finalize (Link : in out ODBC_Feature_Descriptor) is
      Lesson : ODBC_Lecture_Object renames
               ODBC_Lecture_Object (Link.Lesson.all);
   begin
      Remove (Lesson.By_Key, Object_Key'(Persistent_Key with Link.ID));
      Finalize (Feature_Descriptor (Link));
   end Finalize;

   procedure Finalize (Lesson : in out ODBC_Lecture_Object) is
      Command : ODBC_Command'Class renames Lesson.Storage.Command.all;
      Key     : constant Object_Key := (Persistent_Key with Lesson.ID);
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
            for Image in Lesson.Table'Range loop
               if Lesson.Table (Image) /= null then
                  Drop (Command, Lesson.Table (Image).all);
               end if;
            end loop;
            Drop (Lesson);
         else
            Write (Lesson);
         end if;
         Commit (Mutex);
      end;
      Close (Lesson);
      for Image in Image_Type loop
         Free (Lesson.Table (Image));
      end loop;
      Finalize (Caching_Lecture_Object (Lesson));
   exception
      when others =>
         for Image in Image_Type loop
            Free (Lesson.Table (Image));
         end loop;
         Drop (Lesson);
         Finalize (Caching_Lecture_Object (Lesson));
         raise;
   end Finalize;

   function Get_Class (Lesson : ODBC_Lecture_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : ODBC_Lecture_Object)
      return Natural is
      Result : Natural := Cache_Get_Examples_Number (Lesson);
      Mutex  : Read_Mutex (Lesson.Storage);
   begin
      for Image in Image_Type loop
         Result :=
            Natural'Max
            (  Get_Examples_No
               (  Lesson.Storage.Command,
                  Lesson.Table (Image).all
               ),
               Result
            );
      end loop;
      Commit (Mutex);
      return Result;
   end Get_Examples_Number;

   procedure Get_Referents
             (  Lesson : ODBC_Lecture_Object;
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

   function Raw_Get
            (  Lesson  : ODBC_Lecture_Object;
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
               (  Lesson.Storage.Command.all,
                  Lesson.Table (Image).all,
                  Example,
                  ODBC_Feature_Descriptor
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
   end Raw_Get;

   function Raw_Is_Defined
            (  Lesson  : ODBC_Lecture_Object;
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
                  (  Lesson.Storage.Command,
                     Lesson.Table (Image).all,
                     Example,
                     ODBC_Feature_Descriptor
                     (  Get (Lesson.To_Feature, Index).all
                     ) .ID
                  );
               Commit (Mutex);
            end;
         end if;
      end if;
      return Result;
   end Raw_Is_Defined;

   function Raw_Is_Known
            (  Lesson  : ODBC_Lecture_Object;
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
             (  Lesson  : in out ODBC_Lecture_Object;
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
         Put
         (  Lesson.Storage.Command.all,
            Lesson.Table (Image).all,
            Lesson.Storage.Data_SQL_Type.Data_Type,
            Example,
            ODBC_Feature_Descriptor
            (  Get
               (  Lesson.To_Feature,
                  Get_Feature
                  (  Lesson'Unchecked_Access,
                     Feature,
                     Image
                  )
                ) .all
            ) .ID,
            Value
         );
         Commit (Mutex);
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Raw_Put;

   procedure Raw_Put
             (  Lesson  : in out ODBC_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
   begin
      declare
         Mutex : Write_Mutex (Lesson.Storage);
      begin
         Store_Object
         (  Lesson.Storage.all,
            To_Deposit_Ptr (Feature.Self)
         );
         Put
         (  Lesson.Storage.Command.all,
            Lesson.Table (Image).all,
            Lesson.Storage.Data_SQL_Type.Data_Type,
            Example,
            ODBC_Feature_Descriptor
            (  Get
               (  Lesson.To_Feature,
                  Get_Feature
                  (  Lesson'Unchecked_Access,
                     Feature,
                     Image
                  )
               ) .all
            ) .ID,
            Value,
            Feature.Cardinality
         );
         Commit (Mutex);
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Raw_Put;

   procedure Raw_Set_Undefined
             (  Lesson  : in out ODBC_Lecture_Object;
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
               ODBC_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            declare
               Mutex : Write_Mutex (Lesson.Storage);
            begin
               for Image in Image_Type'Range loop
                  Set_Undefined
                  (  Lesson.Storage.Command.all,
                     Lesson.Table (Image).all,
                     Example,
                     ID
                  );
               end loop;
               Commit (Mutex);
            end;
         end if;
      end if;
      Notify_Undefined (Lesson, Example, Feature);
   end Raw_Set_Undefined;

   procedure Raw_Set_Undefined
             (  Lesson  : in out ODBC_Lecture_Object;
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
               ODBC_Feature_Descriptor
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
               begin
                  for Image in Image_Type'Range loop
                     Set_Undefined
                     (  Lesson.Storage.Command.all,
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
      end if;
      Notify_Undefined (Lesson, Feature);
   end Raw_Set_Undefined;

   procedure Read
             (  Lesson  : in out ODBC_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             )  is
      Mutex   : Read_Mutex (Lesson.Storage);
      Storage : Data_Base_Object'Class renames Lesson.Storage.all;
      Command : ODBC_Command'Class renames Storage.Command.all;
      Current : Integer;
   begin
      for Image in Image_Type loop
         begin
            Execute
            (  Command,
               (  "SELECT feature_id,data FROM "
               &  Lesson.Table (Image).all
               &  " WHERE example_no ="
               &  Strings_Edit.Integers.Image (Positive (Index))
            )  );
            while SQL_SUCCESS = Fetch (Command'Access) loop
               Current :=
                  Find
                  (  Lesson.By_Key,
                     Object_Key'
                     (  Persistent_Key
                     with
                        Get_Data (Command'Access, 1, Never)
                  )  );
               if Current > 0 then
                  declare
                     Feature : Feature_Object'Class renames
                        Ptr (Get (Lesson.By_Key, Current).Feature).all;
                     Value   : constant String :=
                        Get_Data
                        (  Command'Unchecked_Access,
                           1,
                           Never
                        );
                  begin
                     if Value'Length > 0 then
                        Cache_Put
                        (  Lesson,
                           Index,
                           Feature,
                           Image,
                           Fuzzy.Edit.Value
                           (  Value,
                              1,
                              Feature.Cardinality
                        )  );
                     else
                        Cache_Set_Undefined
                        (  Lesson,
                           Index,
                           Feature,
                           Image
                        );
                     end if;
                  end;
               end if;
            end loop;
         exception
            when End_Error =>
               Close_Cursor (Command);
            when Constraint_Error =>
               Close_Cursor (Command);
               Raise_Exception
               (  Ada.IO_Exceptions.Data_Error'Identity,
                  (  "Table '"
                  &  Lesson.Table (Image).all
                  &  "' is corrupted"
               )  );
            when Data_Error =>
               Close_Cursor (Command);
               raise;
            when Error : others =>
               Close_Cursor (Command);
               Raise_Exception
               (  Data_Error'Identity,
                  Exception_Message (Error)
              );
         end;
      end loop;
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
            Reference : Self_Reference'Class renames
               Self_Reference'Class (Get (List, 1).all);
            Data_Bank : Data_Bank_Object'Class renames
               Data_Bank_Object'Class (Ptr (Reference.Storage).all);
            Storage   : Data_Base_Object'Class renames
               Data_Base_Object'Class (Data_Bank);
            Key : Object_Key renames Object_Key (Reference.Key.all);
         begin
            Result :=
               new ODBC_Lecture_Object
                   (  Storage'Unchecked_Access,
                      Cache_Index (Default_Cache_Size)
                   );
            declare
               Index    : Feature_Index;
               Lesson   : ODBC_Lecture_Object renames
                             ODBC_Lecture_Object (Result.all);
               Features : Keys.Sets.Set;
            begin
               Lesson.Restoring := True;
               ODBC.API.Links.Get_References
               (  Command    => Storage.Command.all,
                  Table_Name => Backward_Links_Table,
                  Dependant  => Key.ID,
                  Referents  => Features
               );
               Lesson.Reference := Reference.Storage;
               Lesson.ID        := Key.ID;
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
               raise Data_Error;
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
                Lesson      : ODBC_Lecture_Object
             )  is
   begin
      Generic_Store (Destination, Pointer, Lesson);
   end Store;

   procedure Write (Lesson : in out ODBC_Lecture_Object) is
      Mutex : Write_Mutex (Lesson.Storage);
   begin
      Write (Caching_Lecture_Object (Lesson));
      Commit (Mutex);
   end Write;

begin
   Register_Class (Class, Restore'Access);
end Persistent.Native_ODBC.Lectures;
