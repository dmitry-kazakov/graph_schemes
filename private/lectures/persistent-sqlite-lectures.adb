--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.SQLite.Lectures                  Luebeck            --
--  Imaplementation                                Winter, 2010       --
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
with Persistent.Data_Bank.Reference; use Persistent.Data_Bank.Reference;
with Persistent.SQLite_Keys;         use Persistent.SQLite_Keys;
with Persistent.SQLite_Keys.Sets;    use Persistent.SQLite_Keys.Sets;
with Persistent.SQLite_Links;        use Persistent.SQLite_Links;
with SQLite;                         use SQLite;

with Ada.Unchecked_Deallocation;
with Fuzzy.Lecture.General;
with Fuzzy.Stream_IO;

package body Persistent.SQLite.Lectures is
   use ID_To_Index_Map;
   use Index_To_Feature_Map;
--
-- Prepare_Tables -- Creating all things necessary
--
--    Lesson        - The training set
--    Create_Tables - Creating set tables
--
   procedure Prepare_Tables
             (  Lesson        : in out SQLite_Lecture_Object;
                Create_Tables : Boolean := False
             );
   procedure Set_Undefined
             (  Storage    : Data_Base;
                Table_Name : String;
                Feature    : Object_ID
             );
   procedure Set_Undefined
             (  Storage    : Data_Base;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             );

   procedure Free is
      new Ada.Unchecked_Deallocation (String, String_Ptr);

   function Add_Feature
            (  Lesson  : not null access SQLite_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is
      use Feature_Descriptor_Handles;
      Handle : constant Deposit_Handle :=
                  Ref (To_Deposit_Ptr (Feature.Self));
      Link   : constant Feature_Descriptor_Ptr :=
                  new SQLite_Feature_Descriptor (Lesson);
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
         SQLite_Feature_Descriptor (Link.all).ID := Key.ID;
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
             (  Lesson : in out SQLite_Lecture_Object
             )  is
   begin
      Seize_Write (Lesson.Storage.all);
   end Begin_Bulk_Update;

   procedure Create_Table (Storage : Data_Base; Table_Name : String) is
   begin
      Exec
      (  Storage,
         (  "CREATE TABLE IF NOT EXISTS "
         &  Table_Name
         &  "(data BLOB, example_no INTEGER, feature_id INTEGER)"
      )  );
   exception
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Create_Table;

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
                    new SQLite_Lecture_Object
                        (  Where'Unchecked_Access,
                           Cache_Index (Size)
                        );
      This   : SQLite_Lecture_Object
                  renames SQLite_Lecture_Object (Result.all);
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
                    new SQLite_Lecture_Object
                        (  Where'Unchecked_Access,
                           Cache_Index (Size)
                        );
      This   : SQLite_Lecture_Object
                  renames SQLite_Lecture_Object (Result.all);
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

   procedure End_Bulk_Update (Lesson : in out SQLite_Lecture_Object) is
   begin
      Commit (Lesson.Storage.all);
   end End_Bulk_Update;

   procedure Finalize (Link : in out SQLite_Feature_Descriptor) is
      Lesson : SQLite_Lecture_Object renames
               SQLite_Lecture_Object (Link.Lesson.all);
   begin
      Remove (Lesson.By_Key, Object_Key'(Persistent_Key with Link.ID));
      Finalize (Feature_Descriptor (Link));
   end Finalize;

   procedure Finalize (Lesson : in out SQLite_Lecture_Object) is
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
            for Image in Lesson.Table'Range loop
               if Lesson.Table (Image) /= null then
                  begin
                     Exec
                     (  Lesson.Storage.Connection,
                        (  "DROP TABLE IF EXISTS "
                        &  Lesson.Table (Image).all
                     )  );
                  exception
                     when others =>
                        null;
                  end;
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

   procedure Get
             (  Storage    : Data_Base;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : in out Fuzzy.Set
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage,
            (  "SELECT data FROM "
            &  Table_Name
            &  " WHERE example_no = ? AND feature_id = ?"
         )  );
      Bind (Command, 1, Integer_64 (Example));
      Bind (Command, 2, Feature);
      if Step (Command) then
         Value := Fuzzy.Stream_IO.Value (Column (Command, 1));
      else
         Value := (others => Confidence'Last);
      end if;
   exception
      when End_Error =>
         Raise_Exception
         (  Data_Error'Identity,
            "Table '" & Table_Name & "' is corrupted"
         );
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get;

   function Get_Examples_No
            (  Storage    : Data_Base;
               Table_Name : String
            )  return Natural is
      Command : Statement;
   begin
      Command :=
         Prepare (Storage, "SELECT max(example_no) FROM " & Table_Name);
      if Step (Command) then
         return Natural (Integer_64'(Column (Command, 1)));
      else
         return 0;
      end if;
   exception
      when End_Error =>
         return 0;
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Get_Examples_No;

   function Get_Class (Lesson : SQLite_Lecture_Object) return String is
   begin
      return Class;
   end Get_Class;

   function Get_Examples_Number (Lesson : SQLite_Lecture_Object)
      return Natural is
      Result : Natural := Cache_Get_Examples_Number (Lesson);
      Mutex  : Read_Mutex (Lesson.Storage);
   begin
      for Image in Image_Type loop
         Result :=
            Natural'Max
            (  Get_Examples_No
               (  Lesson.Storage.Connection,
                  Lesson.Table (Image).all
               ),
               Result
            );
      end loop;
      Commit (Mutex);
      return Result;
   end Get_Examples_Number;

   procedure Get_Referents
             (  Lesson : SQLite_Lecture_Object;
                List   : in out Deposit_Container'Class
             )  is
   begin
      Add
      (  List,
         Lesson.Reference,
         Object_Key'(Persistent_Key with Lesson.ID)
      );
      Get_Referents (Connotated_Lecture_Object (Lesson), List);
   end Get_Referents;

   function Is_Defined
            (  Storage    : Data_Base;
               Table_Name : String;
               Example    : Positive;
               Feature    : Object_ID
            )  return Boolean is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage,
            (  "SELECT data FROM "
            &  Table_Name
            &  " WHERE example_no = ? AND feature_id = ?"
         )  );
      Bind (Command, 1, Integer_64 (Example));
      Bind (Command, 2, Feature);
      return Step (Command);
   exception
      when End_Error =>
         return False;
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Is_Defined;

   procedure Prepare_Tables
             (  Lesson        : in out SQLite_Lecture_Object;
                Create_Tables : Boolean := False
             )  is
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
      if Create_Tables then
         for Index in Lesson.Table'Range loop
            Create_Table
            (  Storage    => Lesson.Storage.Connection,
               Table_Name => Lesson.Table (Index).all
            );
         end loop;
      end if;
   end Prepare_Tables;

   procedure Put
             (  Storage    : Data_Base;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID;
                Value      : access Stream_Element_Array
             )  is
      pragma Inline (Put);
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage,
            (  "INSERT OR REPLACE INTO "
            &  Table_Name
            &  " VALUES (?, ?, ?)"
         )  );
      Bind (Command, 1, Value);
      Bind (Command, 2, Integer_64 (Example));
      Bind (Command, 3, Feature);
      Step (Command);
   exception
      when Data_Error =>
         raise;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Put;

   procedure Put
             (  Storage       : Data_Base;
                Table_Name    : String;
                Example       : Positive;
                Feature       : Object_ID;
                Value         : Positive;
                Cardinality   : Positive
             )  is
      pragma Inline (Put);
      Data : Fuzzy.Set (1..Cardinality);
   begin
      declare
         Command : Statement;
      begin
         Command :=
            Prepare
            (  Storage,
               (  "SELECT data FROM "
               &  Table_Name
               &  " WHERE example_no = ? AND feature_id = ?"
            )  );
         Bind (Command, 1, Integer_64 (Example));
         Bind (Command, 2, Feature);
         if Step (Command) then
            Data := Fuzzy.Stream_IO.Value (Column (Command, 1));
         else
            Data := (others => Confidence'First);
         end if;
      exception
         when End_Error =>
            Data := (others => Confidence'First);
         when Data_Error =>
            raise;
         when Error : others =>
            Raise_Exception
            (  Data_Error'Identity,
               Exception_Message (Error)
            );
      end;
      Data (Value) := Confidence'Last;
      declare
         Value : aliased Stream_Element_Array :=
                         Fuzzy.Stream_IO.Image (Data);
      begin
         Put (Storage, Table_Name, Example, Feature, Value'Access);
      end;
   end Put;

   function Raw_Get
            (  Lesson  : SQLite_Lecture_Object;
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
               (  Lesson.Storage.Connection,
                  Lesson.Table (Image).all,
                  Example,
                  SQLite_Feature_Descriptor
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
            (  Lesson  : SQLite_Lecture_Object;
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
                  (  Lesson.Storage.Connection,
                     Lesson.Table (Image).all,
                     Example,
                     SQLite_Feature_Descriptor
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
            (  Lesson  : SQLite_Lecture_Object;
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
             (  Lesson  : in out SQLite_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             )  is
   begin
      declare
         Data  : aliased Stream_Element_Array :=
                         Fuzzy.Stream_IO.Image (Value);
         Mutex : Write_Mutex (Lesson.Storage);
      begin
         Store_Object
         (  Lesson.Storage.all,
            To_Deposit_Ptr (Feature.Self)
         );
         Put
         (  Lesson.Storage.Connection,
            Lesson.Table (Image).all,
            Example,
            SQLite_Feature_Descriptor
            (  Get
               (  Lesson.To_Feature,
                  Get_Feature
                  (  Lesson'Unchecked_Access,
                     Feature,
                     Image
                  )
               ) .all
            ) .ID,
            Data'Access
         );
         Commit (Mutex);
      end;
      Notify_Changed (Lesson, Example, Feature, Image);
   end Raw_Put;

   procedure Raw_Put
             (  Lesson  : in out SQLite_Lecture_Object;
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
         (  Lesson.Storage.Connection,
            Lesson.Table (Image).all,
            Example,
            SQLite_Feature_Descriptor
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
             (  Lesson  : in out SQLite_Lecture_Object;
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
               SQLite_Feature_Descriptor
               (  Get (Lesson.To_Feature, Index).all
               ) .ID;
            declare
               Mutex : Write_Mutex (Lesson.Storage);
            begin
               for Image in Image_Type'Range loop
                  Set_Undefined
                  (  Lesson.Storage.Connection,
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
             (  Lesson  : in out SQLite_Lecture_Object;
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
               SQLite_Feature_Descriptor
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
                     (  Lesson.Storage.Connection,
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
             (  Lesson  : in out SQLite_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             )  is
      Mutex   : Read_Mutex (Lesson.Storage);
      Command : Statement;
      Current : Integer;
   begin
      for Image in Image_Type loop
         begin
            Command :=
               Prepare
               (  Lesson.Storage.Connection,
                  (  "SELECT feature_id,data FROM "
                  &  Lesson.Table (Image).all
                  &  " WHERE example_no = ?"
               )  );
            Bind (Command, 1, Integer_64 (Example));
            while Step (Command) loop
               Current :=
                  Find
                  (  Lesson.By_Key,
                     Object_Key'
                     (  Persistent_Key
                     with
                        Column (Command, 1)
                  )  );
               if Current > 0 then
                  declare
                     Feature : Feature_Object'Class renames
                        Ptr (Get (Lesson.By_Key, Current).Feature).all;
                     Value   : constant Stream_Element_Array :=
                        Column (Command, 2);
                  begin
                     if Value'Length > 0 then
                        Cache_Put
                        (  Lesson,
                           Index,
                           Feature,
                           Image,
                           Fuzzy.Stream_IO.Value (Value)
                        );
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
               Raise_Exception
               (  Data_Error'Identity,
                  (  "Table '"
                  &  Lesson.Table (Image).all
                  &  "' is corrupted"
               )  );
            when Data_Error =>
               raise;
            when Error : others =>
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
               new SQLite_Lecture_Object
                   (  Storage'Unchecked_Access,
                      Cache_Index (Default_Cache_Size)
                   );
            declare
               Index    : Feature_Index;
               Lesson   : SQLite_Lecture_Object renames
                             SQLite_Lecture_Object (Result.all);
               Features : Persistent.SQLite_Keys.Sets.Set;
            begin
               Lesson.Restoring := True;
               Get_References
               (  Base       => Lesson.Storage.Connection,
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
             (  Storage    : Data_Base;
                Table_Name : String;
                Example    : Positive;
                Feature    : Object_ID
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage,
            (  "DELETE FROM "
            &  Table_Name
            &  " WHERE example_no = ? AND feature_id = ?"
         )  );
      Bind (Command, 1, Integer_64 (Example));
      Bind (Command, 2, Feature);
      Step (Command);
   exception
      when End_Error =>
         return;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Set_Undefined;

   procedure Set_Undefined
             (  Storage    : Data_Base;
                Table_Name : String;
                Feature    : Object_ID
             )  is
      Command : Statement;
   begin
      Command :=
         Prepare
         (  Storage,
            (  "DELETE FROM "
            &  Table_Name
            &  " WHERE feature_id = ?"
         )  );
      Bind (Command, 1, Feature);
      Step (Command);
   exception
      when End_Error =>
         return;
      when Error : others =>
         Raise_Exception
         (  Data_Error'Identity,
            Exception_Message (Error)
         );
   end Set_Undefined;

   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : SQLite_Lecture_Object
             )  is
   begin
      Generic_Store (Destination, Pointer, Lesson);
   end Store;

   procedure Write (Lesson : in out SQLite_Lecture_Object) is
      Mutex : Write_Mutex (Lesson.Storage);
   begin
      Write (Caching_Lecture_Object (Lesson));
      Commit (Mutex);
   end Write;

begin
   Register_Class (Class, Restore'Access);
end Persistent.SQLite.Lectures;
