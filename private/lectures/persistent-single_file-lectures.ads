--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Single_File.Lectures             Luebeck            --
--  Interface                                      Autumn, 2014       --
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

with Fuzzy.Feature;             use Fuzzy.Feature;
with Fuzzy.Lecture;             use Fuzzy.Lecture;
with Fuzzy.Lecture.Cache;       use Fuzzy.Lecture.Cache;
with Fuzzy.Lecture.Connotated;  use Fuzzy.Lecture.Connotated;
with Fuzzy.Lecture.Handle;      use Fuzzy.Lecture.Handle;
with Object.Archived;           use Object.Archived;

with Generic_Map;
with Persistent.Data_Bank.Reference;
with Persistent.Memory_Pools.Streams.Generic_External_B_Tree;

package Persistent.Single_File.Lectures is
   pragma Elaborate_Body (Persistent.Single_File.Lectures);
--
-- Class -- Name of the class of training sets in ODBC data base
--
   Class : constant String := Lecture_Class & "SingleFile";
--
-- Create -- Create an SQLite training set
--
--    Storage  - The persistent storage (a handle to)
--  [ Name     - The name of the training (UTF-8 encoded string)
--    Parent ] - The parent object
--    Size     - Cache size (number of examples)
--
-- This function creates a new training set in an ODBC  data  base.  The
-- parameter  Storage is a handle to an ODBC persistent storage. Name is
-- the training set name in the data base.
--
-- Returns :
--
--    Handle to the training set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not an ODBC one
--    Data_Error       - Data base error
--    Name_Error       - Name conflict
--
-- Effects :
--
--    Seize_Write
--
   function Create
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory;
               Size    : Positive       := Default_Cache_Size
            )  return Lecture_Handle;
   function Create
            (  Storage : Storage_Handle;
               Size    : Positive := Default_Cache_Size
            )  return Lecture_Handle;
private
   use Persistent.Data_Bank.Reference;

   type Lecture_Reference is new Self_Reference with record
      Tree : Byte_Index;
   end record;
   function Image (Object : Lecture_Reference) return String;

   package ID_To_Descriptor_Maps is
      new Generic_Map
          (  Key_Type    => Object_Key,
             Object_Type => Feature_Descriptor_Ptr
          );
   use ID_To_Descriptor_Maps;

   type Example_Key is record
      Example : Positive;
      Feature : Object_ID;
      Image   : Image_Type;
   end record;
   function "<" (Left, Right : Example_Key) return Boolean;
   function Input (Stream : access Root_Stream_Type'Class)
      return Example_Key;
   function Input (Stream : access Root_Stream_Type'Class)
      return Fuzzy.Set;
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Value  : Example_Key
             );
   procedure Output
             (  Stream : access Root_Stream_Type'Class;
                Set    : Fuzzy.Set
             );
   package Example_Maps is
      new Persistent.Memory_Pools.Streams.Generic_External_B_Tree
          (  Key_Type     => Example_Key,
             Object_Type  => Fuzzy.Set,
             Input_Key    => Input,
             Input_Value  => Input,
             Output_Key   => Output,
             Output_Value => Output
          );
   use Example_Maps;
   type B_Tree_Ptr is access B_Tree;
--
-- Single_File_Lecture_Object -- A training set resident in a data base
--
--    Storage   - The persistent storage
--    Size      - Cache size
--    Reference - A handle to it (to prevent premature destruction)
--    ID        - The key there
--    Table     - The table of the names used
--
   type Single_File_Lecture_Object
        (  Storage : not null access Data_Base_Object'Class;
           Size    : Cache_Index
        )  is new Caching_Lecture_Object (Size) with
   record
      Reference : Storage_Handle;
      Restoring : Boolean := False;
      ID        : Object_ID;
      By_Key    : Map; -- Key to feature descriptor map
      Data      : B_Tree_Ptr;
   end record;
--
-- Single_File_Feature_Descriptor -- Contains feature key
--
   type Single_File_Feature_Descriptor is
      new Feature_Descriptor with
   record
      ID : Object_ID;
   end record;
   overriding
   procedure Finalize (Link : in out Single_File_Feature_Descriptor);
--
-- Add_Feature -- Overrides Fuzzy.Lecture.Connotated...
--
-- It updates the descriptor of the training set in the data base unless
-- Restoring is True.
--
   overriding
   function Add_Feature
            (  Lesson  : not null access Single_File_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle;
--
-- Begin_Bulk_Update -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure Begin_Bulk_Update
             (  Lesson : in out Single_File_Lecture_Object
             );
--
-- End_Bulk_Update -- Overrides Fuzzy.Lecture...
--
   overriding
   procedure End_Bulk_Update
             (  Lesson : in out Single_File_Lecture_Object
             );
--
-- Finalize -- Destructor
--
--    Lesson - The object
--
-- If the flag Dispose set, the teaching set is removed  from  the  data
-- base.
--
-- Effects :
--
--    Seize_Write
--
   overriding
   procedure Finalize (Lesson : in out Single_File_Lecture_Object);
--
-- Get_Class -- Overrides Object.Archived...
--
   overriding
   function Get_Class
            (  Lesson : Single_File_Lecture_Object
            )  return String;
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   overriding
   function Get_Examples_Number (Lesson : Single_File_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Lesson : Single_File_Lecture_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Raw_Get -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Read
--
   overriding
   function Raw_Get
            (  Lesson  : Single_File_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set;
--
-- Raw_Is_Defined -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Read
--
   overriding
   function Raw_Is_Defined
            (  Lesson  : Single_File_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Raw_Put -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Write
--
   overriding
   procedure Raw_Put
             (  Lesson  : in out Single_File_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             );
--
-- Raw_Set_Undefined -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Write
--
   overriding
   procedure Raw_Set_Undefined
             (  Lesson  : in out Single_File_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Read -- Overrides Fuzzy.Lecture.Cache...
--
   overriding
   procedure Read
             (  Lesson  : in out Single_File_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             );
--
-- Restore -- Implements Object.Archived...
--
   procedure Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : out Deposit_Ptr
             );
--
-- Store -- Overrides Object.Archived...
--
   overriding
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Single_File_Lecture_Object
             );
--
-- Write -- Overrides Fuzzy.Lecture.Cache...
--
   overriding
   procedure Write (Lesson : in out Single_File_Lecture_Object);

   pragma Inline (Get_Class);

end Persistent.Single_File.Lectures;
