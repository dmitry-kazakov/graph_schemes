--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.Native_ODBC.Lectures             Luebeck            --
--  Interface                                      Autumn, 2012       --
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

package Persistent.Native_ODBC.Lectures is
   pragma Elaborate_Body (Persistent.Native_ODBC.Lectures);
--
-- Class -- Name of the class of training sets in ODBC data base
--
   Class : constant String := Lecture_Class & "ODBC";
--
-- Create -- Create an ODBC training set
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
   type String_Array is array (Image_Type) of String_Ptr;
   package ID_To_Descriptor_Maps is
      new Generic_Map
          (  Key_Type    => Object_Key,
             Object_Type => Feature_Descriptor_Ptr
          );
   use ID_To_Descriptor_Maps;
--
-- ODBC_Lecture_Object -- A training set resident in a data base
--
--    Data_Base - The persistent storage
--    Reference - A handle to it (to prevent premature destruction)
--    ID        - The key there
--    Restoring - True when the object is being restored
--    Table     - The table of the names used
--    Command   - The commands prepared
--
   type ODBC_Lecture_Object
        (  Storage : not null access Data_Base_Object'Class;
           Size    : Cache_Index
        )  is new Caching_Lecture_Object (Size) with
   record
      Reference : Storage_Handle;
      ID        : Object_ID;
      Restoring : Boolean := False;
      Table     : String_Array;
      By_Key    : Map; -- Key to feature descriptor map
   end record;
--
-- ODBC_Feature_Descriptor -- Contains feature key
--
   type ODBC_Feature_Descriptor is new Feature_Descriptor with record
      ID : Object_ID;
   end record;
   procedure Finalize (Link : in out ODBC_Feature_Descriptor);
--
-- Add_Feature -- Overrides Fuzzy.Lecture.Connotated...
--
-- It updates the descriptor of the training set in the data base unless
-- Restoring is True.
--
   overriding
   function Add_Feature
            (  Lesson  : not null access ODBC_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle;
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
   procedure Finalize (Lesson : in out ODBC_Lecture_Object);
--
-- Get_Class -- Overrides Object.Archived...
--
   function Get_Class (Lesson : ODBC_Lecture_Object) return String;
   pragma Inline (Get_Class);
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   function Get_Examples_Number (Lesson : ODBC_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
   overriding
   procedure Get_Referents
             (  Lesson : ODBC_Lecture_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Raw_Is_Defined -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Read
--
   overriding
   function Raw_Is_Defined
            (  Lesson  : ODBC_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Raw_Get -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Read
--
   overriding
   function Raw_Get
            (  Lesson  : ODBC_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set;
--
-- Raw_Put -- Overrides Fuzzy.Lecture.Cache...
--
-- Effects :
--
--    Seize_Write
--
   overriding
   procedure Raw_Put
             (  Lesson  : in out ODBC_Lecture_Object;
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
             (  Lesson  : in out ODBC_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Read -- Overrides Fuzzy.Lecture.Cache...
--
   overriding
   procedure Read
             (  Lesson  : in out ODBC_Lecture_Object;
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
                Lesson      : ODBC_Lecture_Object
             );
--
-- Write -- Overrides Fuzzy.Lecture.Cache...
--
   overriding
   procedure Write (Lesson : in out ODBC_Lecture_Object);

end Persistent.Native_ODBC.Lectures;
