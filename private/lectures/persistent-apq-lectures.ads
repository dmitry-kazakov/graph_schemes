--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Persistent.APQ.Lectures                     Luebeck            --
--  Interface                                      Winter, 2004       --
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

with Fuzzy.Feature;               use Fuzzy.Feature;
with Fuzzy.Lecture;               use Fuzzy.Lecture;
with Fuzzy.Lecture.Connotated;    use Fuzzy.Lecture.Connotated;
with Fuzzy.Lecture.Handle;        use Fuzzy.Lecture.Handle;
with Object.Archived;             use Object.Archived;

package Persistent.APQ.Lectures is
   pragma Elaborate_Body (Persistent.APQ.Lectures);
--
-- Class -- Name of the class of training sets in APQ data base
--
   Class : constant String := Lecture_Class & "APQ";
--
-- Create -- Create an APQ training set
--
--    Storage - The persistent storage (a handle to)
--    Name    - Of the training set there 
--
-- This function creates a new training set in a data base accessed  via
-- APQ.  The parameter Storage is a handle to an APQ persistent storage.
-- Name is the training set name in the data base. 
--
-- Returns :
--
--    Handle to the training set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not an APQ one
--    Data_Error       - Data base error
--    Name_Error       - Name conflict
--
-- Effects :
--
--    Seize_Write
--
   function Create
            (  Storage : Storage_Handle;
               Name    : Wide_String
            )  return Lecture_Handle;
private
   use Lectures;
   type String_Array is array (Image_Type) of String_Ptr;
--
-- APQ_Lecture_Object -- A training set resident in a data base
--
--    Data_Base - The persistent storage
--    Reference - A handle to it (to prevent premature destruction)
--    ID        - The key there
--    Restoring - True when the object is being restored
--    Table     - The table names used
--    Command   - The commands prepared
--
   type APQ_Lecture_Object
        (  Storage : access Data_Base_Object'Class
        )  is new Connotated_Lecture_Object with
   record
      Reference : Storage_Handle;
      ID        : Object_ID;
      Restoring : Boolean := False;
      Table     : String_Array;
   end record;
--
-- APQ_Feature_Descriptor -- Contains feature key
--
   type APQ_Feature_Descriptor is new Feature_Descriptor with record
      ID : Object_ID;
   end record;
--
-- Add_Feature -- Overrides Fuzzy.Lecture.Connotated...
--
-- It updates the descriptor of the training set in the data base unless
-- Restoring is True. 
--
   function Add_Feature
            (  Lesson  : access APQ_Lecture_Object;
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
   procedure Finalize (Lesson : in out APQ_Lecture_Object);
--
-- Get -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   function Get
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Fuzzy.Set;
--
-- Get_Class -- Overrides Object.Archived...
--
   function Get_Class (Lesson : APQ_Lecture_Object) return String;
   pragma Inline (Get_Class);
--
-- Get_Examples_Number -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   function Get_Examples_Number (Lesson : APQ_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
   procedure Get_Referents
             (  Lesson : APQ_Lecture_Object;
                List   : in out Deposit_Container'Class
             );
--
-- Is_Defined -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   function Is_Defined
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Read
--
   function Is_Known
            (  Lesson  : APQ_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Put -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Write
--
   procedure Put
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Fuzzy.Set
             );
--
-- Put -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Write
--
   procedure Put
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
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
-- Set_Undefined -- Overrides Fuzzy.Lecture...
--
-- Effects :
--
--    Seize_Write
--
   procedure Set_Undefined
             (  Lesson  : in out APQ_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   procedure Set_Undefined
             (  Lesson  : in out APQ_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Store -- Overrides Object.Archived...
--
   procedure Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : APQ_Lecture_Object
             );

end Persistent.APQ.Lectures;
