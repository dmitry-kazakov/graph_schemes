--                                                                    --
--  package Fuzzy.Persistence       Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2004       --
--                                                                    --
--                                Last revision :  12:32 10 Jun 2003  --
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

with Fuzzy.Classifier.Handle;  use Fuzzy.Classifier.Handle;
with Fuzzy.Feature.Handle;     use Fuzzy.Feature.Handle;
with Fuzzy.Graph.Handle;       use Fuzzy.Graph.Handle;
with Fuzzy.Lecture.Handle;     use Fuzzy.Lecture.Handle;
with Persistent;               use Persistent;
with Persistent.Handle;        use Persistent.Handle;

package Fuzzy.Persistence is
   pragma Elaborate_Body (Fuzzy.Persistence);
--
-- Get -- Get a persistent object by name
--
--    Storage - To get object from (a handle to)
--    Name    - Name of the object
--
-- This  function  searches for the specified object by either its name.
-- If  the  object  is  already  available  a  handle to it is returned.
-- Otherwise it first is restored from the persistent storage.
--
-- Returns :
--
--    A handle to the object
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle. parent, wrong object type
--    Data_Error       - Inconsistent storage
--    End_Error        - No such object
--    Use_Error        - Object's class is unknown
--
   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Classifier_Handle;
   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Feature_Handle;
   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Lecture_Handle;
   function Get
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Node_Handle;
--
-- Get_Name -- Of a persistent object
--
--    Storage - A handle to
--    Object  - A handle to
--
-- Returns :
--
--    The object name in Storage (UTF-8 encoded)
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle or not a persistent object
--    Data_Error       - Inconsistent storage
--    Name_Error       - Object is anonymous
--
   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Classifier_Handle
            )  return String;
   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Feature_Handle
            )  return String;
   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Lecture_Handle
            )  return String;
   function Get_Name
            (  Storage : Storage_Handle;
               Object  : Node_Handle
            )  return String;
--
-- Is_In -- Check if an object persists in the storage
--
--    Storage - A handle to
--    Object  - A handle to an object
--
-- This function checks whether Object persists in Storage. If Object is
-- not a valid handle, the result is False.
--
-- Returns :
--
--    True if Object persists in Storage
--
-- Exceptions :
--
--    Data_Error - Inconsistent storage
--
   function Is_In
            (  Storage : Storage_Handle;
               Object  : Classifier_Handle
            )  return Boolean;
   function Is_In
            (  Storage : Storage_Handle;
               Object  : Feature_Handle
            )  return Boolean;
   function Is_In
            (  Storage : Storage_Handle;
               Object  : Lecture_Handle
            )  return Boolean;
   function Is_In
            (  Storage : Storage_Handle;
               Object  : Node_Handle
            )  return Boolean;
--
-- Put -- Make an object persistent
--
--    Storage  - A handle to
--    Object   - A handle to the object to store in Storage
--  [ Name   ] - The name of the object being stored
--  [ Parent ] - The parent of the object
--
-- This procedure is used to store Object in Storage. The parameter Name
-- specifies the object name there. When omitted the  object  is  stored
-- anonymous. Anonymous persistent objects are collected  when  no  more
-- used. When Object already persists in Storage and Name is  specified,
-- then  it  is  checked  that  it is same. If this check fails, Name is
-- illegal,  or  conflicts with the name of another object Name_Error is
-- propagated. When name is not specified no check is made.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, Parent is not persistent
--    Data_Error       - Inconsistent storage
--    Name_Error       - Name conflict
--
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle;
                Name    : String;
                Parent  : Deposit_Handle := Root_Directory
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle
             );
   procedure Put
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle
             );
--
-- Rename -- A persistent objects
--
--    Storage      - A handle to
--    Object       - The object to be renamed (a handle to)
--    New_Name     - The new object name
--  [ New_Parent ] - New parent of the object
--
-- This procedure changes the name of the object specified by either its
-- old name or by a handle to. When renamed object was anonymous  before
-- renaming it becomes a named one. When Object is an invalid handle  or
-- does   not   refer  a  persistent  object  then  Constraint_Error  is
-- propagated. Name_Error indicates an illegal or conflicting name.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, new parent is not persistent
--    Data_Error       - Inconsistent storage
--    Name_Error       - Name conflict, there is another object named so
--
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Classifier_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Feature_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Lecture_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             );
   procedure Rename
             (  Storage    : in out Storage_Handle;
                Object     : in out Node_Handle;
                New_Name   : String;
                New_Parent : Deposit_Handle
             );
   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Classifier_Handle;
                New_Name : String
             );
   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Feature_Handle;
                New_Name : String
             );
   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Lecture_Handle;
                New_Name : String
             );
   procedure Rename
             (  Storage  : in out Storage_Handle;
                Object   : in out Node_Handle;
                New_Name : String
             );
--
-- Unname -- A persistent objects
--
--    Storage - A handle to
--    Object  - The object to be unnamed (a handle to)
--
-- This procedure  makes  the  object  anonymous.  Unnamed  objects  are
-- automatically  deleted  when  no  more in use. Nothing happens if the
-- object  is already unnamed or does not persist in Storage.  Note that
-- anonymous  objects  are  not  deleted  as  long  as  they have memory
-- resident  counterparts.  Note  difference  between  Unname and Delete
-- called on an object handle. Delete requests object deletion from both
-- memory  and persistent storage. Unname does it for persistent storage
-- only.  Both  may  have  no immediate effect if the object is still in
-- use.
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--    Data_Error       - Inconsistent storage
--
   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Classifier_Handle
             );
   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Feature_Handle
             );
   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Lecture_Handle
             );
   procedure Unname
             (  Storage : in out Storage_Handle;
                Object  : in out Node_Handle
             );
private
   pragma Inline (Get);
   pragma Inline (Get_Name);
   pragma Inline (Is_In);
   pragma Inline (Put);
   pragma Inline (Rename);
   pragma Inline (Unname);

end Fuzzy.Persistence;
