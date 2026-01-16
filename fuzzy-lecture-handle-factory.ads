--                                                                    --
--  function                        Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Handle.Factory                Luebeck            --
--  Interface                                      Winter, 2002       --
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

with Persistent;         use Persistent;
with Persistent.Handle;  use Persistent.Handle;

with Fuzzy.Lecture.Composite;
with Fuzzy.Lecture.General;
with Fuzzy.Lecture.General.Text_Dialogue;
with Fuzzy.Lecture.Subrange;

package Fuzzy.Lecture.Handle.Factory is
--
-- Create_Composite -- A training set composed of reference sets
--
--    Mapping - The list or set of features of the result set
--
-- The training set contains the examples from the sets specified in the
-- Mapping.  The parameter Mapping is an array of feature - training set
-- pairs. For each feature in the array the result set contains examples
-- from the corresponding set for this feature.
--
-- Returns :
--
--    Handle to the set
--
-- Exceptions :
--
--    Constraint_Error - Invalid lecture handle
--    Data_Error       - Not unique features
--

--
   function Create_Composite
            (  Mapping : Fuzzy.Lecture.Composite.Feature_To_Lecture_Map
            )  return Lecture_Handle;
--
-- Create_Dialogue -- Create a new dialogue fuzzy training set
--
-- This function creates a new training set like  Create_Memory_Resident
-- does.  The  difference  is that undefined examples are requested from
-- the user using text I/O.
--
-- Returns :
--
--    Handle of the newly created fuzzy training set
--
   function Create_Dialogue return Lecture_Handle;
--
-- Create_Memory_Resident -- Create a new memory-resident training set
--
-- This function creates a new training set and returns a handle to  it.
-- The  newly  created set is empty. The whole set of examples is memory
-- allocated.   The   implementation   can   get  an  advantage  if  the
-- realizations of some features are singletons.
--
-- Returns :
--
--    Handle of the newly created fuzzy training set
--
   function Create_Memory_Resident return Lecture_Handle;
--
-- Create_Persistent -- Create a data base resident fuzzy training set
--
--    Storage  - A handle to
--  [ Name     - The name of the training being stored
--    Parent ] - The parent object
--
-- This function creates a new training set resident  in  Storage.  Note
-- that though  memory  resident  training  sets  can  be  stored  in  a
-- persistent storage as any other object, that could be done only as  a
-- whole. It might be very inefficient to store large sets in this  way.
-- Therefore persistent storage types  also  provide  natively  resident
-- training sets. Such sets are not loaded/stored as a whole but provide
-- I/O operations  for  individual  training  examples.  So  they  might
-- support  sets  larger  than  available  memory.  However   persistent
-- training sets could be considerably slower than the  memory  resident
-- ones.  The  parameter  Name  specifies  the  object  name there. When
-- omitted the object is stored anonymous. Anonymous persistent  objects
-- are collected when no more used.  When  Name  is  empty,  illegal  or
-- conflicts with the name of another immediate child object of  Parent,
-- then  Name_Error  is propagated. It is also propagated when Parent is
-- anonymous.  When  name  is  not   specified   no   check   is   made.
-- Constraint_Error  is  propagated  when  Parent  is legal but does not
-- indicate a persistent in Storage object.
--
-- Returns :
--
--    Handle of the newly created fuzzy training set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle, non-persistent Parent
--    Data_Error       - A persistent storage error
--    Name_Error       - Name conflict, illegal name, anonymous parent
--    Use_Error        - Storage does not support training sets
--
   function Create_Persistent
            (  Storage : Storage_Handle;
               Name    : String;
               Parent  : Deposit_Handle := Root_Directory
            )  return Lecture_Handle;
   function Create_Persistent
            (  Storage : Storage_Handle
            )  return Lecture_Handle;
--
-- Create_Subrange -- A training set, which is a subrange of a reference
--                    set
--
--    Source - The reference set
--    From   - The first example number to include
--    To     - The last example number to include
--
-- The  training  set  contains  the  examples  From..To  of Source. The
-- parameter To may be less than From to indicate empty  range.  It  can
-- also be greater than the total examples number  of  Source.  Updating
-- the  training  set also does the source set. An attempt to modify the
-- Source set outside  the examples range  causes  Use_Error  exception.
-- Examples outside  the  range  when  read  considered  undefined.  The
-- training subrange set are enumerated from 1.
--
-- Returns :
--
--    Handle to the set
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle
--
   function Create_Subrange
            (  Source : Lecture_Handle;
               From   : Positive;
               To     : Natural := Positive'Last
            )  return Lecture_Handle;

private
   function Create_Composite
            (  Mapping : Fuzzy.Lecture.Composite.Feature_To_Lecture_Map
            )  return Lecture_Handle renames
                      Fuzzy.Lecture.Composite.Create;
   function Create_Memory_Resident return Lecture_Handle
      renames Fuzzy.Lecture.General.Create;
   function Create_Dialogue return Lecture_Handle
      renames Fuzzy.Lecture.General.Text_Dialogue.Create;
   function Create_Subrange
            (  Source : Lecture_Handle;
               From   : Positive;
               To     : Natural := Positive'Last
            )  return Lecture_Handle
      renames Fuzzy.Lecture.Subrange.Create;

end Fuzzy.Lecture.Handle.Factory;

