--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.Connotated                    Luebeck            --
--  Interface                                      Winter, 2002       --
--                                                                    --
--                                Last revision :  14:48 30 May 2014  --
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
--
--  This package defines the type Connotated_Lecture_Object, an abstract
--  fuzzy  lecture  type  mantaining  the  list  of features used in the
--  lecture. It  provides  a  mapping  Feature_ID->Feature_Index,  where
--  Feature_Index is the dense column index of the feature [when the set
--  is  considered  as  a  table: example x feature]. Note that features
--  used  in the training set are referenced and thus they are protected
--  from  the garbage collector. The base type implements the operations
--  related to querying features:
--
--  (o)  Get_Feature
--  (o)  Get_Features
--  (o)  Get_Features_Number
--  (o)  Get_Referents
--  (o)  Is_Modified
--  (o)  Reset_Modified
--
--  It  provides  the  operation  Delete_Feature  to  be  used  in   the
--  implementation of  Set_Undefined.  Get_Feature  is  used  to  obtain
--  feature index and add a new column for this feature if necessary. It
--  also defines an abstract operation Add_Feature  called  when  a  new
--  column is added to the set.
--
with Object.Archived.Features;  use Object.Archived.Features;

with Generic_Unbounded_Array;
with Object.Handle.Generic_Unbounded_Array;

package Fuzzy.Lecture.Connotated is
   pragma Elaborate_Body (Fuzzy.Lecture.Connotated);
--
-- Connotated_Lecture_Object -- The training set object
--
   type Connotated_Lecture_Object;
--
-- Feature_Index -- The dense feature index (column)
--
   type Feature_Index is new Natural;
   type ID_To_Index_Array is
      array (Fuzzy.Feature.Feature_ID range <>) of Feature_Index;
--
-- Mapping from Fuzzy.Feature.Feature_ID to Feature_Index
--
   package ID_To_Index_Map is
      new Generic_Unbounded_Array
          (  Index_Type        => Fuzzy.Feature.Feature_ID,
             Object_Type       => Feature_Index,
             Object_Array_Type => ID_To_Index_Array,
             Null_Element      => 0
          );
--
-- Feature_Descriptor -- Reference to a feature in use
--
--    Lesson  - The training set
--    Feature - A handle to
--
-- The type can be inherited from to add  additional  fields  associated
-- with  a  feature used in the training set. The reference is a forward
-- and backward link.
--
   type Feature_Descriptor
        (  Lesson : not null access Connotated_Lecture_Object'Class
        )  is new Feature_Link with
   record
      Feature : Feature_Handle;
   end record;
   type Feature_Descriptor_Ptr is access Feature_Descriptor'Class;
   for Feature_Descriptor_Ptr'Storage_Pool
      use Backward_Link_Ptr'Storage_Pool;
--
-- Deleted -- Overrides Object.Archived...
--
-- The  implementation  calls  Set_Undefined on the feature bound to the
-- notice. Finally, Dispose is set to True.
--
   overriding
   procedure Deleted
             (  Item  : in out Feature_Descriptor;
                Temps : in out Deposit_Container'Class
             );
--
-- Destroyed -- Overrides Object.Archived...
--
   overriding
   procedure Destroyed (Item : in out Feature_Descriptor);
--
-- Renamed -- Overrides Object.Archived.Features...
--
   overriding
   procedure Renamed
             (  Link     : in out Feature_Descriptor;
                Old_Name : String;
                New_Name : String
             );
--
-- Feature_Descriptor_Handles -- Handles to feature descriptors
--
   package Feature_Descriptor_Handles is
      new Object.Handle (Feature_Descriptor, Feature_Descriptor_Ptr);
--
-- Mapping from Feature_Index to Fuzzy.Feature.Feature_ID
--
   package Index_To_Feature_Map is
      new Feature_Descriptor_Handles.Generic_Unbounded_Array
          (  Index_Type  => Feature_Index,
             Handle_Type => Feature_Descriptor_Handles.Handle
          );
   use Feature_Descriptor_Handles;
--
-- Connotated_Lecture_Object -- A training set with features list
--
--    Columns    - Number of features in use
--    Updated    - Modification flag
--    To_Index   - ID to column mapping
--    To_Feature - Column to feature descriptor mapping
--
   type Connotated_Lecture_Object is
      abstract new Lecture_Object with
   record
      Columns : Feature_Index := 0;
      Updated : Boolean       := False;
      --
      -- Maps: ID->Index; Index->Feature
      --
      To_Index   : ID_To_Index_Map.Unbounded_Array;
      To_Feature : Index_To_Feature_Map.Unbounded_Array;
   end record;
--
-- Add_Feature -- Adding a new column to the set
--
--    Lesson  - The training set to add the feature
--    Feature - The feature (a handle to)
--    Image   - The feature image
--
-- This function is called to allocate a descriptor for a  new  feature.
-- The feature index is Lesson.Columns + 1. The caller modifies the maps
-- and  finally  increases Lesson.Columns by one. An implementation need
-- not  to  set the field Handle. It will be done automatically, as well
-- as  inserting the result into the corresponding map. It also need not
-- to set Lesson.Modified, it is caller's responsibility.
--
-- Returns :
--
--    Handle to the feature descriptor
--
-- Exceptions :
--
--    Constraint_Error - Invalid handle to the feature
--    others           - Will propagate out of the caller (Get_Feature)
--
   function Add_Feature
            (  Lesson  : not null access Connotated_Lecture_Object;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Descriptor_Handles.Handle is abstract;
--
-- Delete_Feature -- Removes a feature from the maps
--
--    Lesson  - The training set object
--    Feature - The index of
--
-- This procedure has to be called by an implementation of Set_Undefined
-- which deletes a feature (a column of the  set).  The  result  of  the
-- operation is as follows:
--
-- Exceptions :
--
--    Constraint_Error - Illegal index (Feature)
--
-- Notes :
--
--    There is no need to call Delete_Feature from Finalize
--
   procedure Delete_Feature
             (  Lesson  : in out Connotated_Lecture_Object'Class;
                Feature : Feature_Index
             );
--
-- Finalize -- Destruction
--
--    Lesson - The training set object
--
   overriding
   procedure Finalize (Lesson : in out Connotated_Lecture_Object);
--
-- Get_Feature -- Overrides Fuzzy.Lecture...
--
   function Get_Feature
            (  Lesson : Connotated_Lecture_Object;
               Index  : Positive
            )  return Feature_Handle;
--
-- Get_Feature -- Get feature dense index
--
--    Lesson  - The training set to get the feature of
--    Feature - The feature (a handle to)
--    Image   - The feature image
--
-- This function requests the index of a feature. If the feature is  not
-- used in  Lesson  it  is  registered  there.  It  has  the  effect  of
-- increasing  Lesson.Columns  by  one  and  returning it as the result.
-- Add_Feature is called in this case.
--
-- Returns :
--
--    The index of the feature in Lesson
--
-- Exceptions :
--
--    Propagated out of Add_Feature
--
-- Effects :
--
--    The modification flag is set if the feature is new for the set
--
   function Get_Feature
            (  Lesson  : not null access
                         Connotated_Lecture_Object'Class;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Feature_Index;
--
-- Get_Features -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features (Lesson : Connotated_Lecture_Object)
      return Bounded_Array;
--
-- Get_Features_Number -- Overrides Fuzzy.Lecture...
--
   overriding
   function Get_Features_Number (Lesson : Connotated_Lecture_Object)
      return Natural;
--
-- Get_Referents -- Overrides Object.Archived...
--
-- The dependencies list contains all the features used in the  training
-- set examples.
--
   overriding
   procedure Get_Referents
             (  Lecture : Connotated_Lecture_Object;
                List    : in out Deposit_Container'Class
             );
--
-- Is_Modified -- Overrides Object.Archived...
--
   overriding
   function Is_Modified (Lesson : Connotated_Lecture_Object)
      return Boolean;
--
-- Reset_Modified -- Overrides Object.Archived...
--
   overriding
   procedure Reset_Modified (Lesson : in out Connotated_Lecture_Object);

private
   pragma Inline (Get_Feature);
   pragma Inline (Get_Features_Number);
   pragma Inline (Is_Modified);
   pragma Inline (Reset_Modified);

end Fuzzy.Lecture.Connotated;
