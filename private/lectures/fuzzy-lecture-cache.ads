--                                                                    --
--  package Fuzzy.Lecture.Cache     Copyright (c)  Dmitry A. Kazakov  --
--  Interface                                      Luebeck            --
--                                                 Winter, 2010       --
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
--  This package defines the type Connotated_Lecture_Object, a base type
--  for  a  caching  fuzzy  training  set  type, which may hold training
--  examples of any type. The implementation is write-back. Only a fixed
--  part  of  the  reference  set  is  memory  allocated.  The base type
--  implements the operations:
--
--  (o)  Get
--  (o)  Get_Feature
--  (o)  Get_Features
--  (o)  Get_Features_Number
--  (o)  Get_Referents
--  (o)  Is_Defined
--  (o)  Is_Known
--  (o)  Is_Modified
--  (o)  Reset_Modified
--  (o)  Put
--  (o)  Set_Defined
--
--  The following operations need to be implemented:
--
--  (o)  Raw_Get
--  (o)  Raw_Is_Defined
--  (o)  Raw_Known
--  (o)  Raw_Put
--  (o)  Raw_Set_Defined
--
--  The operations Read and  Write  migth  be  overridden  in  order  to
--  support bulk I/O.
--
with Fuzzy.Lecture.Connotated;       use Fuzzy.Lecture.Connotated;
with Fuzzy.Lecture.Handle;           use Fuzzy.Lecture.Handle;
with Fuzzy.Lecture.Memory_Resident;  use Fuzzy.Lecture.Memory_Resident;

with Ada.Finalization;
with Generic_Unbounded_Array;

package Fuzzy.Lecture.Cache is
   pragma Elaborate_Body (Fuzzy.Lecture.Cache);
--
-- Default_Cache_Size -- The cache size used  by default  in the derived
--                       types
--
   Default_Cache_Size : Positive := 124;

   type Cache_Index is new Integer;
--
-- Lecture_Cache -- Cache for a training set
--
--    Size - The cache size, the number of examples
--
   type Caching_Lecture_Object (Size : Cache_Index) is
      abstract new Connotated_Lecture_Object with private;
--
-- Cache_Get_Examples_Number -- Low-level cache write
--
--    Lesson - The caching training set
--
-- This function  should  be  used  as  a  part  of  Get_Examples_Number
-- implementation. The reference set may have examples which are in  the
-- cache,   not   yet   written.   So  the  number  to  be  returned  by
-- Get_Examples_Number  is  the maximum of the number of examples in the
-- reference set and in the cache.
--
-- Returns :
--
--    The number of examples read or written through the cache
--
   function Cache_Get_Examples_Number
            (  Lesson : Caching_Lecture_Object'Class
            )  return Natural;
--
-- Cache_Put -- Low-level cache write
--
--    Lesson  - The caching training set
--    Index   - Of the cache item
--    Feature - The feature object or a handle to
--    Image   - Of the example
--    Value   - The domain point to set
--
-- This   procedure   writes   a   cache  item.  It  can  be  used  from
-- implemntations of bulk operations.
--
   procedure Cache_Put
             (  Lesson  : in out Caching_Lecture_Object'Class;
                Index   : Cache_Index;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             );
--
-- Cache_Set_Undefined -- Low-level cache write
--
--    Lesson  - The caching training set
--    Index   - Of the cache item
--    Feature - The feature object or a handle to
--    Image   - Of the example
--    Value   - The domain point to set
--
-- This   procedure   writes   a   cache  item.  It  can  be  used  from
-- implemntations of bulk operations.
--
   procedure Cache_Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object'Class;
                Index   : Cache_Index;
                Feature : Feature_Object'Class;
                Image   : Image_Type
             );
--
-- Drop -- Drop dirty cache
--
--    Lesson - The caching training set
--
-- This  procedure  erases  dirty  cache.  All  not yet written data are
-- discarded.
--
   procedure Drop (Lesson : in out Caching_Lecture_Object);
--
-- Finalize -- Destruction
--
--    Lesson - The caching training set
--
-- This procedure is called when overriden. Note that  Program_Error  is
-- propagated when finalized cache is dirty.
--
   overriding
   procedure Finalize (Lesson : in out Caching_Lecture_Object);
--
-- Initialize -- Destruction
--
--    Lesson - The caching training set
--
-- This procedure is called when overriden.
--
   overriding
   procedure Initialize (Lesson : in out Caching_Lecture_Object);
--
-- Get -- Overriding Fuzzy.Lecture...
--
   overriding
   function Get
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set;
--
-- Is_Defined -- Overriding Fuzzy.Lecture...
--
   overriding
   function Is_Defined
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Is_Known -- Overriding Fuzzy.Lecture...
--
   overriding
   function Is_Known
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean;
--
-- Put -- Overriding Fuzzy.Lecture...
--
   overriding
   procedure Put
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             );
   overriding
   procedure Put
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             );
--
-- Raw_Get -- Non-cached implementation
--
--    Lesson  - The caching training set
--    Example - The training example
--    Feature - The feature object or a handle to
--    Image   - Of the example
--
-- Returns :
--
--    The image
--
   function Raw_Get
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is abstract;
--
-- Raw_Is_Defined -- Non-cached implementation
--
--    Lesson  - The caching training set
--    Example - The training example
--    Feature - The feature object or a handle to
--    Image   - Of the example
--
-- Returns :
--
--    True
--
   function Raw_Is_Defined
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is abstract;
--
-- Raw_Put -- Non-cached implementation
--
--    Lesson  - The caching training set
--    Example - The training example
--    Feature - The feature object or a handle to
--    Image   - Of the example
--    Value   - The domain point to set
--
   procedure Raw_Put
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is abstract;
--
-- Raw_Set_Undefined -- Non-cached implementation
--
--    Lesson    - The caching training set
--    Feature   - The feature object or a handle to
--
   procedure Raw_Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object;
                Feature : Feature_Object'Class
             )  is abstract;
--
-- Read -- The training set into the cache
--
--    Lesson  - The caching training set
--    Example - The training example
--  [ Feature - The feature object
--    Image ] - Of the example
--    Index   - Identifies the cache entry
--
-- This procedure reads training examples into  the  cache.  It  can  be
-- overriden  to  provide  read-ahead policy. The default is the variant
-- with the feature parameter. When overridden it may  call  start  bulk
-- I/O and then call to the second form instead.
--
   procedure Read
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Index   : Cache_Index
             );
   procedure Read
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             );
--
-- Set_Undefined -- Overriding Fuzzy.Lecture...
--
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             );
   overriding
   procedure Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object;
                Feature : Feature_Object'Class
             );
--
-- Write -- The dirty cache into the training set
--
--    Lesson - The caching training set
--
-- This  procedure  writes  all dirty data back to the reference set. It
-- can be overridden and wrapped to provide bulk  write  mode  important
-- when the reference training set is backed by a DB engine.
--
   procedure Write (Lesson : in out Caching_Lecture_Object);

private
   type Cached_Example;
   type Cached_Example_Ptr is access all Cached_Example;
   type Cache_Index_Array is array (Positive range <>) of Cache_Index;
   package Example_To_Cache_Index_Mapping is
      new Generic_Unbounded_Array
          (  Index_Type        => Positive,
             Object_Type       => Cache_Index,
             Object_Array_Type => Cache_Index_Array,
             Null_Element      => 0
          );
   use Example_To_Cache_Index_Mapping;
   type Cached_Example is record
      Previous : Cache_Index;
      Next     : Cache_Index;
      Example  : Integer; -- Negative when dirty
   end record;
   type Tags is array (Cache_Index range <>) of Cached_Example;
--
-- Lecture_Cache -- Implementation
--
   type Caching_Lecture_Object (Size : Cache_Index) is
      abstract new Connotated_Lecture_Object with
   record
      Self  : not null access Caching_Lecture_Object'Class :=
                        Caching_Lecture_Object'Unchecked_Access;
      Cache : aliased General_Lecture_Object;
      Rows  : Natural := 0;
      Index : Unbounded_Array;
      Flags : Tags (-2..Size);
   end record;
--
-- Allocate -- Example in the cache
--
--    Lesson  - The caching training set
--    Example - The example number
--    List    - Where to put new item
--
-- Returns :
--
--    Index of the allocated item
--
   function Allocate
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               List    : Cache_Index
            )  return Cache_Index;
--
-- Get_Index - Check if the example in the cache
--
--    Lesson  - The caching training set
--    Example - The example number
--
-- Returns :
--
--    Index of the allocated item or 0
--
   function Get_Index
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive
            )  return Cache_Index;

   procedure Move
             (  Flags : in out Tags;
                Item  : Cache_Index;
                After : Cache_Index
             );

   pragma Inline (Allocate);
   pragma Inline (Cache_Put);
   pragma Inline (Cache_Set_Undefined);
   pragma Inline (Get_Index);
   pragma Inline (Move);

end Fuzzy.Lecture.Cache;
