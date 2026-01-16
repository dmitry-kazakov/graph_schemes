--                                                                    --
--  package Fuzzy.Lecture.Cache     Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Winter, 2010       --
--                                                                    --
--                                Last revision :  10:08 22 Nov 2014  --
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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.IO_Exceptions;    use Ada.IO_Exceptions;
with Fuzzy.Lecture.Block;  use Fuzzy.Lecture.Block;

package body Fuzzy.Lecture.Cache is

   Clean  : constant := -2;
   Dirty  : constant := -1;
   Unused : constant :=  0;

--   procedure Put (Flags : Tags; Item : Cache_Index) is
--      This : Cached_Example := Flags (Item);
--   begin
--      Ada.Text_IO.Put (Cache_Index'Image (This.Previous) & " <-[");
--      Ada.Text_IO.Put (Cache_Index'Image (Item) & " ]->");
--      Ada.Text_IO.Put (Cache_Index'Image (This.Next) & "  ");
--      Ada.Text_IO.New_Line;
--   end Put;

   function Allocate
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               List    : Cache_Index
            )  return Cache_Index is
      Result : Cache_Index;
   begin
      if Lesson.Flags (Unused).Next /= Unused then
         -- Take the first unused cache item
         Result := Lesson.Flags (Unused).Next;
      elsif Lesson.Flags (Clean).Previous /= Clean then
         -- Take the last clean cache item
         Result := Lesson.Flags (Clean).Previous;
      else
         -- Flush dirty cache and take the last clean cache item then
         Write (Lesson.Self.all);
         Result := Lesson.Flags (Clean).Previous;
      end if;
      Move (Lesson.Self.Flags, Result, List);
      Put (Lesson.Self.Index, Example, Result);
      Lesson.Self.Flags (Result).Example := Example;
      return Result;
   end Allocate;

   function Cache_Get_Examples_Number
            (  Lesson : Caching_Lecture_Object'Class
            )  return Natural is
   begin
      return Lesson.Rows;
   end Cache_Get_Examples_Number;

   procedure Cache_Put
             (  Lesson  : in out Caching_Lecture_Object'Class;
                Index   : Cache_Index;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
   begin
      Put (Lesson.Cache, Positive (Index), Feature, Image, Value);
   end Cache_Put;

   procedure Cache_Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object'Class;
                Index   : Cache_Index;
                Feature : Feature_Object'Class;
                Image   : Image_Type
             )  is
   begin
      Set_Undefined
      (  Lesson.Cache,
         Positive (Index),
         Feature,
         Image,
         Undefined
      );
   end Cache_Set_Undefined;

   procedure Drop (Lesson : in out Caching_Lecture_Object) is
   begin
      loop
         declare
            Index : constant Cache_Index := Lesson.Flags (Dirty).Next;
         begin
            exit when Index = Dirty;
            Put (Lesson.Index, abs Lesson.Flags (Index).Example, 0);
            Move (Lesson.Flags, Index, Unused);
         end;
      end loop;
   end Drop;

   procedure Finalize (Lesson : in out Caching_Lecture_Object) is
   begin
      if Lesson.Flags (Dirty).Previous /= Dirty then
         -- The list of examples in the cache is not empty
         Raise_Exception
         (  Program_Error'Identity,
            "The cache is dirty when finalized"
         );
      end if;
      Finalize (Connotated_Lecture_Object (Lesson));
   end Finalize;

   function Get
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      Lesson.Self.Rows := Natural'Max (Lesson.Self.Rows, Example);
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Clean);
         Set_Undefined (Lesson.Self.Cache, Positive (Index), Uncertain);
         Read (Lesson.Self.all, Example, Feature, Image, Index);
         return Get (Lesson.Cache, Positive (Index), Feature, Image);
      end if;
      if Lesson.Flags (Index).Example > 0 then
         -- Move in front of the clean cache
         Move (Lesson.Self.Flags, Index, Clean);
      else
         -- Move in front of the dirty cache
         Move (Lesson.Self.Flags, Index, Dirty);
      end if;
      if Is_Defined (Lesson.Cache, Positive (Index), Feature, Image) =
         Uncertain
      then -- Cache the value
         Read (Lesson.Self.all, Example, Feature, Image, Index);
      end if;
      return Get (Lesson.Cache, Positive (Index), Feature, Image);
   end Get;

   function Get_Index
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive
            )  return Cache_Index is
   begin
      return Fetch (Lesson.Index, Example);
   end Get_Index;

   procedure Initialize (Lesson : in out Caching_Lecture_Object) is
   begin
      declare
         This : Cached_Example renames Lesson.Flags (Dirty);
      begin
         This.Previous := Dirty;
         This.Next     := Dirty;
      end;
      declare
         This : Cached_Example renames Lesson.Flags (Clean);
      begin
         This.Previous := Clean;
         This.Next     := Clean;
      end;
      for Index in 0..Lesson.Flags'Last loop
         declare
            This : Cached_Example renames Lesson.Flags (Index);
         begin
            This.Previous := Index - 1;
            This.Next     := Index + 1;
         end;
      end loop;
      Lesson.Flags (Lesson.Flags'Last).Next := Unused;
      Lesson.Flags (Unused).Previous := Lesson.Flags'Last;
      Initialize (Connotated_Lecture_Object (Lesson));
   end Initialize;

   function Is_Defined
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      Lesson.Self.Rows := Natural'Max (Lesson.Self.Rows, Example);
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Clean);
         Set_Undefined (Lesson.Self.Cache, Positive (Index), Uncertain);
         Read (Lesson.Self.all, Example, Feature, Image, Index);
         return
            Is_Defined (Lesson.Cache, Positive (Index), Feature, Image);
      end if;
      if Lesson.Flags (Index).Example > 0 then
         -- Move in front of the clean cache
         Move (Lesson.Self.Flags, Index, Clean);
      else
         -- Move in front of the dirty cache
         Move (Lesson.Self.Flags, Index, Dirty);
      end if;
      if Is_Defined (Lesson.Cache, Positive (Index), Feature, Image) =
         Uncertain
      then -- Cache the value
         Read (Lesson.Self.all, Example, Feature, Image, Index);
      end if;
      return
         Is_Defined (Lesson.Cache, Positive (Index), Feature, Image);
   end Is_Defined;

   function Is_Known
            (  Lesson  : Caching_Lecture_Object;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      Lesson.Self.Rows := Natural'Max (Lesson.Self.Rows, Example);
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Clean);
         Set_Undefined (Lesson.Self.Cache, Positive (Index), Uncertain);
         Read (Lesson.Self.all, Example, Feature, Image, Index);
         return
            Is_Known (Lesson.Cache, Positive (Index), Feature, Image);
      end if;
      if Lesson.Flags (Index).Example > 0 then
         -- Move in front of the clean cache
         Move (Lesson.Self.Flags, Index, Clean);
      else
         -- Move in front of the dirty cache
         Move (Lesson.Self.Flags, Index, Dirty);
      end if;
      if Is_Defined (Lesson.Cache, Positive (Index), Feature, Image) =
         Uncertain
      then -- Cache the value
         Read (Lesson.Self.all, Example, Feature, Image, Index);
      end if;
      return Is_Known (Lesson.Cache, Positive (Index), Feature, Image);
   end Is_Known;

   procedure Move
             (  Flags : in out Tags;
                Item  : Cache_Index;
                After : Cache_Index
             )  is
      This : Cached_Example renames Flags (Item);
   begin
      declare
         Next     : constant Cache_Index := This.Next;
         Previous : constant Cache_Index := This.Previous;
      begin
         Flags (Previous).Next := Next;
         Flags (Next).Previous := Previous;
      end;
      declare
         Before : constant Cache_Index := Flags (After).Next;
      begin
         This.Next     := Before;
         This.Previous := After;
         Flags (After).Next      := Item;
         Flags (Before).Previous := Item;
      end;
   end Move;

   procedure Put
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Set
             )  is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      Lesson.Rows := Natural'Max (Lesson.Rows, Example);
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Dirty);
         Set_Undefined (Lesson.Cache, Positive (Index), Uncertain);
      else
         -- The example is allocated in the cache
         Move (Lesson.Flags, Index, Dirty);
      end if;
      Lesson.Flags (Index).Example := -Example;
      Put (Lesson.Cache, Positive (Index), Feature, Image, Value);
   end Put;

   procedure Put
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Value   : Positive
             )  is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      Lesson.Rows := Natural'Max (Lesson.Rows, Example);
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Dirty);
         Set_Undefined (Lesson.Cache, Positive (Index), Uncertain);
      else
         -- The example is allocated in the cache
         Move (Lesson.Flags, Index, Dirty);
      end if;
      Lesson.Flags (Index).Example := -Example;
      Put (Lesson.Cache, Positive (Index), Feature, Image, Value);
   end Put;

   procedure Read
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type;
                Index   : Cache_Index
             )  is
   begin
      if Raw_Is_Defined (Lesson.Self.all, Example, Feature, Image) then
         Put
         (  Lesson.Cache,
            Positive (Index),
            Feature,
            Image,
            Raw_Get (Lesson.Self.all, Example, Feature, Image)
         );
      else
         Set_Undefined
         (  Lesson.Cache,
            Positive (Index),
            Feature,
            Image,
            Undefined
         );
      end if;
   end Read;

   procedure Read
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Index   : Cache_Index
             )  is
      No : constant Positive := Positive (Index);
   begin
      for Column in 1..Get_Features_Number (Lesson.Cache) loop
         declare
            Feature : Feature_Object'Class renames
                      Ptr (Get_Feature (Lesson.Cache, Column)).all;
         begin
            for Image in Image_Type'Range loop
               if Raw_Is_Defined
                  (  Lesson.Self.all,
                     Example,
                     Feature,
                     Image
                  )
               then
                  Put
                  (  Lesson.Cache,
                     No,
                     Feature,
                     Image,
                     Raw_Get (Lesson.Self.all, Example, Feature, Image)
                  );
               else
                  Set_Undefined
                  (  Lesson.Cache,
                     No,
                     Feature,
                     Image,
                     Undefined
                  );
               end if;
            end loop;
         end;
      end loop;
   end Read;

   procedure Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
      Index : Cache_Index := Get_Index (Lesson, Example);
   begin
      if Index <= 0 then
         -- Not in the cache, bring it there
         Index := Allocate (Lesson, Example, Dirty);
         Set_Undefined (Lesson.Cache, Positive (Index), Uncertain);
      else
         -- The example is allocated in the cache
         Move (Lesson.Flags, Index, Dirty);
      end if;
      Lesson.Flags (Index).Example := -Example;
      Set_Undefined
      (  Lesson.Cache,
         Positive (Index),
         Feature,
         Undefined
      );
   end Set_Undefined;

   procedure Set_Undefined
             (  Lesson  : in out Caching_Lecture_Object;
                Feature : Feature_Object'Class
             )  is
   begin
      Set_Undefined (Lesson.Cache, Feature);
      Write (Lesson.Self.all);
      Raw_Set_Undefined (Lesson.Self.all, Feature);
   end Set_Undefined;

   procedure Write (Lesson : in out Caching_Lecture_Object) is
   begin
      if Lesson.Flags (Dirty).Next = Dirty then
         return;
      end if;
      for Column in 1..Get_Features_Number (Lesson.Cache) loop
         declare
            Index   : Cache_Index := Lesson.Flags (Dirty).Next;
            Example : Positive;
            No      : Positive;
            Feature : Feature_Object'Class renames
                      Ptr (Get_Feature (Lesson.Cache, Column)).all;
         begin
            loop
               Example := abs Lesson.Flags (Index).Example;
               No      := Positive (Index);
               for Image in Image_Type loop
                  if Is_Defined (Lesson.Cache, No, Feature, Image) then
                     Raw_Put
                     (  Lesson.Self.all,
                        Example,
                        Feature,
                        Image,
                        Get (Lesson.Cache'Access, No, Feature, Image)
                     );
                  end if;
               end loop;
               Index := Lesson.Flags (Index).Next;
               exit when Index = Dirty;
            end loop;
         end;
      end loop;
      declare
         This : Cache_Index := Lesson.Flags (Dirty).Next;
         Next : Cache_Index;
      begin
         loop
            declare
               Current : Cached_Example renames Lesson.Flags (This);
            begin
               Current.Example := abs Current.Example;
               Next := Current.Next;
               Move (Lesson.Flags, This, Clean);
               exit when Next = Dirty;
               This := Next;
            end;
         end loop;
      end;
   end Write;

end Fuzzy.Lecture.Cache;
