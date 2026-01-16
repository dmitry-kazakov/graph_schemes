--                                                                    --
--  package Fuzzy.Feature.Context   Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2003       --
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

with Ada.Unchecked_Deallocation;

package body Fuzzy.Feature.Context is
   use Feature_Data_Arrays.Object_Ptr_Array;

   procedure Free is
      new Ada.Unchecked_Deallocation
          (  Feature_Data'Class,
             Feature_Data_Ptr
          );

   procedure Finalize (Context : in out Lecture_Context) is
      Ptr : Object.Entity_Ptr := Context.Lesson.all'Unchecked_Access;
   begin
      Object.Release (Ptr);
   end Finalize;

   procedure Check
             (  Context : in out Lecture_Context;
                Example : Positive
             )  is
      pragma Inline (Check);
   begin
      if Example /= Context.Example then
         Context.Example := Example;
         if Context.Data.Vector /= null then
            declare
               Data : Feature_Data_Ptr_Array
                         renames Context.Data.Vector.all;
            begin
               for Item in Data'Range loop
                  if Data (Item) /= null then
                     Undefine (Data (Item).all, Context);
                  end if;
               end loop;
           end;
         end if;
      end if;
   end Check;

   function Get_Data
            (  Context : not null access Lecture_Context;
               Feature : Feature_Object'Class
            )  return Feature_Data_Ptr is
      ID  : constant Feature_ID := Get_ID (Feature);
      Ptr : Feature_Data_Ptr;
   begin
      if (  Context.Data.Vector /= null
         and then
            ID in Context.Data.Vector'Range
         )
      then
         Ptr := Context.Data.Vector (ID);
         if Ptr = null then
            Ptr := Create_Data (Feature);
            Context.Data.Vector (ID) := Ptr;
         end if;
      else
         Ptr := Create_Data (Feature);
         Set_Data (Context.all, Feature, Ptr);
      end if;
      return Ptr;
   end Get_Data;

   function Get
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
   begin
      Check (Context.all, Example);
      return Get (Feature, Context, Image);
   end Get;

   function Get
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence is
   begin
      Check (Context.all, Example);
      return Get (Feature, Context, Image, Value);
   end Get;

   procedure Initialize (Context : in out Lecture_Context) is
   begin
      Fuzzy.Feature.Initialize (Context_Object (Context));
      Increment_Count (Context.Lesson.all);
   end Initialize;

   function Is_Defined
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      Check (Context.all, Example);
      return Is_Defined (Feature, Context, Image);
   end Is_Defined;

   function Is_Known
            (  Context : not null access Lecture_Context;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Boolean is
   begin
      Check (Context.all, Example);
      return Is_Known (Feature, Context, Image);
   end Is_Known;

   procedure Select_Example
             (  Context : in out Lecture_Context;
                Example : Positive
             )  is
   begin
      Check (Context, Example);
   end Select_Example;

   procedure Set_Data
             (  Context : in out Lecture_Context;
                Feature : Feature_Object'Class;
                Data    : in out Feature_Data_Ptr
             )  is
      ID : constant Feature_ID := Get_ID (Feature);
   begin
      if (  Context.Data.Vector /= null
         and then
            ID in Context.Data.Vector'Range
         and then
            Context.Data.Vector (ID) /= null
         )
      then
         raise Program_Error;
      else
         Feature_Data_Arrays.Put (Context.Data, ID, Data);
      end if;
   exception
      when others =>
         Free (Data);
         raise;
   end Set_Data;

end Fuzzy.Feature.Context;
