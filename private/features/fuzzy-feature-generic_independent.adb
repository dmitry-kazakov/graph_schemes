--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Feature.Generic_Independent           Luebeck            --
--  Implementation                                 Winter, 2002       --
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

with Fuzzy.Feature.Context;  use Fuzzy.Feature.Context;
with Fuzzy.Lecture;          use Fuzzy.Lecture;

package body Fuzzy.Feature.Generic_Independent is

   function Get
            (  Source     : Set;
               Constraint : Value_Constraint'Class
            )  return Set;
   pragma Inline (Get);

   function Is_Known
            (  Source     : Set;
               Constraint : Value_Constraint'Class
            )  return Boolean;
   pragma Inline (Is_Known);

   function Is_Known (Source : Set) return Boolean;
   pragma Inline (Is_Known);

   procedure Query
             (  Feature : Independent_Feature_Object;
                Context : in out Context_Object'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data'Class
             )  is
      Source : Lecture_Context'Class renames
                  Lecture_Context'Class (Context);
   begin
      case Image is
         when Has_In =>
            Data.Has_In :=
               Get (Source.Lesson, Source.Example, Feature, Image);
         when Has_Out =>
            Data.Has_Out :=
               Get (Source.Lesson, Source.Example, Feature, Image);
         when Has_Not =>
            Data.Has_Not :=
               Get (Source.Lesson, Source.Example, Feature, Image);
         when Has_Not_Out =>
            Data.Has_Not_Out :=
               Get (Source.Lesson, Source.Example, Feature, Image);
      end case;
      Data.Defined (Image) :=
         Is_Defined
         (  Source.Lesson.all,
            Source.Example,
            Feature,
            Image
         );
      Data.Known (Image) := True;
   end Query;

   procedure Cache
             (  Feature : Independent_Feature_Object'Class;
                Context : in out Context_Object'Class;
                Image   : Image_Type;
                Data    : in out Cached_Feature_Data
             )  is
      pragma Inline (Cache);
   begin
      if not Data.Known (Image) then
         Query (Feature, Context, Image, Data);
      end if;
   end Cache;
   
   function Create_Data (Feature : Independent_Feature_Object)
      return Feature_Data_Ptr is
   begin
      return new Cached_Feature_Data (Feature.Cardinality);
   end Create_Data;

   function Get
            (  Source     : Set;
               Constraint : Value_Constraint'Class
            )  return Set is
      Result : Set (Source'Range) := (others => Confidence'First);
   begin
      for Index in Constraint.From..Constraint.To loop
         if Constraint.Allowed (Index) then
            Result (Index) := Source (Index);
         end if;
      end loop;
      return Result;
   end Get;

   function Get
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Set is
      Data : Cached_Feature_Data renames
                Cached_Feature_Data (Get_Data (Context, Feature).all);
   begin
      Cache (Feature, Context.all, Image, Data);
      case Image is
         when Has_In =>
            if Feature_Data (Data).Constraint = null then
               return Data.Has_In;
            else
               return
                  Get
                  (  Data.Has_In,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Out =>
            if Feature_Data (Data).Constraint = null then
               return Data.Has_Out;
            else
               return
                  Get
                  (  Data.Has_Out,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Not =>
            if Feature_Data (Data).Constraint = null then
               return Data.Has_Not;
            else
               return
                  Get
                  (  Data.Has_Not,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Not_Out =>
            if Feature_Data (Data).Constraint = null then
               return Data.Has_Not_Out;
            else
               return
                  Get
                  (  Data.Has_Not_Out,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
      end case;
   end Get;

   function Get
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type;
               Value   : Positive
            )  return Confidence is
      Data : Cached_Feature_Data renames
                Cached_Feature_Data (Get_Data (Context, Feature).all);
      Constraint : constant Value_Constraint_Ptr :=
                      Feature_Data (Data).Constraint;
      Index   : Positive := Value;
      Allowed : Boolean;
   begin
      Cache (Feature, Context.all, Image, Data);
      if Constraint /= null then
         if Constraint.From > Index then
            Index := Constraint.From;
         elsif Constraint.To < Index then
            Index := Constraint.To;
         end if;
         Allowed := Constraint.Allowed (Index);
      else
         Allowed := True;
      end if;
      if Allowed then
         case Image is
            when Has_In =>
               return Data.Has_In (Index);
            when Has_Out =>
               return Data.Has_Out (Index);
            when Has_Not =>
               return Data.Has_Not (Index);
            when Has_Not_Out =>
               return Data.Has_Not_Out (Index);
         end case;
      else
         return Confidence'First;
      end if;
   end Get;

   function Is_Computed
            (  Feature : Independent_Feature_Object;
               Source  : Feature_Object'Class
            )  return Boolean is
   begin
      return Feature.ID = Source.ID;
   end Is_Computed;

   function Is_Defined
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Data : Cached_Feature_Data renames
                Cached_Feature_Data (Get_Data (Context, Feature).all);
   begin
      Cache (Feature, Context.all, Image, Data);
      return Data.Defined (Image);
   end Is_Defined;

   function Is_Known (Source : Set) return Boolean is
   begin
      for Index in Source'Range loop
         if Source (Index) /= Confidence'Last then
            return True;
         end if;
      end loop;
      return False;
   end Is_Known;

   function Is_Known
            (  Source     : Set;
               Constraint : Value_Constraint'Class
            )  return Boolean is
   begin
      for Index in Constraint.From..Constraint.To loop
         if (  Constraint.Allowed (Index)
            or else
               Source (Index) /= Confidence'Last
            )
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Known;

   function Is_Known
            (  Feature : Independent_Feature_Object;
               Context : not null access Context_Object'Class;
               Image   : Image_Type
            )  return Boolean is
      Data : Cached_Feature_Data renames
                Cached_Feature_Data (Get_Data (Context, Feature).all);
   begin
      Cache (Feature, Context.all, Image, Data);
      case Image is
         when Has_In =>
            if Feature_Data (Data).Constraint = null then
               return Is_Known (Data.Has_In);
            else
               return
                  Is_Known
                  (  Data.Has_In,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Out =>
            if Feature_Data (Data).Constraint = null then
               return Is_Known (Data.Has_Out);
            else
               return
                  Is_Known
                  (  Data.Has_Out,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Not =>
            if Feature_Data (Data).Constraint = null then
               return Is_Known (Data.Has_Not);
            else
               return
                  Is_Known
                  (  Data.Has_Not,
                     Feature_Data (Data).Constraint.all
                  );
            end if;
         when Has_Not_Out =>
            if Feature_Data (Data).Constraint = null then
               return Is_Known (Data.Has_Not_Out);
            else
               return
                  Is_Known
                  (  Data.Has_Not_Out,
                     Feature_Data (Data).Constraint.all                     
                  );
            end if;
      end case;
   end Is_Known;

end Fuzzy.Feature.Generic_Independent;
