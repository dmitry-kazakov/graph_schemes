--                                                                    --
--  package Fuzzy.Lecture           Copyright (c)  Dmitry A. Kazakov  --
--  Implementation                                 Luebeck            --
--                                                 Spring, 2002       --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.IO_Exceptions;        use Ada.IO_Exceptions;
with Fuzzy.Basic_Edit;         use Fuzzy.Basic_Edit;
with Fuzzy.Edit;               use Fuzzy.Edit;
with Strings_Edit;             use Strings_Edit;
with Strings_Edit.Integers;    use Strings_Edit.Integers;
with Strings_Edit.UTF8.Maps;   use Strings_Edit.UTF8.Maps;

with Name_Tables;

package body Fuzzy.Lecture is
   Number_Of_Lectures : Natural := 0;

   procedure Begin_Bulk_Update (Lesson : in out Lecture_Object) is
   begin
      null;
   end Begin_Bulk_Update;

   procedure Copy
             (  Target : in out Lecture_Object'Class;
                Source : Lecture_Object'Class;
                From   : Positive := 1;
                To     : Positive := Positive'Last;
                Viewer : not null access Indicator_Object'Class :=
                            Negleter'Access
             )  is
      Last : constant Natural :=
                Natural'Min (Get_Examples_Number (Source), To);
      Target_No : Positive := Get_Examples_Number (Target) + 1;
      Features  : Bounded_Array renames Get_Features (Source);
   begin
      Reset (Viewer.all, Last - From + 1);
      for Source_No in From..Last loop
         for Index in Features.First..Features.Last loop
            declare
               Feature : Feature_Object'Class renames
                            Get (Features, Index).all;
            begin
               if Is_Defined (Source, Source_No, Feature, Has_In)
               then
                  Put
                  (  Target,
                     Target_No,
                     Feature,
                     Has_In,
                     Get (Source, Source_No, Feature, Has_In)
                  );
               end if;
               if Is_Defined (Source, Source_No, Feature, Has_Out)
               then
                  Put
                  (  Target,
                     Target_No,
                     Feature,
                     Has_Out,
                     Get (Source, Source_No, Feature, Has_Out)
                  );
               end if;
               if Is_Defined (Source, Source_No, Feature, Has_Not)
               then
                  Put
                  (  Target,
                     Target_No,
                     Feature,
                     Has_Not,
                     Get (Source, Source_No, Feature, Has_Not)
                  );
               end if;
               if Is_Defined (Source, Source_No, Feature, Has_Not_Out)
               then
                  Put
                  (  Target,
                     Target_No,
                     Feature,
                     Has_Not_Out,
                     Get (Source, Source_No, Feature, Has_Not_Out)
                  );
               end if;
            end;
         end loop;
         Check (Viewer.all);
         Target_No := Target_No + 1;
      end loop;
      Done (Viewer.all);
   end Copy;

   procedure End_Bulk_Update (Lesson : in out Lecture_Object) is
   begin
      null;
   end End_Bulk_Update;

   procedure Finalize (Lesson : in out Lecture_Object) is
      This : Lecture_Observer_Ptr := Lesson.Observers;
      Next : Lecture_Observer_Ptr;
   begin
      while This /= null loop
         Next := This.Next;
         This.Prev := null;
         This.Next := null;
         This := Next;
      end loop;
      Object.Archived.Finalize (Deposit (Lesson));
      Number_Of_Lectures := Number_Of_Lectures - 1;
   end Finalize;

   procedure Finalize (Observer : in out Lecture_Observer) is
   begin
      Remove (Observer);
      Object.Finalize (Object.Entity (Observer));
   end Finalize;

   procedure Generic_Notify (Lesson : in out Lecture_Object'Class) is
   begin
      if Lesson.Observers /= null then
         declare
            This : Lecture_Observer_Ptr := Lesson.Observers;
            Next : Lecture_Observer_Ptr;
            Ptr  : Object.Entity_Ptr;
         begin
            Lesson.Observers := null;
            loop
               Next := This.Next;
               Remove (This.all);
               Increment_Count (This.all);
               Ptr := This.all'Unchecked_Access;
               Process (This.all);
               Object.Release (Ptr);
               exit when This = Next;
               This := Next;
            end loop;
         end;
      end if;
   end Generic_Notify;

   procedure Generic_Restore
             (  Source  : String;
                Pointer : in out Integer;
                Class   : String;
                List    : Deposit_Container'Class;
                Lesson  : in out Lecture_Object'Class
             )  is
      Rows    : Natural;
      Columns : Natural;
   begin
      begin
         Get (Source, Pointer, Rows);
      exception
         when Constraint_Error =>
            raise Data_Error;
      end;
      Get (Source, Pointer, Name_Tables.Blanks);
      if (  Pointer > Source'Last
         or else
            To_Lower (Source (Pointer)) /= 'x'
         )
      then
         raise Data_Error;
      else
         Pointer := Pointer + 1;
         Get (Source, Pointer, Name_Tables.Blanks);
      end if;
      Get (Source, Pointer, Columns);
      declare
         Feature : Feature_Object_Ptr;
         Got_It  : Boolean;
      begin
         for Row in 1..Rows loop
            if Row = 1 then
               Get (Source, Pointer, Name_Tables.Blanks);
            else
               Get_Delimiter (Source, Pointer, Got_It, Semicolon);
               if not Got_It then
                  raise Data_Error;
               end if;
            end if;
            for Column in 1..Columns loop
               if Column /= 1 then
                  Get (Source, Pointer, Name_Tables.Blanks);
               end if;
               begin
                  Feature := To_Feature_Object_Ptr (Get (List, Column));
               exception
                 when Constraint_Error =>
                    raise Use_Error;
               end;
               declare
                  Value : Set (1..Feature.Cardinality);
               begin
                  for Image in Image_Type loop
                     Get (Source, Pointer, Value);
                     Put (Lesson, Row, Feature.all, Image, Value);
                     Get_Delimiter (Source, Pointer, Got_It, Semicolon);
                     if not Got_It then
                        raise Data_Error;
                     end if;
                  end loop;
               end;
            end loop;
         end loop;
      end;
   exception
      when Constraint_Error =>
         raise Data_Error;
   end Generic_Restore;

   procedure Generic_Store
             (  Destination : in out String;
                Pointer     : in out Integer;
                Lesson      : Lecture_Object'Class
             )  is
      Rows     : constant Natural := Get_Examples_Number (Lesson);
      Columns  : constant Natural := Get_Features_Number (Lesson);
      Position : Integer := Pointer;
   begin
      Put (Destination, Position, Rows);
      Put (Destination, Position, 'x');
      Put (Destination, Position, Columns);
      for Row in 1..Rows loop
         if Row = 1 then
            Put (Destination, Position, ' ');
         else
            Put (Destination, Position, "; ");
         end if;
         for Column in 1..Columns loop
            declare
               Feature : Feature_Object'Class renames
                         Ptr (Get_Feature (Lesson, Column)).all;
            begin
               if Column /= 1 then
                  Put (Destination, Position, ' ');
               end if;
               for Image in Image_Type loop
                  Put
                  (  Destination,
                     Position,
                     Get (Lesson, Row, Feature, Image)
                  );
                  Put (Destination, Position, "; ");
               end loop;
            end;
         end loop;
      end loop;
      Pointer := Position;
   end Generic_Store;

   function Get
            (  Lesson  : not null access Lecture_Object'Class;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Image_Type
            )  return Set is
   begin
      if not
         (  Is_Defined (Lesson.all, Example, Feature, Has_In)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Out)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Not)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Not_Out)
         )
      then
         Query (Lesson.all, Example, Feature);
      end if;
      return Get (Lesson.all, Example, Feature, Image);
   end Get;

   function Get_Number_Of_Lectures return Natural is
   begin
      return Number_Of_Lectures;
   end Get_Number_Of_Lectures;

   procedure Initialize (Lesson : in out Lecture_Object) is
   begin
      Object.Archived.Initialize (Deposit (Lesson));
      Number_Of_Lectures := Number_Of_Lectures + 1;
   end Initialize;

   procedure Initialize (Observer : in out Lecture_Observer) is
   begin
      Object.Initialize (Object.Entity (Observer));
      Insert (Observer);
   end Initialize;

   function Is_Known
            (  Lesson  : not null access Lecture_Object'Class;
               Example : Positive;
               Feature : Feature_Object'Class;
               Image   : Fuzzy.Feature.Image_Type
            )  return Boolean is
   begin
      if not
         (  Is_Defined (Lesson.all, Example, Feature, Has_In)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Out)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Not)
         or else
            Is_Defined (Lesson.all, Example, Feature, Has_Not_Out)
         )
      then
         Query (Lesson.all, Example, Feature);
      end if;
      return Is_Known (Lesson.all, Example, Feature, Image);
   end Is_Known;

   procedure Insert (Observer : in out Lecture_Observer) is
      Head : Lecture_Observer_Ptr renames Observer.Lesson.Observers;
   begin
      if Observer.Next /= null then
         raise Constraint_Error;
      elsif Head = null then
         Head := Observer'Unchecked_Access;
         Observer.Prev := Observer'Unchecked_Access;
         Observer.Next := Observer'Unchecked_Access;
      else
         Observer.Prev := Head;
         Observer.Next := Head.Next;
         Observer.Next.Prev := Observer'Unchecked_Access;
         Observer.Prev.Next := Observer'Unchecked_Access;
      end if;
   end Insert;

   procedure Notify_Changed
             (  Lesson  : in out Lecture_Object'Class;
                Example : Positive;
                Feature : Feature_Object'Class;
                Image   : Image_Type
             )  is
      procedure Changed (Observer : in out Lecture_Observer'Class) is
      begin
         Changed (Observer, Lesson, Example, Feature, Image);
      end Changed;
      procedure Notify is new Generic_Notify (Changed);
   begin
      Notify (Lesson);
   end Notify_Changed;

   procedure Notify_New
             (  Lesson  : in out Lecture_Object'Class;
                Feature : Feature_Object'Class
             )  is
      procedure Added (Observer : in out Lecture_Observer'Class) is
      begin
         Added (Observer, Lesson, Feature);
      end Added;
      procedure Notify is new Generic_Notify (Added);
   begin
      Notify (Lesson);
   end Notify_New;

   procedure Notify_Renamed
             (  Lesson   : in out Lecture_Object'Class;
                Feature  : Feature_Object'Class;
                Old_Name : String;
                New_Name : String
             )  is
      procedure Renamed (Observer : in out Lecture_Observer'Class) is
      begin
         Renamed (Observer, Lesson, Feature, Old_Name, New_Name);
      end Renamed;
      procedure Notify is new Generic_Notify (Renamed);
   begin
      Notify (Lesson);
   end Notify_Renamed;

   procedure Notify_Undefined
             (  Lesson  : in out Lecture_Object'Class;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
      procedure Deleted (Observer : in out Lecture_Observer'Class) is
      begin
         Deleted (Observer, Lesson, Example, Feature);
      end Deleted;
      procedure Notify is new Generic_Notify (Deleted);
   begin
      Notify (Lesson);
   end Notify_Undefined;

   procedure Notify_Undefined
             (  Lesson  : in out Lecture_Object'Class;
                Feature : Feature_Object'Class
             )  is
      procedure Deleted (Observer : in out Lecture_Observer'Class) is
      begin
         Deleted (Observer, Lesson, Feature);
      end Deleted;
      procedure Notify is new Generic_Notify (Deleted);
   begin
      Notify (Lesson);
   end Notify_Undefined;

   procedure Remove (Observer : in out Lecture_Observer) is
   begin
      if Observer.Next /= null then
         declare
            Head : Lecture_Observer_Ptr renames
                      Observer.Lesson.Observers;
         begin
            if Head = Observer'Unchecked_Access then
               if Observer.Next = Observer'Unchecked_Access then
                  Head := null;
               else
                  Head := Observer.Next;
               end if;
            end if;
            Observer.Next.Prev := Observer.Next;
            Observer.Prev.Next := Observer.Prev;
         end;
         Observer.Next := null;
      end if;
   end Remove;

   procedure Query
             (  Lesson  : in out Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
   begin
      null;
   end Query;

   function To_Lecture_Ptr is
      new Ada.Unchecked_Conversion
          (  Deposit_Ptr,
             Lecture_Object_Ptr
          );

   function To_Lecture_Object_Ptr (Ptr : Deposit_Ptr)
      return Lecture_Object_Ptr is
   begin
      if Ptr.all in Lecture_Object'Class then
         return To_Lecture_Ptr (Ptr);
      else
         raise Constraint_Error;
      end if;
   end To_Lecture_Object_Ptr;

end Fuzzy.Lecture;
