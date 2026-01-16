--                                                                    --
--  package                         Copyright (c)  Dmitry A. Kazakov  --
--     Fuzzy.Lecture.General.Text_Dialogue         Luebeck            --
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

with Ada.Text_IO;                use Ada.Text_IO;
with Fuzzy.Feature.Edit;         use Fuzzy.Feature.Edit;
with Fuzzy.Feature.Handle.Edit;  use Fuzzy.Feature.Handle.Edit;
with Fuzzy.Intuitionistic;       use Fuzzy.Intuitionistic;

package body Fuzzy.Lecture.General.Text_Dialogue is

   function Create return Lecture_Handle is
      Result : constant Lecture_Object_Ptr :=
                  new Dialogue_Lecture_Object;
   begin
      return Ref (Result);
   end Create;

   procedure Query
             (  Lesson  : in out Dialogue_Lecture_Object;
                Example : Positive;
                Feature : Feature_Object'Class
             )  is
      Input : String (1..1024);
      Last  : Integer;
   begin
      loop
         Put ("Enter a value of " & Get_Name (Feature) & ":");
         Get_Line (Input, Last);
         exit when Last < 1;
         begin
            declare
               Data : Classification renames
                  Value (Input (1..Last), Feature, Input_Defaults);
            begin
               Put
               (  Lesson,
                  Example,
                  Feature,
                  Fuzzy.Feature.Has_In,
                  Data.Possibility
               );
               Put
               (  Lesson,
                  Example,
                  Feature,
                  Fuzzy.Feature.Has_Not,
                  not Data.Necessity
               );
               exit;
            end;
         exception
            when Constraint_Error =>
               Put ("Range error. ");
            when Data_Error =>
               Put ("Syntax error. ");
            when End_Error =>
               exit;
         end;
      end loop;
   end Query;

begin
   Register_Class
   (  Class,
      Fuzzy.Lecture.Memory_Resident.Restore'Access
   );
end Fuzzy.Lecture.General.Text_Dialogue;
